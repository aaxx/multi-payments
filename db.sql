drop schema public cascade;
create schema public;
grant all on schema public to postgres;
grant all on schema public to public;


create type currency_code as enum
  ( 'BTC'
  , 'LTC'
  , 'DASH'
  , 'XMR'
  , 'ETH'
  , 'ETC'
  , 'TIME'
  , 'SNM'
  );


create table ico_config
  ( snm_total_limit int8 not null default 444000000
  , snm_ico_limit int8 not null default 331360000
  , snm_per_eth int8 not null default 2424
  , time_address text not null default
    '0x6531f133e6deebe7f2dce5a0441aa7ef330b4e53'
  , time_topic text not null default
    '0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef'
  );
insert into ico_config default values;


create table address
  ( currency currency_code not null
  , addr text unique not null
  );
-- imposed index on addr


create table invoice
  ( ctime timestamptz not null default now()
  , last_activity timestamptz not null default now()
  , currency currency_code not null
  , from_addr text unique not null references address(addr)
  , to_eth_addr text not null
  , constraint invoice_unique_addr unique (to_eth_addr, currency)
  );
-- imposed index on (to_eth_addr, currency)


create table block
  ( ctime timestamptz not null default now()
  , currency currency_code not null
  , height int not null
  , num_of_transactions int not null
  , hash text unique not null
  , deprecated boolean not null default false
  );


create or replace function notify_new_block() returns trigger as $$
  declare
  begin
    perform pg_notify('new_block', new.currency::text);
    return new;
  end;
$$ language plpgsql;

create trigger notify_new_block
  after insert on block for each row
  execute procedure notify_new_block();


create table transaction
  ( ctime timestamptz not null default now()
  , currency currency_code not null
  , value int8 not null
  , block_hash text not null references block(hash)
  , tx_hash text not null
  , to_addr text not null
  );


create table price
  ( id serial primary key
  , ctime timestamptz not null default now()
  , currency currency_code not null
  , price numeric(24,18) not null -- currency units per BTC
  );
create index price_ctime_ix on price(ctime);


create table price_correction
  ( currency currency_code unique not null
  , factor numeric(24,18) not null
  );


create view actual_price as
  with eth as (
    select price from price where currency = 'ETH'
      order by ctime desc limit 1)
  select distinct on (p.currency)
      p.currency,
      coalesce(pc.factor, 1) * ico_config.snm_per_eth * p.price / eth.price
        as snm_per_unit
    from price p
      join ico_config on (true)
      join eth on (true)
      left outer join price_correction pc
        on (p.currency = pc.currency)
    order by p.currency, p.ctime desc;



create table currency_limit
  ( currency currency_code not null
  , ctime timestamptz not null default now()
  , soft_limit int8 not null
  , hard_limit int8 not null
  );


create table token_emission
  ( ctime timestamptz not null default now()
  , currency currency_code not null
  , block_height int not null -- chk enough tx validations
  , price int references price(id)
  , cur_value int8 not null
  , snm_value int8 not null
  , tx_hash text unique not null -- ICO.foreignBuy
  , tx_result json not null default '{}'
  );




create function ico_info() returns json as $$
  with
    limits as (
      select distinct on (currency)
          currency, hard_limit
        from currency_limit
        order by currency, ctime desc),
    raised as (
      select currency, sum(cur_value) as sum
        from token_emission
        group by currency
    ),
    instruments as (
      select json_agg(row_to_json(i.*)) as arr
        from (
          select
              p.currency as name,
              p.snm_per_unit :: text as price,
              hard_limit :: text as "totalAmount",
              coalesce(r.sum, 0) :: text as raised
            from actual_price p
              left outer join limits l on (p.currency = l.currency)
              left outer join raised r on (r.currency = p.currency)
          ) i),
    snm_sold as (
      select sum(value) as value from transaction where currency = 'SNM')
    select json_build_object(
          'snmTokenAmount', ico_config.snm_total_limit,
          'snmTokenSold', coalesce(snm_sold.value, 0) :: text,
          'instruments', instruments.arr)
        from instruments, snm_sold, ico_config
$$ language sql immutable;


create function addr_info(eth_addr text) returns json as $$
  select json_build_object(
    'snmBalance', '0',
    'tx', json_build_object())
$$ language sql immutable;


create function create_invoice(cur currency_code, eth_addr text) returns json as $$
  insert into invoice (currency, from_addr, to_eth_addr)
    select a.currency, a.addr, eth_addr
      from address a left join invoice i on (a.addr = i.from_addr)
      where a.currency = cur
        and (to_eth_addr is null or to_eth_addr = eth_addr)
      order by to_eth_addr nulls last
      limit 1
    on conflict on constraint invoice_unique_addr
      do update set last_activity = now()
    returning json_build_object(
      'ethAddress', to_eth_addr,
      'depositAddress', from_addr,
      'currency', currency)
$$ language sql;
