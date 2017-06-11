

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
  , price numeric(12,9) not null
  );
create index price_ctime_ix on price(ctime);


create table price_correction
  ( id serial primary key
  , ctime timestamptz not null default now()
  , currency currency_code not null
  , factor numeric(12,9) not null
  );


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


create table ico_config
  ( snm_total_limit int8 not null default 444000000
  , snm_ico_limit int8 not null default 331360000
  , snm_per_eth int8 not null default 2424
  );
insert into ico_config default values;


create function ico_info() returns json as $$
  with
    prices as (
      select distinct on (currency)
          currency, price
        from price
        order by currency, ctime desc),
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
              p.currency as "name",
              price :: text,
              hard_limit :: text as "totalAmount",
              coalesce(r.sum, 0) :: text as raised
            from prices p
              left outer join limits l on (p.currency = l.currency)
              left outer join raised r on (r.currency = p.currency)
          ) i),
    snm_sold as (
      select sum(value) as value from transaction where currency = 'SNM'),
    result as (
      select
          ico_config.snm_total_limit as "snmTokenAmount",
          coalesce(snm_sold.value, 0) :: text as "snmTokenSold",
          instruments.arr as instruments
        from instruments, snm_sold, ico_config)
    select row_to_json(r.*) from result r;
$$ language sql immutable;
