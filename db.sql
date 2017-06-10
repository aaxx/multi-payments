

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
