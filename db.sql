

create type currency_code as enum ('BTC', 'LTC', 'DASH');


create table address
  ( currency currency_code not null
  , addr text unique not null
  );


create table invoice
  ( ctime timestamptz not null default now()
  , last_activity timestamptz not null default now()
  , currency currency_code not null
  , from_addr text unique not null references address(addr)
  , to_eth_addr text not null
  , constraint invoice_unique_addr unique (to_eth_addr, currency)
  );


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


-- TODO: grant permissions
