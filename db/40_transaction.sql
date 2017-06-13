create table transaction
  ( id serial primary key
  , ctime timestamptz not null default now()
  , currency currency_code not null
  , value numeric(32,18) not null
  , tx_hash text not null
  , deposit_addr text not null
  , confirmed bool not null default false
  );
