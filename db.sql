

create type currency_code as enum ('BTC', 'LTC', 'DASH');


create table address
  ( currency currency_code not null
  , addr text unique not null
  );

create table invoice
  ( id serial primary key
  , ctime timestamptz not null default now()
  , currency currency_code not null
  , from_addr text unique not null references address(addr)
  , to_eth_addr text not null
  );

--? status: new, unconfirmed, confirmed


-- grant all on transaction to info;
