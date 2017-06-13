create table currency_limit
  ( currency currency_code unique not null
  , soft_limit int8 not null
  , hard_limit int8 not null
  );


create table token_emission
  ( ctime timestamptz not null default now()
  , currency currency_code not null
  , price int references price(id)
  , cur_value int8 not null
  , snm_value int8 not null
  , tx_hash text unique not null -- ICO.foreignBuy
  , tx_result json not null default '{}'
  );
