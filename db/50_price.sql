create table price
  ( id serial primary key
  , ctime timestamptz not null default now()
  , currency currency_code not null
  , price numeric(32,18) not null -- BTC per currency units
  );
create index price_ctime_ix on price(ctime);


create table price_correction
  ( currency currency_code unique not null
  , factor numeric(32,18) not null
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
