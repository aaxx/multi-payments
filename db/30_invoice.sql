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
