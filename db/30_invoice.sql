create table address
  ( currency currency_code not null
  , addr text unique not null
  );
-- imposed index on addr


create table invoice
  ( ctime timestamptz not null default now()
  , last_activity timestamptz not null default now()
  , currency currency_code not null
  , deposit_addr text unique not null references address(addr)
  , snm_addr text not null
  , constraint invoice_unique_addr unique (snm_addr, currency)
  );
-- imposed index on (snm_addr, currency)


create function create_invoice(cur currency_code, eth_addr text) returns json as $$
  insert into invoice (currency, deposit_addr, snm_addr)
    select a.currency, a.addr, eth_addr
      from address a left join invoice i on (a.addr = i.deposit_addr)
      where a.currency = cur
        and (snm_addr is null or snm_addr = eth_addr)
      order by snm_addr nulls last
      limit 1
    on conflict on constraint invoice_unique_addr
      do update set last_activity = now()
    returning json_build_object(
      'ethAddress', snm_addr,
      'depositAddress', deposit_addr,
      'currency', currency)
$$ language sql;
