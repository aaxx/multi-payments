create table transaction
  ( id serial primary key
  , ctime timestamptz not null default now()
  , currency currency_code not null
  , value numeric(32,18) not null
  , tx_hash text not null
  , deposit_addr text not null
  , snm_addr text not null -- eth address to receive SNM
  , confirmed bool not null default false
  );


create function transaction_filter() returns trigger as $$
  declare
    _snm_addr text;
  begin
    case new.currency
      when 'TIME' then _snm_addr := lower(new.deposit_addr);
      when 'ETH'  then _snm_addr := lower(new.deposit_addr);
      when 'ETC'  then _snm_addr := lower(new.deposit_addr);
      when 'SNM'  then _snm_addr := lower(new.deposit_addr);
      else select i.snm_addr into _snm_addr
        from invoice i
        where new.deposit_addr = i.deposit_addr
          and new.currency = i.currency;
    end case;

    if _snm_addr is null then
      return null; -- skip this transaction
    end if;

    new.snm_addr := _snm_addr;
    return new;
  end
$$ language plpgsql;


create trigger transaction_filter
  before insert on transaction
  for each row
  execute procedure transaction_filter();
