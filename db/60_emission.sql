

create type token_emission_status as enum
  ( 'fresh', 'transferred', 'limit_reached', 'error' );


create table token_emission
  ( ctime timestamptz not null default now()
  , src_tx int unique not null references transaction(id)
  , eth_value numeric(32,18) not null
  , snm_value numeric(32,18) not null
  , snm_addr text not null
  , tx_hash text -- unique? -- SNM emission transaction hash
  , status token_emission_status not null default 'fresh'
  , error text not null default ''
  );

create index token_emission_snm_ix on token_emission(snm_addr);



create or replace function create_token_emission() returns trigger as $$
  declare
  begin
    insert into token_emission (src_tx, eth_value, snm_value, snm_addr)
      select
          new.id,
          p.eth_per_unit * new.value,
          p.snm_per_unit * new.value,
          new.snm_addr
        from actual_price p
        where p.currency = new.currency
          and new.currency <> 'SNM' -- no emission for SNM and ETH
          and new.currency <> 'ETH';
    return new;
  end;
$$ language plpgsql;


create trigger create_token_emission_after_update
  after update on transaction
  for each row
  when (new.confirmed and new.* is distinct from old.*)
  execute procedure create_token_emission();

create trigger create_token_emission_after_insert
  after insert on transaction
  for each row
  when (new.confirmed)
  execute procedure create_token_emission();
