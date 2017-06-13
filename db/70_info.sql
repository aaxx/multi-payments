
create function fmt_precise_number(x numeric) returns text as $$
  select to_char(x, 'FM9999999999999999D99999999')
$$ language sql immutable;

create function fmt_number(x numeric) returns text as $$
  select to_char(x, 'FM9999999999999999D999')
$$ language sql immutable;

create function fmt_utc(x timestamptz) returns text as $$
  select to_char(x at time zone 'UTC', 'YYYY-MM-DD HH24:MI:SSZ')
$$ language sql immutable;


create function ico_info() returns json as $$
  with
    raised as (
      select currency, sum(value) as sum
        from transaction where confirmed group by currency
    ),
    instruments as (
      select json_agg(row_to_json(i.*)) as arr
        from (
          select
              p.currency as name,
              fmt_precise_number(p.snm_per_unit) as price,
              fmt_number(l.hard_limit) as "totalAmount",
              fmt_number(coalesce(r.sum, 0)) as raised
            from actual_price p
              left outer join currency_limit l on (p.currency = l.currency)
              left outer join raised r on (r.currency = p.currency)
          ) i),
    snm_sold as (
      select sum(value) as value from transaction where currency = 'SNM')
    select json_build_object(
          'snmTokenAmount', fmt_number(ico_config.snm_ico_limit),
          'snmTokenSold', fmt_number(coalesce(snm_sold.value, 0)),
          'instruments', instruments.arr)
        from instruments, snm_sold, ico_config
$$ language sql immutable;


create view transactions_by_addr as
  -- completed transactions
  (select
      fmt_utc(t.ctime) as "datatime",
      fmt_precise_number(t.value) as "snmValue",
      coalesce(src1.currency, src2.currency) :: text as currency,
      fmt_precise_number(coalesce(src1.value, src2.value)) as amount,
      coalesce(src1.tx_hash, src2.tx_hash) as "txLink",
      'confirmed' :: text as status,
      t.deposit_addr as "snmAddr"
    from transaction t
      left outer join token_emission e on (t.tx_hash = e.tx_hash)
      left outer join transaction src1 on (e.src_tx = src1.id)
      left outer join transaction src2
        on (t.tx_hash = src2.tx_hash and src2.currency = 'ETH')
    where t.currency = 'SNM')
  union all
  -- not completed transactions
  select
      fmt_utc(t.ctime) as "datatime",
      null as "snmValue",
      t.currency :: text as currency,
      fmt_precise_number(t.value) as amount,
      t.tx_hash as "txLink",
      'not confirmed' as status,
      i.snm_addr as "snmAddr"
    from transaction t
      join invoice i on (i.deposit_addr = t.deposit_addr)
      -- where not confirmed;
;
