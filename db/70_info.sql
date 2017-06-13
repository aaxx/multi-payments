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
              p.snm_per_unit :: text as price,
              l.hard_limit :: text as "totalAmount",
              coalesce(r.sum, 0) :: text as raised
            from actual_price p
              left outer join currency_limit l on (p.currency = l.currency)
              left outer join raised r on (r.currency = p.currency)
          ) i),
    snm_sold as (
      select sum(value) as value from transaction where currency = 'SNM')
    select json_build_object(
          'snmTokenAmount', ico_config.snm_total_limit,
          'snmTokenSold', coalesce(snm_sold.value, 0) :: text,
          'instruments', instruments.arr)
        from instruments, snm_sold, ico_config
$$ language sql immutable;


create view transactions_by_addr as
  -- completed transactions
  (select
      t.ctime as "datatime",
      t.value as "snmValue",
      coalesce(src1.currency, src2.currency) as currency,
      coalesce(src1.value, src2.value) as amount,
      'confirmed' as status,
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
      t.ctime as "datatime",
      null as "snmValue",
      t.currency as currency,
      t.value as amount,
      'not confirmed' as status,
      i.snm_addr as "snmAddr"
    from transaction t
      join invoice i on (i.deposit_addr = t.deposit_addr)
      -- where not confirmed;
;




create function addr_info(eth_addr text) returns json as $$
  select json_build_object(
    'snmBalance', '0',
    'tx', json_build_object(
      'BTC', ''
      'LTC', ''
      'DASH', ''
      'XMR', ''
      'ETH', ''
      'ETC', ''
      'TIME', ''
    ))
$$ language sql immutable;
