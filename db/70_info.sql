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


create function addr_info(eth_addr text) returns json as $$
  select json_build_object(
    'snmBalance', '0',
    'tx', json_build_object())
$$ language sql immutable;
