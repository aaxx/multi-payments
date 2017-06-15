
create or replace  function fmt_precise_number(x numeric) returns text as $$
  select to_char(x, 'FM9999999999999990D09999999')
$$ language sql immutable;

create or replace  function fmt_number(x numeric) returns text as $$
  select to_char(x, 'FM9999999999999990D099')
$$ language sql immutable;

create or replace function fmt_utc(x timestamptz) returns text as $$
  select to_char(x at time zone 'UTC', 'YYYY-MM-DD HH24:MI:SSZ')
$$ language sql immutable;


create or replace function ico_info() returns json as $$
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
              fmt_number(coalesce(r.sum, 0)) as raised,
              case
                when l.soft_limit is null then true
                else coalesce(r.sum, 0) < l.soft_limit
              end as "depositEnabled"
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


create or replace function addr_info(_addr text) returns json as $$
  with unconfirmed as (
    select
        ctime,
        currency,
        value as cur_value,
        null :: numeric(32,18) as snm_value,
        tx_hash,
        'Waiting for confirmation' :: text as status
      from transaction
      where snm_addr = _addr and not confirmed
  ),
  emissions as (
    select
        t.ctime,
        t.currency,
        t.value as cur_value,
        e.snm_value,
        t.tx_hash,
        case e.status
          when 'transferred' then 'Tokens transferred'
          when 'limit_reached'
            then 'Currency limit reached, '
              || 'please contact SONM team to get refund'
          when 'error' then 'Error occured, please contact SONM team'
          else 'Confirmed, tokens in transit'
        end :: text as status
      from token_emission e
        join transaction t on (e.src_tx = t.id)
      where e.snm_addr = _addr
  ),
  all_txs as (
    select
      currency,
      json_build_object(
        'datetime', fmt_utc(t.ctime),
        'amount',   fmt_precise_number(t.cur_value),
        'snmValue', fmt_precise_number(t.snm_value),
        'status',   t.status,
        'txLink',
          case t.currency
            when 'BTC'  then 'https://blockexplorer.com/tx/'      || t.tx_hash
            when 'LTC'  then 'http://explorer.litecoin.net/tx/'   || t.tx_hash
            when 'DASH' then 'https://bitinfocharts.com/dash/tx/' || t.tx_hash
            when 'XMR'  then 'https://xmrchain.net/tx/'           || t.tx_hash
            when 'ETC'  then 'https://gastracker.io/tx/'          || t.tx_hash
            when 'ETH'  then 'https://etherscan.io/tx/'           || t.tx_hash
            when 'TIME' then 'https://etherscan.io/tx/'           || t.tx_hash
            else t.tx_hash
          end :: text) as tx
      from (select * from unconfirmed union all select * from emissions) t
  )
  select json_build_object(
    'snmBalance',
      (select sum(value) from transaction
        where currency = 'SNM' and snm_addr = _addr),
    'tx', json_build_object(
      'BTC',   (select json_agg(tx) from all_txs where currency = 'BTC'),
      'LTC',   (select json_agg(tx) from all_txs where currency = 'LTC'),
      'DASH',  (select json_agg(tx) from all_txs where currency = 'DASH'),
      'XMR',   (select json_agg(tx) from all_txs where currency = 'XMR'),
      'ETC',   (select json_agg(tx) from all_txs where currency = 'ETC'),
      'ETH',   (select json_agg(tx) from all_txs where currency = 'ETH'),
      'TIME',  (select json_agg(tx) from all_txs where currency = 'ETC')))
$$ language sql immutable;
