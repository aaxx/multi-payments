#!/bin/bash


if [[ $# -lt 2 ]] ; then
  echo "Usage: $0 <count> <currency>"
  echo 'Generates <count> addresses for <currency>.'
  echo 'Result is written to <currency>.csv file.'
  echo 'Supported currencies are:'
  echo '    - BTC'
  echo '    - LTC'
  echo '    - DASH'
  exit 1
fi

COUNT=$1
CURRENCY=$2

case $CURRENCY in
  BTC)
    PREFIX="-i 1SNM" ;;
  LTC)
    PREFIX=LSNM ;;
  DASH)
    PREFIX="-i XSNM" ;;
  *) echo Unexpected currency code $CURRENCY
    exit 1
esac

TMP=$CURRENCY.tmp
touch $TMP

vanitygen -C $CURRENCY -k -o $TMP -k -q $PREFIX &
genproc=$!

function ctrl_c {
  stop_now=1
  echo
  echo Terminating ...
  kill $genproc
}

trap ctrl_c SIGINT

while true ; do
  sleep 3
  [[ $stop_now -eq 1 ]] && break
  [[ `wc -l $TMP | cut -d ' ' -f 1` -ge $(($COUNT * 3)) ]] && break
done

echo
kill $genproc

sed \
  -e '/Pattern/d' \
  -e 'N;s/\n/ /' \
  -e 's/ Address: /;/' \
  -e 's/ \S* Privkey: /;/' \
  $TMP > $CURRENCY.csv
