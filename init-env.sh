#!/bin/bash -e

# database name
DB=sonm
# private key storage
KEYS=keys


echo '==== Init DB'
dropdb $DB ; createdb $DB
# TODO: create users
psql $DB -f db.sql


echo '==== Generate one-time addresses'
mkdir -p $KEYS && cd $KEYS
../gen-addrs.sh 20 BTC
../gen-addrs.sh 20 LTC
../gen-addrs.sh 20 DASH
cd ..


echo '==== Load addresses into DB'
sed 's/;[^;]*$//' $KEYS/*.csv \
  | psql $DB -c "copy address from stdin with (delimiter ';')"
