#! /bin/bash -e

PSQL="psql -h $RDS_HOSTNAME -U $RDS_USERNAME $RDS_DB_NAME"
ls *sql | sort | xargs cat | ${PSQL}

cat *.csv | sed 's/;[^;]*$//' \
  | ${PSQL} -c "copy address from stdin with delimiter as ';'"
