#! /bin/bash -e

PSQL="psql -h $RDS_HOSTNAME -U $RDS_USERNAME $RDS_DB_NAME"
ls *sql | sort | xargs cat | ${PSQL}
