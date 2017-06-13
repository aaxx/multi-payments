#! /bin/bash

ls *sql | sort | xargs cat | psql -h $PG_HOST -U $PG_USER $PG_DATABASE
