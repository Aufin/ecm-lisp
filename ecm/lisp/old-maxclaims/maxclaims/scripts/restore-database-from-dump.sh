#!/bin/sh

# Assumes local pgpass for authentication (obviously only use with
# the dev database on a secure system)

export PGHOST=localhost
export PGUSER=maxclaims
export PGPASSFILE=./pgpass

DBMASTER="maxclaims_new_pristine"
DBCLONES="maxclaims_new"

dropdb -w -e $DBMASTER
createdb -w -e $DBMASTER
echo "Restoring Schema to $DBMASTER"
pg_restore -w -s -d $DBMASTER $1
# restore the data separately with triggers disabled otherwise it
# takes ~2 hours to restore (!)
echo "Restoring Data to $DBMASTER"
pg_restore -w --data-only --disable-triggers -d $DBMASTER $1

for d in $DBCLONES; do
    dropdb -w -e $d
    createdb -w -e $d -T $DBMASTER
done

echo "done"