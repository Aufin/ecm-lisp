#!/bin/sh

REMOTE=maxclaims@212.110.171.229
DUMP_NAME=full-dump-from-`date +%Y-%m-%d.pg`

echo $0
ssh $REMOTE "pg_dump maxclaims -F c >$DUMP_NAME"
scp -C $REMOTE:$DUMP_NAME ../database/dumps/