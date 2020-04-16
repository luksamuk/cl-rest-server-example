#!/bin/bash
qlot exec mito generate-migrations -t postgres \
     -u postgres -p docker -P 5432 -d cl-rest \
     -s rest-server-example -D ./mito
