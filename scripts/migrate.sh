#!/bin/bash
mito migrate -t postgres \
     -u postgres -p docker -P 5432 -d cl-rest \
     -D ./mito
