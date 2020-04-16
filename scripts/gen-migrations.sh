#!/bin/bash

qlot exec mito generate-migrations -t postgres -u postgres -d cl-rest -p docker -P 5432 -s rest-server-example -D ./mito
