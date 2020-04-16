#!/bin/bash

mito migrate -t postgres -u postgres -d cl-rest -p docker -P 5432 -D ./mito
