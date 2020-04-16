#!/bin/bash

mito migration-status -t postgres -u postgres -d cl-rest -p docker -P 5432 -D ./mito
