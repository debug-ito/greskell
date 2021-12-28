#!/bin/sh

## Run janusgraph docker container for testing.

docker run --rm -p 8182:8182 janusgraph/janusgraph:0.4.0
