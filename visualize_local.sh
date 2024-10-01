#! /bin/env bash

SCRIPT_ABS_PATH=$(readlink -f $0)
SCRIPT_ABS_DIR=$(dirname $SCRIPT_ABS_PATH)
# OUTPUT_DIR=out_$(date +%s)
OUTPUT_DIR=out
DATABASE="neo4j"
DATABASE_USER="neo4j"
DATABASE_PASSWORD="12345678"

rm -rf $OUTPUT_DIR
$SCRIPT_ABS_DIR/joern-export --repr=all --format=neo4jcsv --out $OUTPUT_DIR ./cpg.bin

# docker pull neo4j:5.23.0-community-bullseye

# docker run \
#   --publish=7474:7474 --publish=7687:7687 \
#   --name neo4j \
#   neo4j:5.23.0-community-bullseye

# Run with volume
# docker run \
#   --publish=7474:7474 --publish=7687:7687 \
#   --name neo4j \
#   --env NEO4J_AUTH=none \
#   --volume $HOME/neo4j/data:/data \
#   --detach \
#   neo4j:5.23.0-community-bullseye

# Remove old import file
docker exec neo4j bash -c "rm -rf /var/lib/neo4j/import/*"

# Remove old graph
docker exec neo4j bash -c "cypher-shell -u $DATABASE_USER -p $DATABASE_PASSWORD -d $DATABASE \"MATCH (n) DETACH DELETE n\""

# # Create Joern database in Neo4j
# docker exec neo4j bash -c "cypher-shell -u $DATABASE_USER -p $DATABASE_PASSWORD -d $DATABASE 'CREATE DATABASE $DATABASE'"

docker cp $OUTPUT_DIR/. neo4j:/var/lib/neo4j/import

docker exec neo4j bash -c "find /var/lib/neo4j/import/ -name 'nodes_*_cypher.csv' -exec cypher-shell -u $DATABASE_USER -p $DATABASE_PASSWORD -d $DATABASE --file {} \;"
docker exec neo4j bash -c "find /var/lib/neo4j/import/ -name 'edges_*_cypher.csv' -exec cypher-shell -u $DATABASE_USER -p $DATABASE_PASSWORD -d $DATABASE --file {} \;"
