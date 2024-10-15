#! /bin/env bash

docker run \
  --publish=7474:7474 --publish=7687:7687 \
  --name neo4j \
  neo4j:5.23.0-community-bullseye

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
