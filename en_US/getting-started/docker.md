# Deploy with Docker

This chapter introduces how to use the official Docker image to install and run EMQX, and how to use Docker Compose to build an EMQX cluster.

:::tip

When starting EMQX in a container, use the hostname or FQDN to form the node name. This prevents data loss by ensuring data is stored in the correct directory ('data/mnesia/<node name>').
:::

## Run a Single EMQX Node with Docker

1. Get a docker image from [Docker Hub](https://hub.docker.com/r/emqx/emqx-ee)

   ```bash
   $ docker pull emqx/emqx-ee:4.3.19
   ```

2. Start docker container

   ```bash
   docker run -d --name emqx-ee -p 1883:1883 -p 8081:8081 -p 8083:8083 -p 8084:8084 -p 8883:8883 -p 18083:18083 emqx/emqx-ee:4.3.19
   ```

## Build an EMQX Cluster with Docker Compose

1. Create `docker-compose.yaml` file

   ```yaml
   version: '3'
   
   services:
     emqx1:
       image: emqx/emqx-ee:4.3.19
       environment:
       - "EMQX_NAME=emqx"
       - "EMQX_HOST=node1.emqx.io"
       - "EMQX_CLUSTER__DISCOVERY=static"
       - "EMQX_CLUSTER__STATIC__SEEDS=emqx@node1.emqx.io, emqx@node2.emqx.io"
       volumes:
           - ./tmp/emqx.lic:/opt/emqx/etc/emqx.lic
       healthcheck:
         test: ["CMD", "/opt/emqx/bin/emqx_ctl", "status"]
         interval: 5s
         timeout: 25s
         retries: 5
       networks:
         emqx-bridge:
           aliases:
           - node1.emqx.io
   
     emqx2:
       image: emqx/emqx-ee:4.3.19
       environment:
       - "EMQX_NAME=emqx"
       - "EMQX_HOST=node2.emqx.io"
       - "EMQX_CLUSTER__DISCOVERY=static"
       - "EMQX_CLUSTER__STATIC__SEEDS=emqx@node1.emqx.io, emqx@node2.emqx.io"
       volumes:
           - ./tmp/emqx.lic:/opt/emqx/etc/emqx.lic
       healthcheck:
         test: ["CMD", "/opt/emqx/bin/emqx_ctl", "status"]
         interval: 5s
         timeout: 25s
         retries: 5
       networks:
         emqx-bridge:
           aliases:
           - node2.emqx.io
   
   networks:
     emqx-bridge:
       driver: bridge
   
   ```

2. Start docker-compose cluster

   ```bash
   $ docker-compose -p my_emqx up -d
   ```

3. View cluster

   ```bash
   $ docker exec -it my_emqx_emqx1_1 sh -c "emqx_ctl cluster status"
   Cluster status: #{running_nodes => ['emqx@node1.emqx.io','emqx@node2.emqx.io'],
                     stopped_nodes => []}
   ```

For more information about EMQX Broker Docker, please visit [Docker Hub](https://hub.docker.com/r/emqx/emqx-ee

## Next Step

Use an MQTT client to connect EMQX for message publish/subscribe. For more information, see [Publish and Subscribe](../development/protocol.md).

- On how to configure EMQX parameters and other features, see [Configuration](../configuration/configuration.md).
- On how to build an EMQX cluster with multiple nodes, see [Clustering](../advanced/cluster.md).