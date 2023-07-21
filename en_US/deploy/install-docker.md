# Deploy with Docker

This chapter introduces how to use the official Docker image to install and run EMQX, and how to use Docker Compose to build an EMQX cluster.

:::tip Note

1. If you want to persist data generated in the EMQX Docker container, you need to keep the following directories, so that the data will persist even if the container no longer exists.

```bash
/opt/emqx/data
/opt/emqx/etc
/opt/emqx/log
```

2. Due to EMQX using `data/mnesia/<node_name>` as the data storage directory, please use fixed information such as hostname or FQDN as the node name to avoid data loss caused by node name changes.

:::

## Use Docker to Run A Single EMQX Node

This section will introduce how to use the Docker image to install the latest version of EMQX. If you want to work with other versions,  please visit the [EMQX Deployment page](https://www.emqx.com/en/try?product=enterprise).

1. To get the Docker image, run: 

{% emqxce %}

```bash
docker pull emqx/emqx:@CE_VERSION@
```

{% endemqxce %}

{% emqxee %}

```bash
docker pull emqx/emqx-enterprise:@EE_VERSION@
```

{% endemqxee %}

2. To start the Docker container, run:

{% emqxce %}

```bash
docker run -d --name emqx -p 1883:1883 -p 8083:8083 -p 8084:8084 -p 8883:8883 -p 18083:18083  emqx:@CE_VERSION@
```

For more information about EMQX official docker image, see [Docker Hub - emqx](https://hub.docker.com/_/emqx). 

{% endemqxce %}

{% emqxee %}

```bash
docker run -d --name emqx -p 1883:1883 -p 8083:8083 -p 8084:8084 -p 8883:8883 -p 18083:18083  emqx/emqx-enterprise:@EE_VERSION@
```

For more information about EMQX official docker image, see [Docker Hub - emqx/emqx-enterprise](https://hub.docker.com/r/emqx/emqx-enterprise).

{% endemqxee %}

## Use Docker Compose to Build an EMQX Cluster

Docker Compose is a tool for defining and running multi-container Docker applications. This section introduces how to use Docker Compose to create a static EMQX cluster.

Please note that the Docker Compose example file in this section is only applicable to local testing. If you need to deploy a cluster in a production environment, please refer to [Clustering](./cluster/introduction.md).

:::tip

Docker Compose is already included in Docker Desktop. If your Docker Compose still needs to be installed, you may refer to [Install Docker Compose](https://docs.docker.com/compose/install/) for detailed operating steps.

:::

1. Create a  `docker-compose.yml` file under any directory with the following content:

{% emqxce %}

```yml
version: '3'

services:
  emqx1:
    image: emqx:@CE_VERSION@
    container_name: emqx1
    environment:
    - "EMQX_NODE_NAME=emqx@node1.emqx.io"
    - "EMQX_CLUSTER__DISCOVERY_STRATEGY=static"
    - "EMQX_CLUSTER__STATIC__SEEDS=[emqx@node1.emqx.io,emqx@node2.emqx.io]"
    healthcheck:
      test: ["CMD", "/opt/emqx/bin/emqx_ctl", "status"]
      interval: 5s
      timeout: 25s
      retries: 5
    networks:
      emqx-bridge:
        aliases:
        - node1.emqx.io
    ports:
      - 1883:1883
      - 8083:8083
      - 8084:8084
      - 8883:8883
      - 18083:18083 
    # volumes:
    #   - $PWD/emqx1_data:/opt/emqx/data

  emqx2:
    image: emqx:@CE_VERSION@
    container_name: emqx2
    environment:
    - "EMQX_NODE_NAME=emqx@node2.emqx.io"
    - "EMQX_CLUSTER__DISCOVERY_STRATEGY=static"
    - "EMQX_CLUSTER__STATIC__SEEDS=[emqx@node1.emqx.io,emqx@node2.emqx.io]"
    healthcheck:
      test: ["CMD", "/opt/emqx/bin/emqx_ctl", "status"]
      interval: 5s
      timeout: 25s
      retries: 5
    networks:
      emqx-bridge:
        aliases:
        - node2.emqx.io
    # volumes:
    #   - $PWD/emqx2_data:/opt/emqx/data

networks:
  emqx-bridge:
    driver: bridge
```

{% endemqxce %}

{% emqxee %}

```yml
version: '3'

services:
  emqx1:
    image: emqx/emqx-enterprise:@EE_VERSION@
    container_name: emqx1
    environment:
    - "EMQX_NODE_NAME=emqx@node1.emqx.com"
    - "EMQX_CLUSTER__DISCOVERY_STRATEGY=static"
    - "EMQX_CLUSTER__STATIC__SEEDS=[emqx@node1.emqx.com,emqx@node2.emqx.com]"
    healthcheck:
      test: ["CMD", "/opt/emqx/bin/emqx_ctl", "status"]
      interval: 5s
      timeout: 25s
      retries: 5
    networks:
      emqx-bridge:
        aliases:
        - node1.emqx.com
    ports:
      - 1883:1883
      - 8083:8083
      - 8084:8084
      - 8883:8883
      - 18083:18083
    # volumes:
    #   - $PWD/emqx1_data:/opt/emqx/data

  emqx2:
    image: emqx/emqx-enterprise:@EE_VERSION@
    container_name: emqx2
    environment:
    - "EMQX_NODE_NAME=emqx@node2.emqx.com"
    - "EMQX_CLUSTER__DISCOVERY_STRATEGY=static"
    - "EMQX_CLUSTER__STATIC__SEEDS=[emqx@node1.emqx.com,emqx@node2.emqx.com]"
    healthcheck:
      test: ["CMD", "/opt/emqx/bin/emqx_ctl", "status"]
      interval: 5s
      timeout: 25s
      retries: 5
    networks:
      emqx-bridge:
        aliases:
        - node2.emqx.com
    # volumes:
    #   - $PWD/emqx2_data:/opt/emqx/data

networks:
  emqx-bridge:
    driver: bridge
```

{% endemqxee %}

2. In the command line tool, switch to the directory where  `docker-compose.yml` is stored, and run the following command to start the EMQX cluster:

```bash
docker-compose up -d
```

3. To check the cluster status, run:

```bash
$ docker exec -it emqx1 sh -c "emqx_ctl cluster status"
Cluster status: #{running_nodes => ['emqx@node1.emqx.com','emqx@node2.emqx.com'],
                  stopped_nodes => []}
```

## Next

Use an MQTT client to connect EMQX for message publish/subscribe. For more information, see [Publish and Subscribe](../messaging/publish-and-subscribe.md). 

- On how to configure EMQX parameters and other features, see [Configuration](../configuration/configuration.md).

- On how to build an EMQX cluster with multiple nodes, see  [Clustering](./cluster/introduction.md).
