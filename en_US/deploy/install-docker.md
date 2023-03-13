# Deploy with Docker

This chapter will introduce how to use the official Docker image to install and run EMQX, and how to use Docker Compose to build an EMQX cluster.

:::tip Note

1. If you want to keep the data, please mount the EMQX data directory (`/opt/emqx/data`) out of the container, so that the data will persist even if the container no longer exists.

2. In Docker, `localhost` or `127.0.0.1`  points to the internal address of the container. Please use the host’s IP or [host networking](https://docs.docker.com/network/host/) to access the host address. If you are using Docker for Mac or Docker for Windows, you can use `host.docker.internal` as the host address.

   :::

## Use Docker to run a single EMQX node

This section will introduce how to use the Docker image to install the latest version of EMQX. If you want to work with other versions,  please visit the [EMQX Deployment page](https://www.emqx.com/zh/try?product=enterprise).

1. To get the Docker image, run: 

{% emqxce %}

```bash
docker pull emqx:5.0.17
```

{% endemqxce %}

{% emqxee %}

```bash
docker pull emqx/emqx-enterprise:5.0.0
```

{% endemqxee %}

2. To start the Docker container, run:

{% emqxce %}

```bash
docker run -d --name emqx -p 1883:1883 -p 8083:8083 -p 8084:8084 -p 8883:8883 -p 18083:18083  emqx:5.0.17
```

For more information about EMQX official docker image, see [Docker Hub - emqx](https://hub.docker.com/_/emqx). 

{% endemqxce %}

{% emqxee %}

```bash
docker run -d --name emqx -p 1883:1883 -p 8083:8083 -p 8084:8084 -p 8883:8883 -p 18083:18083  emqx/emqx-enterprise:5.0.0
```

For more information about EMQX official docker image, see [Docker Hub - emqx/emqx-enterprise](https://hub.docker.com/r/emqx/emqx-enterprise).

{% endemqxee %}

## Use Docker Compose to build an EMQX cluster

Docker Compose is a tool for defining and running multi-container Docker applications. This section will introduce how to use Docker Compose to create a static EMQX cluster. 

:::tip
Docker Compose is already included in Docker Desktop. If your Docker Compose still needs to be installed, you may refer to [Install Docker Compose](https://docs.docker.com/compose/install/) for detailed operating steps.

:::

1. Create a  `docker-compose.yml` file under any directory with the following content:

{% emqxce %}

```yml
version: '3'

services:
  emqx1:
    image: emqx:5.0.17
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
    image: emqx:5.0.17
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

{% endemqxce %}

{% emqxee %}

```yml
version: '3'

services:
  emqx1:
    image: emqx/emqx-enterprise:5.0.0
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
    image: emqx/emqx-enterprise:5.0.0
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

2. In the command line tool, switch to the directory where  `docker-compose.yml` is stored，and run the following command to start the EMQX cluster:

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

Use an MQTT client to connect EMQX for message publish/subscribe. For more information, see [Publish and Subscribe](../messaging/mqtt-publish-and-subscribe.md). 

- On how to configure EMQX parameters and other features, see [Configuration](../configuration/configuration.md).

- On how to build an EMQX cluster with multiple nodes, see  [Clustering](./cluster/introduction.md).
