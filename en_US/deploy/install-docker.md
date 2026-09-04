---
description: This page introduces how to use the official Docker image to install and run EMQX, and how to use Docker Compose to build an EMQX cluster.
---

# Install EMQX Using Docker
This page introduces how to use the official Docker image to install and run the EMQX Enterprise, and how to use Docker Compose to build an EMQX cluster.

## Before You Start

Before you start EMQX in Docker, review the following deployment considerations.

### Choose a Stable Node Name

EMQX stores node data in the `data/mnesia/<node_name>` directory. Choose a stable node name before you start the container to avoid data loss caused by later node name changes.

For a single-node deployment, set the `EMQX_NODE_NAME` environment variable in the format `emqx@<host>`. Set the container hostname to the same `<host>` value.

**Note:** The `<host>` part must be an IP address or a fully qualified domain name (FQDN), such as `node1.emqx.com`. EMQX runs its Erlang node in long-name mode, so you cannot use a short hostname without dots, such as `node1`.

### Prepare Persistent Storage

To preserve EMQX data after a container is removed, mount the following container directories on the host:

- `/opt/emqx/data`: Stores EMQX data.
- `/opt/emqx/log`: Stores file logs and crash dumps.

EMQX containers use console logging by default, but the Erlang VM writes a crash dump to `/opt/emqx/log` when a node terminates abnormally. Without the mount, the dump is lost when the container is removed. The host log directory must be writable by the `emqx` user in the container (UID 1000). For details, see [Crash Dumps in Docker](../configuration/logs.md#crash-dumps-in-docker).

For more information about EMQX directories, see [EMQX Files and Directories](./install.md#files-and-directories).

### Access Services on the Host

If EMQX needs to access a service running on the host, do not use `localhost` or `127.0.0.1` as the service address. These addresses refer to the container's own network interface. Use the host IP address or [host networking](https://docs.docker.com/network/host/). On Docker Desktop for Mac or Windows, you can also use `host.docker.internal`.

## Use Docker to Run A Single EMQX Node

Follow these steps to run a single EMQX node. For more information about the official EMQX Docker image, see [Docker Hub - emqx/emqx-enterprise](https://hub.docker.com/r/emqx/emqx-enterprise).

1. Pull the Docker image:

   ```bash
   docker pull emqx/emqx-enterprise:@EE_VERSION@
   ```

2. Create the host directories and make the log directory writable by the `emqx` user in the container:

   ```bash
   mkdir -p $PWD/data $PWD/log
   sudo chown $UID:$GID $PWD/log
   ```

3. Start the container with the stable node name and mounted directories:

   ```bash
   docker run -d --name emqx-enterprise \
     --hostname node1.emqx.com \
     -e "EMQX_NODE_NAME=emqx@node1.emqx.com" \
     -p 1883:1883 -p 8083:8083 \
     -p 8084:8084 -p 8883:8883 \
     -p 18083:18083 \
     -v $PWD/data:/opt/emqx/data \
     -v $PWD/log:/opt/emqx/log \
     emqx/emqx-enterprise:@EE_VERSION@
   ```

### Listener Addresses in Docker

Starting from EMQX 6.3.0, the official image's entrypoint sets `EMQX_NODE__DEFAULT_LISTENER_ADDRESS=all` when the variable is unset or empty. This selects all network interfaces for MQTT listeners, gateway listeners, and the Dashboard HTTP listener whose binds specify only a port, so they can be reached through published container ports under either security profile. Explicit IP addresses in listener binds remain unchanged. This setting only controls the bind address; it does not relax authentication or authorization requirements.

To override this default, pass another supported value with `docker run -e EMQX_NODE__DEFAULT_LISTENER_ADDRESS=<value>`, or set the variable in the service's `environment` section in Docker Compose. Environment variables take precedence over configuration files, so setting `node.default_listener_address` only in a mounted `emqx.conf` does not override the entrypoint's default. See [Default Listener Address](../access-control/security-profile.md#default-listener-address) for supported values.

With Docker bridge networking, setting this variable to `loopback` binds the affected listeners to loopback inside the container's network namespace. They cannot then be reached through published ports, even if you use `-p`. To control which host addresses are used for published ports, see [Docker port publishing and mapping](https://docs.docker.com/engine/network/port-publishing/).

### Start EMQX with Feature Gates

Starting from EMQX 6.3.0, you can use the `EMQX_FEATURES` environment variable to control which optional features are available at startup. For example, to start EMQX with only core applications, run:

```bash
docker run -d --name emqx-enterprise \
  -e "EMQX_FEATURES=ESSENTIAL" \
  -p 1883:1883 -p 8083:8083 \
  -p 8084:8084 -p 8883:8883 \
  emqx/emqx-enterprise:@EE_VERSION@
```

To start EMQX with a custom feature set, run:

```bash
docker run -d --name emqx-enterprise \
  -e "EMQX_FEATURES=dashboard,metrics,plugins" \
  -p 1883:1883 -p 18083:18083 \
  emqx/emqx-enterprise:@EE_VERSION@
```

For the full feature list and dependency behavior, see [Feature Gates](./feature-gates.md).

## Use Docker Compose to Build an EMQX Cluster

Docker Compose is a tool for defining and running multi-container Docker applications. This section introduces how to use Docker Compose to create a static EMQX cluster.

The Docker Compose example in this section is intended only for local testing, and its volume mounts are commented out. To preserve data and crash dumps, prepare the host directories as described in [Before You Start](#before-you-start) and uncomment the `volumes` entries. For production cluster deployment, see [Clustering](./cluster/introduction.md).

:::tip

Docker Compose is already included in Docker Desktop. If your Docker Compose still needs to be installed, you may refer to [Install Docker Compose](https://docs.docker.com/compose/install/) for detailed operating steps.

:::

1. Create a  `docker-compose.yml` file under any directory with the following content:

   ```yml
   version: '3'
   
   services:
     emqx1:
       image: emqx/emqx-enterprise:@EE_VERSION@
       container_name: emqx1
       environment:
       - "EMQX_NODE_NAME=emqx@node1.emqx.com"
       # - "EMQX_FEATURES=dashboard,metrics,plugins"
       - "EMQX_CLUSTER__DISCOVERY_STRATEGY=static"
       - "EMQX_CLUSTER__STATIC__SEEDS=[emqx@node1.emqx.com,emqx@node2.emqx.com]"
       healthcheck:
         test: ["CMD", "/opt/emqx/bin/emqx", "ctl", "status"]
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
       #   - $PWD/emqx1_log:/opt/emqx/log

     emqx2:
       image: emqx/emqx-enterprise:@EE_VERSION@
       container_name: emqx2
       environment:
       - "EMQX_NODE_NAME=emqx@node2.emqx.com"
       # - "EMQX_FEATURES=dashboard,metrics,plugins"
       - "EMQX_CLUSTER__DISCOVERY_STRATEGY=static"
       - "EMQX_CLUSTER__STATIC__SEEDS=[emqx@node1.emqx.com,emqx@node2.emqx.com]"
       healthcheck:
         test: ["CMD", "/opt/emqx/bin/emqx", "ctl", "status"]
         interval: 5s
         timeout: 25s
         retries: 5
       networks:
         emqx-bridge:
           aliases:
           - node2.emqx.com
       # volumes:
       #   - $PWD/emqx2_data:/opt/emqx/data
       #   - $PWD/emqx2_log:/opt/emqx/log

   networks:
     emqx-bridge:
       driver: bridge
   ```

   If you set `EMQX_FEATURES` in a Docker Compose cluster, use the same value for all EMQX services.

2. In the command line tool, switch to the directory where  `docker-compose.yml` is stored, and run the following command to start the EMQX cluster:

   ```bash
   docker-compose up -d
   ```

3. To check the cluster status, run:

   ```bash
   $ docker exec -it emqx1 sh -c "emqx ctl cluster status"
   Cluster status: #{running_nodes => ['emqx@node1.emqx.com','emqx@node2.emqx.com'],
                     stopped_nodes => []}
   ```

## Next Steps

Use an MQTT client to connect EMQX for message publish/subscribe. For more information, see [Publish and Subscribe](../messaging/publish-and-subscribe.md). 

- On how to configure EMQX parameters and other features, see [Configuration](../configuration/configuration.md).

- On how to build an EMQX cluster with multiple nodes, see  [Clustering](./cluster/introduction.md).
