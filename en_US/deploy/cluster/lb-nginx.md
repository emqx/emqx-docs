# Load Balance EMQX Cluster with Nginx

Nginx is a high-performance, multifunctional server software that can serve as a web server and reverse proxy server. Additionally, Nginx can function as a load balancer, distributing client requests to multiple backend servers to ensure load balancing and performance optimization. Nginx is particularly well-suited for IoT applications, where handling a large number of concurrent requests is crucial. In IoT, there are typically a large number of devices, which requires a server that is capable of handling a high request load. EMQX natively supports a distributed cluster architecture consisting of multiple MQTT servers. Therefore, deploying Nginx for load balancing and an EMQX cluster ensures high availability and scalability.

This page primarily explains how to install and configure Nginx for reverse proxy and load balancing purposes to set up MQTT servers for an EMQX cluster.

## Features and Benefits

Using Nginx to load balance an EMQX cluster offers several features and advantages:

- As a reverse proxy server, Nginx sits on the MQTT server side, representing MQTT clients to initiate MQTT connection requests to the EMQX cluster and handling requests on behalf of the EMQX cluster. It then returns the EMQX cluster's response to the MQTT clients. This configuration hides multiple clusters and exposes a single access point to MQTT clients. MQTT clients only need to communicate with Nginx and do not need to know about the number and layout of the clusters behind it. This approach enhances system maintainability and scalability.
- Nginx can be used to terminate SSL-encrypted MQTT connections between MQTT clients and the EMQX cluster, reducing the encryption and decryption load on the EMQX cluster. This offers several advantages, such as improved performance, simplified certificate management, and enhanced security.
- Nginx provides flexible load balancing strategies to determine which EMQX node in the cluster should receive requests, helping distribute traffic and requests, thereby improving performance and reliability. For example, sticky load balancing can route requests to the same backend server, enhancing performance and session persistence.

## Prerequisites

Before getting started, ensure that you have created a cluster consisting of the following 3 EMQX nodes. To learn how to create an EMQX cluster, see [Create a Cluster](./create-cluster.md) for details.

| Node Address          | MQTT TCP Port | MQTT WebSocket Port |
| --------------------- | ------------- | ------------------- |
| emqx1-cluster.emqx.io | 1883          | 8083                |
| emqx2-cluster.emqx.io | 1883          | 8083                |
| emqx3-cluster.emqx.io | 1883          | 8083                |

The examples in this page will use a single Nginx server configured as a load balancer to forward requests to the cluster composed of these 3 EMQX nodes.

## Quick Start

Here's a Docker Compose configuration with a real-world example that allows you to easily try and validate the setup. Follow these steps:

1. Clone the example repository and navigate to the `mqtt-lb-nginx` directory:

```bash
git clone https://github.com/emqx/emqx-usage-example
cd emqx-usage-example/mqtt-lb-nginx
```

2. Start the example using Docker Compose:

```bash
docker compose up -d
```

3. Establish 10 TCP connections using [MQTTX](https://mqttx.app/) CLI to simulate MQTT client connections:

```bash
mqttx bench conn -c 10
```

4. You can monitor the Nginx connection status and the distribution of EMQX client connections:

   - Use the following command to view Nginx connection monitoring:

   ```bash
   $ curl http://localhost:8888/status                                
   Active connections: 11 
   server accepts handled requests
    60 60 65 
   Reading: 0 Writing: 1 Waiting: 0
   ```

   This will display the current active connection count and server request processing statistics, including reading, writing, and waiting states.

   - Use the following commands to view the client connection status of each EMQX node:

   ```bash
   docker exec -it emqx1 emqx ctl broker stats | grep connections.count
   docker exec -it emqx2 emqx ctl broker stats | grep connections.count
   docker exec -it emqx3 emqx ctl broker stats | grep connections.count
   ```

   This will display the connection count and active connection count for each node, with 10 connections evenly distributed across the cluster nodes:

   ```bash
   connections.count             : 3
   live_connections.count        : 3
   connections.count             : 4
   live_connections.count        : 4
   connections.count             : 3
   live_connections.count        : 3
   ```

Through these steps, you can verify the Nginx load-balancing functionality in the example and observe the distribution of client connections in the EMQX cluster. You can also customize the configuration for testing by modifying the `emqx-usage-example/mqtt-lb-nginx/nginx.conf` file.

Next, we will start from scratch and explain how to install and configure Nginx to meet various load-balancing requirements.

## Install Nginx

The demonstration installs Nginx on an Ubuntu 22.04 LTS system using source code compilation. You can also install Nginx using Docker or binary packages.

### Prerequisites

Before compiling and installing Nginx, ensure that the following dependencies are installed on your system:

- GNU C and C++ compilers
- PCRE (Perl Compatible Regular Expressions) library
- zlib compression library
- OpenSSL library

You can install these dependencies on an Ubuntu system with the following commands:

```bash
sudo apt-get update
sudo apt-get install build-essential libpcre3-dev zlib1g-dev libssl-dev
```

### Download Source Code

You can download the latest stable version of Nginx from the [Nginx official website](https://nginx.org/en/download.html). For example:

```bash
wget https://nginx.org/download/nginx-1.24.0.tar.gz
```

### Configure and Compile

After downloading, extract the source code and navigate to the source code directory:

```bash
tar -zxvf nginx-1.24.0.tar.gz
cd nginx-1.24.0
```

Configure the compilation options with the following command:

```bash
./configure \
 --with-threads \
 --with-http_stub_status_module \
  --with-http_ssl_module \
  --with-http_realip_module \
  --with-stream \
  --with-stream_ssl_module
```

In the above command, the `--with-http_ssl_module` parameter is used to add SSL support, while the `--with-stream` and `--with-stream_ssl_module` parameters are used to add TCP reverse proxy support.

Start the compilation with the following command:

```bash
make
```

### Install Nginx

After compilation, you can install Nginx with the following command:

```bash
sudo make install
```

Create a symbolic link to the Nginx executable in a directory in your system's PATH:

```bash
sudo ln -s /usr/local/nginx/sbin/nginx /usr/local/bin/nginx
```

## Get Started

Nginx's configuration file is located by default at `/usr/local/nginx/conf/nginx.conf`. Simply add the configuration examples from this page to the end of the file. The basic Nginx operation commands are as follows:

Check the configuration file:

```bash
sudo nginx -t
```

If the Nginx configuration file is validated successfully, you can start Nginx:

```bash
sudo nginx
```

To reload a running Nginx and apply new configurations, it's recommended to check the configuration for errors before performing the operation:

```bash
sudo nginx -s reload
```

To stop Nginx:

```bash
sudo nginx stop
```

## Reverse Proxy MQTT

You can use the following configuration in Nginx's configuration file to reverse proxy MQTT connection requests from clients and forward them to the backend MQTT servers:

```bash
stream {
  upstream mqtt_servers {
    # down: indicates the current server temporarily does not participate in the load balancing
    # max_fails: the number of allowed failed requests; defaults to 1
    # fail_timeout: the timeout for failed requests, defaults to 10s when max_fails is reached
    # backup: when all non-backup servers are down or busy, requests go to backup servers

    server emqx1-cluster.emqx.io:1883 max_fails=2 fail_timeout=10s;
    server emqx2-cluster.emqx.io:1883 down;
    server emqx3-cluster.emqx.io:1883 backup;
  }

  server {
    listen 1883;
    proxy_pass mqtt_servers;

    # When enabling this option, the corresponding backend listener also needs to enable proxy_protocol
    proxy_protocol on;
    proxy_connect_timeout 10s;
    # Default keep-alive time is 10 minutes
    proxy_timeout 1800s;
    proxy_buffer_size 3M;
    tcp_nodelay on;
  }
}
```

## Reverse Proxy MQTT SSL

You can configure Nginx to reverse proxy MQTT and decrypt TLS connections, forwarding encrypted MQTT requests from clients to the backend MQTT servers to ensure communication security. You only need to add SSL-related parameters on top of the TCP-based configuration:

```bash
stream {
  upstream mqtt_servers {
    server emqx1-cluster.emqx.io:1883;
    server emqx2-cluster.emqx.io:1883;
  }

  server {
    listen 8883 ssl;

    ssl_session_cache shared:SSL:10m;
    ssl_session_timeout 10m;
    ssl_certificate /usr/local/nginx/certs/emqx.pem;
    ssl_certificate_key /usr/local/nginx/certs/emqx.key;
    ssl_verify_depth 2;
    ssl_protocols TLSv1 TLSv1.1 TLSv1.2;
    ssl_ciphers HIGH:!aNULL:!MD5;

    # To enable mutual authentication, add the CA certificate and client certificate verification
    # ssl_client_certificate /usr/local/nginx/certs/ca.pem;
    # ssl_verify_client on;
    # ssl_verify_depth 1;

    proxy_pass mqtt_servers;

    # When enabling this option, the corresponding backend listener also needs to enable proxy_protocol
    proxy_protocol on;
    proxy_connect_timeout 10s;
    # Default keep-alive time is 10 minutes
    proxy_timeout 1800s;
    proxy_buffer_size 3M;
    tcp_nodelay on;
  }
}
```

## Reverse Proxy MQTT WebSocket

You can use the following configuration to reverse proxy MQTT WebSocket connections in Nginx, forwarding client requests to the backend MQTT servers. You need to specify an HTTP domain name or IP address using `server_name`:

```bash
http {
  upstream mqtt_websocket_servers {
    server emqx1-cluster.emqx.io:8083;
    server emqx2-cluster.emqx.io:8083;
  }

  server {
    listen 80;
    server_name mqtt.example.com;

    location /mqtt {
      proxy_pass http://mqtt_websocket_servers;

      proxy_http_version 1.1;
      proxy_set_header Upgrade $http_upgrade;
      proxy_set_header Connection "Upgrade";

      # Disable caching
      proxy_buffering off;

      proxy_connect_timeout 10s;
      # WebSocket connection timeout
      # If there's no data exchange within this time, the WebSocket connection will automatically disconnect; default is 60s
      proxy_send_timeout 3600s;
      proxy_read_timeout 3600s;

      # Reverse proxy real IP
      proxy_set_header Host $host;
      proxy_set_header X-Real-IP $remote_addr;
      proxy_set_header REMOTE-HOST $remote_addr;
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    }
  }
}
```

## Reverse Proxy MQTT WebSocket SSL

You can configure Nginx to reverse proxy MQTT WebSocket and decrypt TLS connections, forwarding encrypted MQTT requests from clients to the backend MQTT servers to ensure communication security. Specify an HTTP domain name or IP address using `server_name`. To achieve this, you only need to add SSL and certificate-related parameters on top of the WebSocket-based configuration:

```bash
http {
  upstream mqtt_websocket_servers {
    server emqx1-cluster.emqx.io:8083;
    server emqx2-cluster.emqx.io:8083;
  }

  server {
    listen 443 ssl;
    server_name mqtt.example.com;

    ssl_session_cache shared:SSL:10m;
    ssl_session_timeout 10m;
    ssl_certificate /usr/local/nginx/certs/emqx.pem;
    ssl_certificate_key /usr/local/nginx/certs/emqx.key;
    ssl_protocols TLSv1 TLSv1.1 TLSv1.2;
    ssl_ciphers HIGH:!aNULL:!MD5;

    # To enable mutual authentication, add the CA certificate and client certificate verification
    # ssl_client_certificate /usr/local/nginx/certs/ca.pem;
    # ssl_verify_client on;

    location /mqtt {
        proxy_pass http://mqtt_websocket_servers;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "Upgrade";

        # Reverse proxy real IP
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header REMOTE-HOST $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;

        # Disable caching
        proxy_buffering off;
    }
  }
}
```

## Load-Balancing Strategies

Nginx provides several load-balancing strategies for controlling how connections are distributed. In real-world usage, it's essential to choose the appropriate load-balancing strategy based on your server performance, traffic requirements, and other factors. Here are some common Nginx load-balancing strategies that you can configure in the `upstream` block:

### Round Robin

This is the default load-balancing strategy. It evenly distributes requests to each backend server in a circular manner. This is suitable when backend servers have similar performance.

```bash
upstream backend_servers {
  server emqx1-cluster.emqx.io:1883;
  server emqx2-cluster.emqx.io:1883;
  server emqx3-cluster.emqx.io:1883;
}
```

### Weighted Round Robin

Building on Round Robin, you can assign different weights to each EMQX node. This affects the distribution ratio of requests. Servers with higher weights receive more requests.

```bash
upstream backend_servers {
  server emqx1-cluster.emqx.io:1883 weight=3;
  server emqx2-cluster.emqx.io:1883 weight=2;
  server emqx3-cluster.emqx.io:1883 weight=1;
}
```

### IP Hash

This strategy calculates a hash based on the client's IP address and then assigns the request to a specific backend server. This ensures that requests from the same client are always routed to the same server.

```bash
upstream backend_servers {
  ip_hash;
  server emqx1-cluster.emqx.io:1883;
  server emqx2-cluster.emqx.io:1883;
  server emqx3-cluster.emqx.io:1883;
}
```

### Least Connections

Requests are distributed to the server with the fewest current connections, ensuring that the load on each server is as balanced as possible. This is suitable when there are significant differences in server performance.

```bash
upstream backend_servers {
  least_conn;
  server emqx1-cluster.emqx.io:1883;
  server emqx2-cluster.emqx.io:1883;
  server emqx3-cluster.emqx.io:1883;
}
```

### MQTT Sticky Session

MQTT sticky session load balancing is only available in the Nginx Plus version. The Nginx version compiled and installed in this chapter cannot reference this configuration. For information on optimizing MQTT connections using Nginx Plus, refer to this [document](https://www.nginx.com/blog/optimizing-mqtt-deployments-in-enterprise-environments-nginx-plus/).

"Sticky" refers to the load balancer's ability to route clients back to the same server when they reconnect, preventing session takeover. This is particularly useful for clients that frequently reconnect or problematic clients that disconnect and reconnect, improving efficiency.

To implement stickiness, the server needs to identify the client identifier (usually the client ID) in the connection request. This requires the load balancer to inspect MQTT packets. Once the client identifier is obtained, for static clusters, the server can hash it to a server ID, or the load balancer can maintain a mapping table of client identifiers to destination node IDs for more flexible routing.

```bash
mqtt_preread on;

upstream backend_servers {
    hash $mqtt_preread_clientid consistent;
    server emqx1-cluster.emqx.io:1883;
    server emqx2-cluster.emqx.io:1883;
    server emqx3-cluster.emqx.io:1883;
}
```

Please note that the example configuration above may need adjustments according to your specific setup. The modules used in the configuration (such as `ip_hash` and `least_conn`) are built-in Nginx modules and do not require additional module dependencies.

## Performance Optimization and Monitoring

This section explains how to optimize the performance of Nginx through configuration and enable the status monitoring feature.

### Nginx Basic Configuration Adjustments

- `worker_processes`: The number of worker processes. Set it close to the number of CPU cores on your server, but avoid setting too many processes to prevent resource contention.
- `worker_connections`: The maximum number of simultaneous connections that a single worker process can handle. Ensure it does not exceed the maximum file descriptor limit allowed by the operating system.

```bash
worker_processes auto;

events {
 worker_connections 20480;
}
```

### Nginx Multi-NIC Support for Handling Massive Connections in Reverse Proxy

In reverse proxy scenarios, Nginx establishes connections to backend EMQX nodes as a client. In such cases, a single IP address can create approximately 60,000 long-lived connections at most. To support more connections, you can choose to deploy multiple Nginx servers or configure multiple IP addresses.

Here's an example of configuring multiple IPs using Nginx's built-in `split_clients` module to define a variable `$multi_ip`. It distributes requests based on the client's IP address and port number. Ensure that the IP addresses you use are available locally.

```bash
stream {
 split_clients "$remote_addr$remote_port" $multi_ip {
    20% 10.211.55.5;
    20% 10.211.55.20;
    20% 10.211.55.21;
    20% 10.211.55.22;
    * 10.211.55.23;
  }

  upstream mqtt_servers {
    server emqx1-cluster.emqx.io:1883;
    server emqx2-cluster.emqx.io:1883;
  }

  server {
    listen 1883;

    proxy_pass mqtt_servers;
    proxy_bind $multi_ip;
  }
}
```

### Nginx Status Monitoring

To enable Nginx status monitoring, ensure that the monitoring module `http_stub_status_module` is installed. If it's installed, you can enable status monitoring for Nginx:

```bash
http {
  server {
    listen 8888;

    location /status {
      stub_status on;
      access_log off;
    }
  }
}
```

You can access the status data by opening http://localhost:8888/status:

```bash
$ curl http://localhost:8888/status
Active connections: 12
server accepts handled requests
 25 25 60
Reading: 0 Writing: 1 Waiting: 1
```

## Appendix: Explanation of Main Parameters

Here are explanations of the main parameters used in the example configurations. These parameters ensure stable connections to the backend MQTT servers or ensure that MQTT communication is encrypted and protected via Nginx while following best security practices to safeguard communication privacy and integrity for IoT applications.

| Parameter Name         | Explanation                                                  |
| ---------------------- | ------------------------------------------------------------ |
| proxy_protocol         | Enables the PROXY protocol, allowing Nginx to attach additional proxy information at the beginning of the connection when forwarding requests. This ensures that EMQX can obtain the real client IP. |
| proxy_pass             | Defines the address of the backend MQTT servers, indicating that all requests from clients will be forwarded to this address. |
| proxy_connect_timeout  | Timeout for establishing a connection to the backend MQTT servers. If the connection is not established within this time, Nginx will abort the connection attempt. |
| proxy_timeout          | Timeout for the backend MQTT server. If no response is received from the backend within this time, Nginx will close the connection. |
| proxy_buffer_size      | Size of the buffer used to store data received from the backend MQTT server. Ensures a sufficient buffer size to handle large data streams. |
| tcp_nodelay            | Enables the TCP_NODELAY option and disables the Nagle algorithm. This can reduce packet transmission latency, benefiting real-time MQTT communication. |
| ssl_session_cache      | Configures a shared SSL session cache. This stores the state of SSL sessions to speed up the handshake process when clients reconnect. `shared:SSL:10m` specifies the cache name and size, which is 10MB here. |
| ssl_session_timeout    | Sets the timeout for SSL sessions to 10 minutes. Sessions not reused within this time will be cleared. |
| ssl_certificate        | Specifies the path to the SSL certificate file. This certificate is used to prove the server's identity. |
| ssl_certificate_key    | Specifies the path to the private key file corresponding to the SSL certificate. |
| ssl_protocols          | Specifies the allowed SSL/TLS protocol versions.             |
| ssl_ciphers            | Configures allowed encryption algorithms (cipher suites). `HIGH:!aNULL:!MD5` specifies the use of strong cipher suites while excluding some insecure options, such as empty cipher suites and the MD5 hashing algorithm. |
| ssl_client_certificate | Specifies the path to the Certificate Authority (CA) certificate file used to verify the authenticity of client certificates. |
| ssl_verify_client      | Enables client certificate verification. When set to `on`, Nginx requires clients to provide valid SSL certificates. |
| ssl_verify_depth       | Sets the maximum depth for verifying client certificates. Here, it's set to `1`, indicating that only one level of verification beyond the client certificate and CA certificate is performed. |
