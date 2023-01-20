# Cluster load balancing

Load Balancer (LB) will balance the load among multiple network components, optimize resource usage, and avoid system malfunctions caused by overload.-

Though not a mandatory component, it could bring some obvious system benefits, for example, when an LB is configured in EMQX:

- Balance the load of EMQX to avoid single node overload;
- Simplify client configuration, the client only needs to connect to the LB and need not worry about the scaling within the cluster;
- TLS/SSL termination, to reduce the load of EMQX clusters;
- Improve cluster security, with LB configured at the front end of the cluster, unwanted traffic can be blocked to protect the EMQX cluster from malicious attacks. 

This chapter will introduce the LB configuration in EMQX. 

## Deployment architecture

For an EMQX cluster configured with LB, the LB will handle the incoming TCP traffic and then distribute the received MQTT connection requests and messages to different EMQX nodes. The typical deployment architecture is as follows:

![image](./assets/lb_1.png)

It is recommended to terminate the SSL/TLS connection at LB, that is, we will use SSL/TLS to secure connection between clients and LB and TCP connection between LB and EMQX nodes, thus maximizing the performance of the EMQX cluster. The architecture is as follows:

![TLS termination](./assets/lb_2.png)

Besides this load balancing mode, you can also use DNS polling to connect to the EMQX cluster directly, that is, all nodes are added to the DNS polling list, and the clients access the EMQX cluster via domain name or IP address list. However, this DNS polling mode is not recommended in the production environment. 

## Select an LB

Many load balancing products are currently available, including open-source and commercial editions, and public cloud providers also have their load balancing services.

LB products for public cloud:

| Cloud provider                            | SSL Termination | LB Product                                                  |
| ----------------------------------------- | --------------- | ----------------------------------------------------------- |
| [AWS](https://aws.amazon.com)             | Yes             | <https://aws.amazon.com/elasticloadbalancing/?nc1=h_ls>     |
| [Azure](https://azure.microsoft.com)      | Unknown         | <https://azure.microsoft.com/en-us/products/load-balancer/> |
| [Google Cloud](https://cloud.google.com/) | Yes             | <https://cloud.google.com/load-balancing>                   |

 LB products for private cloud:

| Open-Source LB                     | SSL Termination | DOC/URL                                                 |
| ---------------------------------- | --------------- | ------------------------------------------------------- |
| [HAProxy](https://www.haproxy.org) | Yes             | <https://www.haproxy.com/solutions/load-balancing.html> |
| [NGINX](https://www.nginx.com)     | Yes             | <https://www.nginx.com/solutions/load-balancing/>       |

In the following section, we will take the HAProxy or NGINX for private deployment as an example to illustrate how to configure an LB in the EMQX cluster. 

## Configure HAProxy/NGINX in EMQX

Suppose we have two nodes in one EMQX cluster:

| Node  | IP          |
| ----- | ----------- |
| emqx1 | 192.168.0.2 |
| emqx2 | 192.168.0.3 |

You can follow the step below to configure the HAProxy or NGINX LB. 

### Enable Proxy Protocol

Suppose the EMQX cluster is deployed behind HAProxy or Nginx. In that case, we will need the true source IP and the port number of the client, then enable `proxy_protocol` in the configuration file of the corresponding listeners, taking the listener on port 1883 as an example:

```
listeners.tcp.default {
  bind = "0.0.0.0:1883"
  max_connections = 1024000

  proxy_protocol = true
}
```

For proxy protocols and that used in Nginx, see: 

- Proxy protocol: https://www.haproxy.com/blog/haproxy/proxy-protocol
- Nginx: [https://docs.nginx.com/nginx/admin-guide/load-balancer/using-proxy-protocol/](https://docs.nginx.com/nginx/admin-guide/load-balancer/using-proxy-protocol/)

:::: tabs type:card

::: tab Configure HAProxy 

**Prerequisite**: HAProxy installed. For detailed introduction and installation of HAProxy, see [HAProxy official website](http://www.haproxy.org/).

To configure HAProxy as the LB for EMQX and terminate the SSL connection, you can modify `/etc/haproxy/haproxy.cfg` as shown in the code example below. 

Note: The file path may differ based on your installation mode.

```bash
listen mqtt-ssl
  bind *:8883 ssl crt /etc/ssl/emqx/emq.pem no-sslv3
  mode tcp
  maxconn 50000
  timeout client 600s
  default_backend emqx_cluster

backend emqx_cluster
  mode tcp
  balance source
  timeout server 50s
  timeout check 5000
  server emqx1 192.168.0.2:1883 check inter 10000 fall 2 rise 5 weight 1
  server emqx2 192.168.0.3:1883 check inter 10000 fall 2 rise 5 weight 1
```

:::

::: tab Configure NGINX

**Prerequisite**: NGINX installed. For detailed introduction and installation of HAProxy, see [Nginx website](https://www.nginx.com/).

To configure NGINX as the LB for EMQX and terminate the SSL connection, you can modify `/etc/nginx/nginx.conf` as shown in the code example below. 

Note: The file path may differ based on your installation mode.

```bash
stream {
  upstream stream_backend {
      zone tcp_servers 64k;
      hash $remote_addr;
      server 192.168.0.2:1883 max_fails=2 fail_timeout=30s;
      server 192.168.0.3:1883 max_fails=2 fail_timeout=30s;
  }

  server {
      listen 8883 ssl;
      status_zone tcp_server;
      proxy_pass stream_backend;
      proxy_buffer_size 4k;
      ssl_handshake_timeout 15s;
      ssl_certificate     /etc/emqx/certs/cert.pem;
      ssl_certificate_key /etc/emqx/certs/key.pem;
  }
}
```

:::

::::
