
.. _deploy:

========
部署架构
========

EMQ 消息服务器集群可作为物联网接入服务(IoT Hub)，部署在青云、AWS、阿里等公有云或企业私有云平台。

典型部署结构:

.. image:: _static/images/deploy_1.png

------------
LB(负载均衡)
------------

LB(负载均衡器)负责分发设备的MQTT连接与消息到EMQ集群，LB提高EMQ集群可用性、实现负载平衡以及动态扩容。

部署架构推荐在LB终结SSL连接。设备与LB之间SSL安全连接，LB 与EMQ之间普通TCP连接。这种部署模式下EMQ单集群可轻松支持100万设备。

公有云厂商LB产品:

+---------------+-----------------+----------------------------------------------------+
| 云计算厂商    | 是否支持SSL终结 | LB产品介绍                                         |
+===============+=================+====================================================+
| `青云`_       | 是              | https://docs.qingcloud.com/guide/loadbalancer.html |
+---------------+-----------------+----------------------------------------------------+
| `AWS`_        | 是              | https://aws.amazon.com/cn/elasticloadbalancing/    |
+---------------+-----------------+----------------------------------------------------+
| `阿里云`_     | 否              | https://www.aliyun.com/product/slb                 |
+---------------+-----------------+----------------------------------------------------+
| `UCloud`_     | 未知            | https://ucloud.cn/site/product/ulb.html            |
+---------------+-----------------+----------------------------------------------------+
| `QCloud`_     | 未知            | https://www.qcloud.com/product/clb                 |
+---------------+-----------------+----------------------------------------------------+

私有部署LB服务器:

+---------------+-----------------+------------------------------------------------------+
| 开源LB        | 是否支持SSL终结 | 方案介绍                                             |
+===============+=================+======================================================+
| `HAProxy`_    | 是              | https://www.haproxy.com/solutions/load-balancing.html|
+---------------+-----------------+------------------------------------------------------+
| `NGINX`_      | PLUS产品支持    | https://www.nginx.com/solutions/load-balancing/      |
+---------------+-----------------+------------------------------------------------------+

国内公有云部署推荐青云(EMQ合作伙伴)，国外部署推荐AWS。私有部署推荐使用HAPRoxy作为LB。

--------
EMQ 集群
--------

EMQ 节点集群部署在LB之后，建议部署在VPC或私有网络内。公有云厂商青云、AWS、UCloud、QCloud均支持VPC网络。

EMQ 默认开启的MQTT服务TCP端口:

+-----------+-----------------------------------+
| 1883      | MQTT协议端口                      |
+-----------+-----------------------------------+
| 8883      | MQTT/SSL端口                      |
+-----------+-----------------------------------+
| 8083      | MQTT/WebSocket端口                |
+-----------+-----------------------------------+
| 8084      | MQTT/WebSocket/SSL端口            |
+-----------+-----------------------------------+

防火墙根据使用的MQTT接入方式，开启上述端口的访问权限。

EMQ 节点集群使用的TCP端口:

+-----------+-----------------------------------+
| 4369      | 集群节点发现端口                  |
+-----------+-----------------------------------+
| 6369      | 集群节点控制通道                  |
+-----------+-----------------------------------+

集群节点间如有防护墙，需开启上述TCP端口互访权限。

-------------------
青云(QingCloud)部署
-------------------

1. 创建VPC网络。

2. VPC网络内创建EMQ集群'私有网络'，例如: 192.168.0.0/24

3. 私有网络内创建两台EMQ主机，例如:

    +-------+-------------+
    | emq1  | 192.168.0.2 |
    +-------+-------------+
    | emq2  | 192.168.0.3 |
    +-------+-------------+

4. 安装并集群EMQ 主机，具体配置请参考安装集群章节。

5. 创建LB(负载均衡器)并指定公网IP地址。

6. 在LB上创建MQTT TCP监听器:

.. image:: _static/images/deploy_2.png
 
或创建SSL监听器，并终结SSL在LB:

.. image:: _static/images/deploy_3.png
  
7. MQTT客户端连接LB公网地址测试。

---------------
亚马逊(AWS)部署
---------------

1. 创建VPC网络。

2. VPC网络内创建EMQ集群'私有网络'，例如: 192.168.0.0/24

3. 私有网络内创建两台EMQ主机，指定上面创建的VPC网络,例如:

    +-------+-------------+
    | emq1  | 192.168.0.2 |
    +-------+-------------+
    | emq2  | 192.168.0.3 |
    +-------+-------------+

4. 在安全组中，开放MQTT服务的TCP端口，比如1883, 8883。

5. 安装并集群EMQ主机，具体配置请参考安装集群章节。

6. 创建ELB(Classic负载均衡器)，指定VPC网络，并指定公网IP地址。

7. 在ELB上创建MQTT TCP监听器:

.. image:: _static/images/deploy_4.png

或创建SSL监听器，并终结SSL在LB:

.. image:: _static/images/deploy_5.png

8. MQTT客户端连接LB公网地址测试。

----------
阿里云部署
----------

.. TODO:: 阿里云LB终结SSL?

------------
私有网络部署
------------

EMQ 集群直连
------------

EMQ集群直接挂DNS轮询，设备通过域名或者IP地址列表访问:

1. 部署EMQ集群，具体参考`程序包安装`与`集群配置`文档。

2. EMQ节点防火墙开启外部MQTT访问端口，例如1883, 8883。

3. 设备通过IP地址列表或域名访问EMQ集群。

.. NOTE:: 不推荐这种部署方式。

HAProxy -> EMQ Cluster
----------------------

HAProxy作为LB部署EMQ集群，并终结SSL连接:

1. 创建EMQ集群节点，例如:

+-------+-------------+
| 节点  | IP地址      |
+=======+=============+
| emq1  | 192.168.0.2 |
+-------+-------------+
| emq2  | 192.168.0.3 |
+-------+-------------+

2. 配置/etc/haproxy/haproxy.cfg，示例::

    listen mqtt-ssl
        bind *:8883 ssl crt /etc/ssl/emqttd/emq.pem no-sslv3
        mode tcp
        maxconn 50000
        timeout client 600s
        default_backend emq_cluster

    backend emq_cluster
        mode tcp
        balance source
        timeout server 50s
        timeout check 5000
        server emq1 192.168.0.2:1883 check inter 10000 fall 2 rise 5 weight 1
        server emq2 192.168.0.3:1883 check inter 10000 fall 2 rise 5 weight 1
        source 0.0.0.0 usesrc clientip

NGINX Plus -> EMQ Cluster
-------------------------

NGINX Plus产品作为EMQ集群的LB，并终结SSL连接:

1. 注册NGINX Plus试用版，Ubuntu下安装: https://cs.nginx.com/repo_setup

2. 创建EMQ节点集群，例如: 

+-------+-------------+
| 节点  | IP地址      |
+=======+=============+
| emq1  | 192.168.0.2 |
+-------+-------------+
| emq2  | 192.168.0.3 |
+-------+-------------+

3. 配置/etc/nginx/nginx.conf，示例::

    stream {
        # Example configuration for TCP load balancing

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
            ssl_certificate     /etc/emqttd/certs/cert.pem;
            ssl_certificate_key /etc/emqttd/certs/key.pem;
        }
    }

.. _青云:    https://qingcloud.com
.. _AWS:     https://aws.amazon.com
.. _阿里云:  https://www.aliyun.com
.. _UCloud:  https://ucloud.cn
.. _QCloud:  https://www.qcloud.com
.. _HAProxy: https://www.haproxy.org
.. _NGINX:   https://www.nginx.com 

