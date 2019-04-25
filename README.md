
《*EMQ X* R3.1 中文文档》
========================

* **[开始使用(GetStarted)](http://docs.emqtt.cn/zh_CN/latest/getstarted.html)**
  * [*EMQ X* R3.1 消息服务器简介](http://docs.emqtt.cn/zh_CN/latest/getstarted.html#emq-3-0)
  * [MQTT 发布订阅模式简述](http://docs.emqtt.cn/zh_CN/latest/getstarted.html#mqtt)
  * [五分钟下载启动EMQ](http://docs.emqtt.cn/zh_CN/latest/getstarted.html#emq)
  * [源码编译EMQ X R3.1](http://docs.emqtt.cn/zh_CN/latest/getstarted.html#compile)
  * [Web 管理控制台(Dashboard)](http://docs.emqtt.cn/zh_CN/latest/getstarted.html#web-dashboard)
  * [*EMQ X* R3.1消息服务器功能列表](http://docs.emqtt.cn/zh_CN/latest/getstarted.html#features)
  * [*EMQ X* R3.1扩展插件列表](http://docs.emqtt.cn/zh_CN/latest/getstarted.html#plugins)
  * [100万线连接测试说明](http://docs.emqtt.cn/zh_CN/latest/getstarted.html#c1000k)
  * [开源 MQTT 客户端项目](http://docs.emqtt.cn/zh_CN/latest/getstarted.html#mqtt-clients)

* **[程序安装(Installation)](http://docs.emqtt.cn/zh_CN/latest/install.html)**
  * [*EMQ X* R3.1 程序包下载](http://docs.emqtt.cn/zh_CN/latest/install.html#emq-3-0)
  * [RPM 包安装](http://docs.emqtt.cn/zh_CN/latest/install.html#rpm)
  * [DEB 包安装](http://docs.emqtt.cn/zh_CN/latest/install.html#deb)
  * [Linux服务器安装](http://docs.emqtt.cn/zh_CN/latest/install.html#linux)
  * [FreeBSD服务器安装](http://docs.emqtt.cn/zh_CN/latest/install.html#freebsd)
  * [Mac OS X系统安装](http://docs.emqtt.cn/zh_CN/latest/install.html#mac-os-x)
  * [Windows服务器安装](http://docs.emqtt.cn/zh_CN/latest/install.html#windows)
  * [Docker镜像安装](http://docs.emqtt.cn/zh_CN/latest/install.html#docker)
  * [源码编译安装](http://docs.emqtt.cn/zh_CN/latest/install.html#build-from-source)
  * [TCP服务端口占用](http://docs.emqtt.cn/zh_CN/latest/install.html#tcp)
  * [快速设置](http://docs.emqtt.cn/zh_CN/latest/install.html#quick-setup)

* **[配置说明(Configuration)](http://docs.emqtt.cn/zh_CN/latest/config.html)**
  * [*EMQ X* R3.1 配置文件](http://docs.emqtt.cn/zh_CN/latest/config.html#emqx-r3-0)
  * [*EMQ* 配置变更历史](http://docs.emqtt.cn/zh_CN/latest/config.html#emq)
  * [*EMQ* 3.1 环境变量](http://docs.emqtt.cn/zh_CN/latest/config.html#id2)
  * [*EMQ* 集群设置](http://docs.emqtt.cn/zh_CN/latest/config.html#emq-x) 
  * [*EMQ* 集群自动发现](http://docs.emqtt.cn/zh_CN/latest/config.html#id7) 
  * [*EMQ* 节点与Cookie](http://docs.emqtt.cn/zh_CN/latest/config.html#emq-x-cookie)
  * [*EMQ* 节点连接方式](http://docs.emqtt.cn/zh_CN/latest/config.html#id8)
  * [Erlang虚拟机参数](http://docs.emqtt.cn/zh_CN/latest/config.html#erlang)
  * [RPC参数配置](http://docs.emqtt.cn/zh_CN/latest/config.html#rpc)
  * [日志参数配置](http://docs.emqtt.cn/zh_CN/latest/config.html#id9)
  * [匿名认证与ACL文件](http://docs.emqtt.cn/zh_CN/latest/config.html#acl)
  * [MQTT 协议参数配置](http://docs.emqtt.cn/zh_CN/latest/config.html#mqtt)
  * [Zones 参数配置](http://docs.emqtt.cn/zh_CN/latest/config.html##mqtt-zones)  
  * [MQTT Listeners参数说明](http://docs.emqtt.cn/zh_CN/latest/config.html#mqtt-listeners)
  * [MQTT(TCP)监听器 - 1883](http://docs.emqtt.cn/zh_CN/latest/config.html#mqtt-tcp-1883)
  * [MQTT(SSL)监听器 - 8883](http://docs.emqtt.cn/zh_CN/latest/config.html#mqtt-ssl-8883)
  * [MQTT(WebSocket)监听器 - 8083](http://docs.emqtt.cn/zh_CN/latest/config.html#mqtt-websocket-8083)
  * [MQTT(WebSocket/SSL)监听器 - 8084](http://docs.emqtt.cn/zh_CN/latest/config.html#mqtt-websocket-ssl-8084)
  * [Bridges 桥接](http://docs.emqtt.cn/zh_CN/latest/config.html#bridges)
  * [Modules 模块](http://docs.emqtt.cn/zh_CN/latest/config.html#modules)
  * [扩展插件配置文件 ](http://docs.emqtt.cn/zh_CN/latest/config.html#id8)
  * [Broker 消息中间件](http://docs.emqtt.cn/zh_CN/latest/config.html#broker)
  * [Erlang虚拟机监控设置](http://docs.emqtt.cn/zh_CN/latest/config.html#id19)

* **[分布集群(Cluster)](http://docs.emqtt.cn/zh_CN/latest/cluster.html)**
  * [Erlang/OTP分布式编程](http://docs.emqtt.cn/zh_CN/latest/cluster.html#erlang-otp)
  * [*EMQ X*分布集群设计](http://docs.emqtt.cn/zh_CN/latest/cluster.html#emq-x)
  * [节点发现与自动集群](http://docs.emqtt.cn/zh_CN/latest/cluster.html#autodiscovery)
  * [集群脑裂与自动愈合](http://docs.emqtt.cn/zh_CN/latest/cluster.html#cluster-netsplit)
  * [集群节点自动清除](http://docs.emqtt.cn/zh_CN/latest/cluster.html#id8)
  * [跨节点会话(Session)](http://docs.emqtt.cn/zh_CN/latest/cluster.html#session)
  * [防火墙设置](http://docs.emqtt.cn/zh_CN/latest/cluster.html#cluster-firewall)

* **[节点桥接(Bridge)](http://docs.emqtt.cn/zh_CN/latest/bridge.html)**
  * [*EMQ X*节点间桥接](http://docs.emqtt.cn/zh_CN/latest/bridge.html#emq-x)
  * [mosquitto桥接](http://docs.emqtt.cn/zh_CN/latest/bridge.html#mosquitto)
  * [rsmb桥接](http://docs.emqtt.cn/zh_CN/latest/bridge.html#rsmb)

* **[用户指南(User Guide)](http://docs.emqtt.cn/zh_CN/latest/guide.html)**
  * [MQTT认证设置](http://docs.emqtt.cn/zh_CN/latest/guide.html#mqtt)
  * [开启匿名认证](http://docs.emqtt.cn/zh_CN/latest/guide.html#id1)
  * [用户名密码认证](http://docs.emqtt.cn/zh_CN/latest/guide.html#id2)
  * [ClientId认证](http://docs.emqtt.cn/zh_CN/latest/guide.html#clientid)
  * [LDAP插件认证](http://docs.emqtt.cn/zh_CN/latest/guide.html#ldap)
  * [HTTP插件认证](http://docs.emqtt.cn/zh_CN/latest/guide.html#http)
  * [MySQL插件认证](http://docs.emqtt.cn/zh_CN/latest/guide.html#mysql)
  * [Postgre插件认证](http://docs.emqtt.cn/zh_CN/latest/guide.html#postgre)
  * [Redis插件认证](http://docs.emqtt.cn/zh_CN/latest/guide.html#redis)
  * [MongoDB插件认证](http://docs.emqtt.cn/zh_CN/latest/guide.html#mongodb)
  * [访问控制(ACL)](http://docs.emqtt.cn/zh_CN/latest/guide.html#acl)
  * [默认访问控制设置](http://docs.emqtt.cn/zh_CN/latest/guide.html#id4)
  * [HTTP插件访问控制](http://docs.emqtt.cn/zh_CN/latest/guide.html#id5)
  * [MySQL插件访问控制](http://docs.emqtt.cn/zh_CN/latest/guide.html#id6)
  * [Postgre插件访问控制](http://docs.emqtt.cn/zh_CN/latest/guide.html#id7)
  * [Redis插件访问控制](http://docs.emqtt.cn/zh_CN/latest/guide.html#id8)
  * [MongoDB插件访问控制](http://docs.emqtt.cn/zh_CN/latest/guide.html#id9)
  * [MQTT发布订阅](http://docs.emqtt.cn/zh_CN/latest/guide.html#id10)
  * [HTTP发布接口](http://docs.emqtt.cn/zh_CN/latest/guide.html#http-publish)
  * [MQTT WebSocket连接](http://docs.emqtt.cn/zh_CN/latest/guide.html#mqtt-websocket)
  * [$SYS-系统主题](http://docs.emqtt.cn/zh_CN/latest/guide.html#sys)
  * [追踪](http://docs.emqtt.cn/zh_CN/latest/guide.html#trace)

* **[高级特性 (Advanced)](http://docs.emqtt.cn/zh_CN/latest/advanced.html)**
  * [共享订阅(Shared Subscription)](http://docs.emqtt.cn/zh_CN/latest/advanced.html#shared-subscription)

* **[架构设计(Design)](http://docs.emqtt.cn/zh_CN/latest/design.html)**
  * [前言](http://docs.emqtt.cn/zh_CN/latest/design.html#intro)
  * [系统架构](http://docs.emqtt.cn/zh_CN/latest/design.html#architecture)
  * [连接层设计](http://docs.emqtt.cn/zh_CN/latest/design.html#connection-layer)
  * [会话层设计](http://docs.emqtt.cn/zh_CN/latest/design.html#session-layer)
  * [路由层设计](http://docs.emqtt.cn/zh_CN/latest/design.html#route-layer)
  * [分布层设计](http://docs.emqtt.cn/zh_CN/latest/design.html#distributed-layer)
  * [认证与访问控制设计](http://docs.emqtt.cn/zh_CN/latest/design.html#auth-acl)
  * [钩子(Hook)设计](http://docs.emqtt.cn/zh_CN/latest/design.html#hook)
  * [插件(Plugin)设计](http://docs.emqtt.cn/zh_CN/latest/design.html#plugin)
  * [Mnesia/ETS 表设计](http://docs.emqtt.cn/zh_CN/latest/design.html#mnesia-ets)
  * [Erlang设计相关](http://docs.emqtt.cn/zh_CN/latest/design.html#erlang)

* **[管理命令(Commands)](http://docs.emqtt.cn/zh_CN/latest/commands.html)**
  * [status命令](http://docs.emqtt.cn/zh_CN/latest/commands.html#status)
  * [mgmt命令](http://docs.emqtt.cn/zh_CN/latest/commands.html#mgmt)
  * [broker命令](http://docs.emqtt.cn/zh_CN/latest/commands.html#broker)
  * [cluster命令](http://docs.emqtt.cn/zh_CN/latest/commands.html#cluster)
  * [clients命令](http://docs.emqtt.cn/zh_CN/latest/commands.html#clients)
  * [sessions命令](http://docs.emqtt.cn/zh_CN/latest/commands.html#sessions)
  * [routes命令](http://docs.emqtt.cn/zh_CN/latest/commands.html#routes)
  * [subscriptions命令](http://docs.emqtt.cn/zh_CN/latest/commands.html#subscriptions)
  * [plugins命令](http://docs.emqtt.cn/zh_CN/latest/commands.html#plugins)
  * [bridges命令](http://docs.emqtt.cn/zh_CN/latest/commands.html#bridges)
  * [vm命令](http://docs.emqtt.cn/zh_CN/latest/commands.html#vm)
  * [trace命令](http://docs.emqtt.cn/zh_CN/latest/commands.html#trace)
  * [listeners](http://docs.emqtt.cn/zh_CN/latest/commands.html#listeners)
  * [mnesia命令](http://docs.emqtt.cn/zh_CN/latest/commands.html#mnesia)
  * [retainer命令](http://docs.emqtt.cn/zh_CN/latest/commands.html#retainer)
  * [admins命令](http://docs.emqtt.cn/zh_CN/latest/commands.html#admins)

* **[扩展插件(Plugins)](http://docs.emqtt.cn/zh_CN/latest/plugins.html)**
  * [emqx_retainer Retainer 模块插件](http://docs.emqtt.cn/zh_CN/latest/plugins.html#emqx-retainer-retainer)
  * [emqx_auth_clientid - ClientID认证插件](http://docs.emqtt.cn/zh_CN/latest/plugins.html#emqx-auth-clientid-clientid)
  * [emqx_auth_username - 用户名密码认证插件](http://docs.emqtt.cn/zh_CN/latest/plugins.html#emqx-auth-username)
  * [emqx_plugin_template: 插件开发模版](http://docs.emqtt.cn/zh_CN/latest/plugins.html#emqx-plugin-template)
  * [emqx_dashboard: Dashboard插件](http://docs.emqtt.cn/zh_CN/latest/plugins.html#emqx-dashboard-dashboard)
  * [emqx_auth_ldap: LDAP认证插件](http://docs.emqtt.cn/zh_CN/latest/plugins.html#emqx-auth-ldap-ldap)
  * [emqx_auth_http: HTTP认证/访问控制插件](http://docs.emqtt.cn/zh_CN/latest/plugins.html#emqx-auth-http-http)
  * [emqx_auth_mysql: MySQL认证/访问控制插件](http://docs.emqtt.cn/zh_CN/latest/plugins.html#emqx-auth-mysql-mysql)
  * [emqx_auth_pgsql: Postgre认证/访问控制插件](http://docs.emqtt.cn/zh_CN/latest/plugins.html#emqx-auth-pgsql-postgre)
  * [emqx_auth_redis: Redis认证/访问控制插件](http://docs.emqtt.cn/zh_CN/latest/plugins.html#emqx-auth-redis-redis)
  * [emqx_auth_mongo: MongoDB认证/访问控制插件](http://docs.emqtt.cn/zh_CN/latest/plugins.html#emqx-auth-mongo-mongodb)
  * [emqx_coap: CoAP协议插件](http://docs.emqtt.cn/zh_CN/latest/plugins.html#emqx-coap-coap)
  * [emqx_sn: MQTT-SN协议插件](http://docs.emqtt.cn/zh_CN/latest/plugins.html#emqx-sn-mqtt-sn)
  * [emqx_stomp: Stomp协议插件](http://docs.emqtt.cn/zh_CN/latest/plugins.html#emqx-stomp-stomp)
  * [emqx_recon: Recon性能调试插件](http://docs.emqtt.cn/zh_CN/latest/plugins.html#emqx-recon-recon)
  * [emqx_reloader: 代码热加载插件](http://docs.emqtt.cn/zh_CN/latest/plugins.html#emqx-reloader)
  * [EMQ 3.1插件开发](http://docs.emqtt.cn/zh_CN/latest/plugins.html#emqx-3-0)

* **[测试调优(Tune Guide)](http://docs.emqtt.cn/zh_CN/latest/tune.html)**
  * [Linux操作系统参数](http://docs.emqtt.cn/zh_CN/latest/tune.html#linux)
  * [TCP协议栈网络参数](http://docs.emqtt.cn/zh_CN/latest/tune.html#tcp)
  * [Erlang虚拟机参数](http://docs.emqtt.cn/zh_CN/latest/tune.html#erlang)
  * [EMQ X 消息服务器参数](http://docs.emqtt.cn/zh_CN/latest/tune.html#emq-x)
  * [测试客户端设置](http://docs.emqtt.cn/zh_CN/latest/tune.html#id1)

支持与联系
==========

公司:    http://emqtt.com

微信:    emqttd

微博:    http://weibo.com/emqtt

Twitter: @emqtt

联系:    contact@emqx.io <feng@emqx.io>

http://www.emqtt.com/docs/v3/index.html | http://www.emqtt.com/docs/v2/index.html | http://docs.emqtt.cn/

![weixin](https://github.com/emqtt/docs_zh/blob/master/source/_static/images/weixin.jpg)

