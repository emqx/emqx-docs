.. EMQ documentation master file, created by
   sphinx-quickstart on Fri Jan 15 13:58:10 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

================================
*EMQ* - 百万级开源MQTT消息服务器
================================

*EMQ* 2.0 (Erlang/Enterprise/Elastic MQTT Broker) 是基于 Erlang/OTP 语言平台开发，支持大规模连接和分布式集群，发布订阅模式的开源 MQTT 消息服务器。

.. NOTE:: 2.0 版本开始 emqttd 消息服务器自正式简称为 EMQ

*EMQ* 2.0 完整支持 MQTT V3.1/V3.1.1 版本协议规范，并扩展支持 WebSocket、Stomp、CoAP、MQTT-SN 或私有 TCP 协议。*EMQ* 2.0 消息服务器支持单节点100万连接与多节点分布式集群:

TODO: 2.0-rc.1 图片更新.

.. image:: ./_static/images/emqtt.png

*EMQ* 2.0 为大规模客户端连接 (C1000K+) 的移动推送、移动消息、物联网、车联网、智能硬件等应用，提供一个完全开放源码、安装部署简便、企业级稳定可靠、可弹性扩展、易于定制开发的 MQTT 消息服务器。

.. NOTE:: MQTT-SN、CoAP 协议已在2.0-rc.1版本发布，LWM2M、LoRaWan 协议在2.3-beta.1版本发布。

*EMQ* 2.0 项目文档目录:

.. toctree::
   :maxdepth: 2

   getstarted
   deploy
   install
   image
   config
   cluster
   bridge
   guide
   advanced
   design
   commands
   plugins
   rest
   tune
   changes
   upgrade
   mqtt
   mqtt_sn
   lwm2m

EMQ 项目支持与联系:

+------------+--------------------------------+
| 官网:      | http://emqtt.com               |
+------------+--------------------------------+
| 项目:      | https://github.com/emqtt       |
+------------+--------------------------------+
| 微信:      | emqttd                         |
+------------+--------------------------------+
| 微博:      | http://weibo.com/emqtt         |
+------------+--------------------------------+
| Twitter:   | @emqtt                         |
+------------+--------------------------------+
| 作者:      | 李枫 <feng@emqtt.io>           |
+------------+--------------------------------+

.. image:: ./_static/images/weixin.jpg

