.. EMQ documentation master file, created by
   sphinx-quickstart on Fri Jan 15 13:58:10 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

================================
*EMQ* - 百万级开源MQTT消息服务器
================================

*EMQ* 2.0(Erlang/Enterprise/Elastic MQTT Broker)是基于Erlang/OTP语言平台开发，支持大规模连接和分布式集群，发布订阅模式的开源MQTT消息服务器。

.. NOTE:: 2.0版本开始emqttd消息服务器自正式简称为EMQ。

*EMQ* 2.0完整支持MQTT V3.1/V3.1.1版本协议规范，并扩展支持WebSocket、Stomp、CoAP、MQTT-SN或私有TCP协议。*EMQ* 2.0消息服务器支持单节点100万连接与多节点分布式集群:

TODO: 2.0-rc.1图片更新.

.. image:: ./_static/images/emqtt.png

*EMQ* 2.0为大规模客户端连接(C1000K+)的移动推送、移动消息、物联网、车联网、智能硬件等应用，提供一个完全开放源码、安装部署简便、企业级稳定可靠、可弹性扩展、易于定制开发的MQTT消息服务器。

.. NOTE:: MQTT-SN、CoAP协议支持已在2.0-rc.1版本发布。

*EMQ* 2.0项目文档目录:

.. toctree::
   :maxdepth: 2

   getstarted
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
   mqtt-sn
   lwm2m

EMQ项目支持与联系:

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

