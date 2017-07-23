
.. _lwm2m:

=========
LWM2M协议
=========

LWM2M是由Open Mobile Alliance(OMA)定义的一套适用于物联网的协议，它提供了设备管理和通讯的功能。协议可以在`这里 <http://www.openmobilealliance.org/wp/>`_下载。

LWM2M使用coap作为底层的传输协议，承载在UDP或者SMS上。

LWM2M定义了两种服务器

- 一种是LWM2M BOOTSTRAP SERVER，emq-lwm2m插件并未实现该服务器的功能。
- 一种是LWM2M SERVER，emq-lwm2m实现该服务器在UDP上的功能，SMS并没有实现。

LWM2M把设备上的服务抽象为Object和Resource, 在XML文件中定义各种Object的属性和功能。可以在`这里 <http://www.openmobilealliance.org/wp/OMNA/LwM2M/LwM2MRegistry.html>`_找到XML的各种定义。

-------------
EMQ-LWM2M插件
-------------

EMQ-LWM2M是EMQ的一个网关插件，实现了LWM2M的大部分功能。MQTT客户端可以通过EMQ-LWM2M访问支持LWM2M的设备。设备也可以往EMQ-LWM2M上报notification，为EMQ后端的服务采集数据。

-----------------
MQTT和LWM2M的转换
-----------------

从MQTT客户端可以发送Command给LWM2M设备。MQTT到LWM2M的命令使用如下的topic

.. code-block::

    "lwm2m/{?device_end_point_name}/command".

其中mqtt payload会是一个json格式的字符串，指定要发送的命令，更多的细节请参见emq-lwm2m的文档。
    

LWM2M设备的回复用如下topic传送
    
.. code-block::

    "lwm2m/{?device_end_point_name}/response".

mqtt payload也是一个json格式的字符串，更多的细节请参见emq-lwm2m的文档。
    


配置参数
--------

File: etc/emq_lwm2m.conf::

    lwm2m.port = 5783
       
    lwm2m.certfile = etc/certs/cert.pem

    lwm2m.keyfile = etc/certs/key.pem

    lwm2m.xml_dir =  etc/lwm2m_xml

+-----------------------------+-------------------------------------------------------------------------+
| lwm2m.port                  | 指定lwm2m监听的端口号，为了避免和emq-coap冲突，使用了非标准的5783端口   |
+-----------------------------+-------------------------------------------------------------------------+
| lwm2m.certfile              | DTLS使用的证书                                                          |
+-----------------------------+-------------------------------------------------------------------------+
| lwm2m.keyfile               | DTLS使用的秘钥                                                          |
+-----------------------------+-------------------------------------------------------------------------+
| lwm2m.xml_dir               | 存放XML文件的目录，这些XML用来定义LWM2M Object                          |
+-----------------------------+-------------------------------------------------------------------------+

启动emq-lwm2m
----------

.. code-block::

    ./bin/emqttd_ctl plugins load emq_lwm2m

---------------
LWM2M的客户端库
---------------

- https://github.com/eclipse/wakaama
- https://github.com/OpenMobileAlliance/OMA-LWM2M-DevKit 
- https://github.com/AVSystem/Anjay
- https://github.com/ConnectivityFoundry/AwaLWM2M
- http://www.eclipse.org/leshan/


