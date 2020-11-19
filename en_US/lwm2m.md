# LWM2M Protocol

Lightweight M2M (LWM2M) is a set of protocols defined by the Open Mobile Alliance (OMA) for machine-to-machine (M2M) or Internet of Things (IoT) device management and communications. It can be found [ here ](http://www.openmobilealliance.org/wp/) .

LWM2M is based on coap protocol, and can be carried on UDP or SMS.

There are two types of servers:

- LWM2M BOOTSTRAP SERVER. emq-lwm2m does not support such kind of server.
- LWM2M SERVER. emq-lwm2m implements most features of this server type, except for SMS binding.

LWM2M defines service on a device as Object and Resource, which is represented in a XML file. A list of registered XML Objects can be found [ here ](http://www.openmobilealliance.org/wp/OMNA/LwM2M/LwM2MRegistry.html) .

## EMQ-LWM2M plugin

EMQ-LWM2M is an EMQ plugin，which implemnts most LWM2M features. MQTT client is able to access LWM2M device through emq-lwm2m plugin, by sending a command and reading its response.

## Convert between MQTT and LWM2M

Commands from mqtt client to LWM2M device is carried in following topic:

    "lwm2m/{?device_end_point_name}/command".

And mqtt payload will be a json format data which specifies the command. Please refer to emq-lwm2m document for details.

Response from LWM2M device to mqtt client is carried in following topic:

    "lwm2m/{?device_end_point_name}/response".

And mqtt payload will be a json format data which specifies the command. Please refer to emq-lwm2m document for details.

## Parameters

File: etc/emq_lwm2m.conf:

    lwm2m.port = 5783

    lwm2m.certfile = etc/certs/cert.pem

    lwm2m.keyfile = etc/certs/key.pem

    lwm2m.xml_dir =  etc/lwm2m_xml

| lwm2m.port     | LWM2M udp port. Port 5783 is used to prevent conflict against emq-coap |
| -------------- | ---------------------------------------------------------------------- |
| lwm2m.certfile | DTLS certificate                                                       |
| lwm2m.keyfile  | DTLS private key                                                       |
| lwm2m.xml_dir  | A directory to store XML files which define LWM2M Objects              |

## Start emq-lwm2m

    ./bin/emqttd_ctl plugins load emq_lwm2m

## LWM2M clients

- [ https://github.com/eclipse/wakaama ](https://github.com/eclipse/wakaama)
- [ https://github.com/OpenMobileAlliance/OMA-LWM2M-DevKit ](https://github.com/OpenMobileAlliance/OMA-LWM2M-DevKit)
- [ https://github.com/AVSystem/Anjay ](https://github.com/AVSystem/Anjay)
- [ https://github.com/ConnectivityFoundry/AwaLWM2M ](https://github.com/ConnectivityFoundry/AwaLWM2M)
- [ http://www.eclipse.org/leshan/ ](http://www.eclipse.org/leshan/)
