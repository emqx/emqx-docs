# Keep Alive mechanism

MQTT protocol has introduced the Keep Alive mechanism to avoid the communication issue caused by TCP half-open. The MQTT clients and broker can follow the Keep Alive mechanism to confirm the TCP connection and close the corresponding connection if there are any issues. 

When a client initiates a `CONNECT` request to the broker, for example, EMQX, you can use this parameter to determine the keep alive period. 

If the client is in an idle state and has no packets to send, it can send a 2-byte  `PINGREQ` heartbeat message to EMQX, after EMQX receives the `PINGREQ` message, it will return a 2-byte `PINGRESP` message. 

By default, EMQX will disconnect the client if it fails to receive any packets or `PINGREQ` messages from the client within 1.5 times the Keep Alive time. 

::: tip

For more information on the Keep Alive mechanism, see blog [*What is the MQTT Keep Alive parameter for?*](https://www.emqx.com/en/blog/mqtt-keep-alive)

:::
