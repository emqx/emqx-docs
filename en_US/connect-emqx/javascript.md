# MQTT JavaScript SDK Example

[MQTT.js](https://www.npmjs.com/package/mqtt) is a module written in JavaScript that implements the MQTT protocol client function and can be used in browsers and Node.js environments.

Due to the single-threaded nature of JavaScript, MQTT.js is a fully asynchronous MQTT client. MQTT.js supports MQTT and MQTT over WebSocket. The support in different operating environments is as follows:

- Browser environment: MQTT over WebSocket (including WeChat applet, Alipay applet and other customized browser environments)
- Node.js environment: MQTT, MQTT over WebSocket

Except for a small number of connection parameters that is different in different environments, the other APIs are the same.

Install using npm :

```bash
npm i mqtt
```

Install using CDN (browser):

```html
<script src="https://unpkg.com/mqtt/dist/mqtt.min.js"></script>
<script>
    // Initialize a mqtt variable globally
    console.log(mqtt)
</script>
```

In the environment where Node.js is installed, you can use MQTT.js globally in the form of a command line of `npm i mqtt -g` command.

```bash
npm i mqtt -g

mqtt help

> MQTT.js command line interface, available commands are:

  * publish     publish a message to the broker
  * subscribe   subscribe for updates from the broker
  * version     the current MQTT.js version
  * help        help about commands

> Launch 'mqtt help [command]' to know more about the commands.
```

## MQTT.js usage example

This example contains the complete code of MQTT.js in JavaScript language connecting EMQX, sending and receiving messages:

```javascript
// const mqtt = require('mqtt')
import mqtt from 'mqtt'

// connection option
const options = {
  		clean: true, // retain session
      connectTimeout: 4000, // Timeout period
      // Authentication information
      clientId: 'emqx_test',
      username: 'emqx_test',
      password: 'emqx_test',
}

// Connect string, and specify the connection method by the protocol
// ws Unencrypted WebSocket connection
// wss Encrypted WebSocket connection
// mqtt Unencrypted TCP connection
// mqtts Encrypted TCP connection
// wxs WeChat applet connection
// alis Alipay applet connection
const connectUrl = 'wss://broker.emqx.io:8084/mqtt'
const client = mqtt.connect(connectUrl, options)

client.on('reconnect', (error) => {
    console.log('reconnecting:', error)
})

client.on('error', (error) => {
    console.log('Connection failed:', error)
})

client.on('message', (topic, message) => {
  console.log('receive message：', topic, message.toString())
})
```


## MQTT.js MQTT 5.0 support

Currently, MQTT.js has fully supported MQTT 5.0.
