# Clients Compatible with Multimedia Services

The Multimedia AI Server uses WebRTC for audio and video streaming, and MQTT for general messages and WebRTC signaling. In practice, clients do not need a dedicated SDK. Any client that supports WebRTC and MQTT protocols can interact with the Multimedia AI Service. Common client types include:

- **Web Browsers**: Modern browsers such as Chrome, Firefox, Edge, and Safari support WebRTC natively and can directly access the Multimedia AI Service.
- **Mobile Applications**: Mobile apps can integrate a WebRTC SDK (e.g., [Pion](https://pion.ly)) to communicate with the Multimedia AI Service.
- **Embedded Devices**: IoT devices can integrate WebRTC libraries adapted for their platforms, such as [ESP32 WebRTC](https://github.com/espressif/esp-webrtc-solution), to connect to the Multimedia AI Service.

For details on WebRTC signaling and MQTT message formats, see the [Multimedia AI Messaging Protocol](../../multimedia-ai/message-protocol.md).

We also provide a browser-based client code example demonstrating how to interact with the Multimedia Service: [TypeScript WebRTC Example](./webrtc-typescript.md).