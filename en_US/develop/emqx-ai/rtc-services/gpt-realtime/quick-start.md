# Build a Real-Time Voice Agent with EMQX + GPT-Realtime

This guide explains how to quickly build a real-time voice agent application using the GPT-Realtime model together with EMQX.

## Obtain a Temporary API Key

To connect to GPT-Realtime from a browser using native WebRTC, you must first obtain a temporary (ephemeral) API key. This key can be generated via the OpenAI REST API:

```bash
export OPENAI_API_KEY="sk-xxxxxx"
curl -s -X POST https://api.openai.com/v1/realtime/client_secrets \
  -H "Authorization: Bearer $OPENAI_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{"session": {"type": "realtime", "model": "gpt-realtime"}}' | jq .value
```

## Implement Real-Time Voice Chat

The following example shows how to connect to the GPT-Realtime model using native WebRTC to implement real-time voice chat:

```javascript
// Put the obtained ephemeral key here
const EPHEMERAL_KEY = "ek_xxxxxx";

// Create a peer connection
const pc = new RTCPeerConnection();

// Set up to play remote audio from the model
audioElement.current = document.createElement("audio");
audioElement.current.autoplay = true;
pc.ontrack = (e) => (audioElement.current.srcObject = e.streams[0]);

// Add local audio track for microphone input in the browser
const ms = await navigator.mediaDevices.getUserMedia({
    audio: true,
});
pc.addTrack(ms.getTracks()[0]);

// Set up data channel for sending and receiving events
const dc = pc.createDataChannel("oai-events");

// Start the session using the Session Description Protocol (SDP)
const offer = await pc.createOffer();
await pc.setLocalDescription(offer);

const sdpResponse = await fetch("https://api.openai.com/v1/realtime/calls", {
    method: "POST",
    body: offer.sdp,
    headers: {
        Authorization: `Bearer ${EPHEMERAL_KEY}`,
        "Content-Type": "application/sdp",
    },
});

const answer = {
    type: "answer",
    sdp: await sdpResponse.text(),
};
await pc.setRemoteDescription(answer);

// Listen for server events
dc.addEventListener("message", (e) => {
    const event = JSON.parse(e.data);
    console.log("Received event:", event);
});
```

In addition to creating a WebRTC audio channel, this code also creates a Data Channel for sending and receiving events from the GPT-Realtime model. All received events are logged to the console. If you encounter issues such as missing audio during testing, check the console output for detailed error information.

## Control Devices Using MCP

1. Start EMQX, install and configure the MCP Bridge plugin.

2. Run an MCP Server to simulate a smart light. For detailed steps, refer to [Use the EMQX MCP Bridge to Access IoT Devices](../../mcp-bridge/quick-start.md).

   Note that EMQX must be deployed in a public network environment, and the MCP Bridge plugin must be configured with a valid SSL certificate so that GPT-Realtime can access the MCP service over HTTPS.

3. Modify Frontend Code to Use MCP Tools.

   To enable MCP tools, add a function `handle_event()` to process GPT-Realtime events:

   ```javascript
   // Listen for server events
   dc.addEventListener("message", (e) => {
       const event = JSON.parse(e.data);
       handle_event(event);
   });
   ```

   Within this function, handle the `session.created` event so that a `session.update` event is sent when the session is created to enable MCP tools. Set the MCP server address to `https://your-emqx-host:port/mcp`:

   ```javascript
   function handle_event(event) {
       if (event.type === "session.created") {
           // Send client events
           const session_update_event = {
               type: "session.update",
               session: {
                   type: "realtime",
                   model: "gpt-realtime",
                   // can be set to "text"
                   output_modalities: ["audio"],
                   tools: [
                       {
                           type: "mcp",
                           server_label: "mqtt_mcp_bridge",
                           server_description: "EMQX MCP over MQTT Bridge",
                           server_url: "https://your-emqx-host:port/mcp",
                           require_approval: "never",
                       }
                   ],
                   tool_choice: "auto",
                   // You can still set direct session fields; these override prompt fields if they overlap:
                   instructions: "I have a smart light and its client ID is abc123"
               }
           };
           dc.send(JSON.stringify(session_update_event));
       } else if (event.type === "response.done") {
           console.log("Received response done:", event);
       } else {
           console.log("Received event:", event);
       }
   }
   ```

Now, when you access the frontend page in a browser and have a voice conversation with GPT-Realtime, the model can access and control IoT devices through MCP tools.

::: tip
 GPT-Realtime can access MCP servers only over HTTPS. Make sure that:

- The MCP plugin is configured with a valid, non-self-signed SSL certificate
- The URL uses a domain name instead of an IP address and is publicly accessible
   :::

::: tip

GPT-Realtime requires Streamable HTTP to access MCP servers, so you must use the `/mcp` endpoint of the EMQX MCP Bridge plugin rather than the `/sse` endpoint.

:::

## Send Messages to the Model

In the earlier code, system instructions were used to inform the model of the device’s client ID in advance:

```javascript
const session_update_event = {
    type: "session.update",
    session: {
        ...
        instructions: "I have a smart light and its client ID is abc123"
    }
};
```

GPT-Realtime also supports sending messages to the model during an ongoing conversation via the WebRTC Data Channel to add contextual information:

```javascript
// Send client events
const event = {
    type: "conversation.item.create",
    item: {
        type: "message",
        role: "user",
        content: [
            {
                type: "input_text",
                text: "I have a smart light and its client ID is abc123",
            },
        ],
    },
};
dc.send(JSON.stringify(event));
```
