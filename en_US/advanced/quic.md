# MQTT over QUIC

QUIC is a new general-purpose transport layer protocol for the modern networks.

See: [RFC9000](https://datatracker.ietf.org/doc/html/rfc9000)

## Introduction

MQTT over QUIC protocol is an experimental feature in EMQX 5.0.

You can evaluate MQTT over QUIC to verify how it could improve network connectivity between Client and EMQX compared to TLS1.2 and TCP. 

::: tip

This is a feature of MQTT over QUIC, not MQTT over HTTP/3.

:::

## Features

- Support 0-RTT handshake

  For low-latency connection establishments.
  
- Support Client Address Migration

  MQTT (over QUIC) client could change its network layer address or transport layer port without 
  any traffic disturbance. For example, in the scenario of NAT rebinding.
  
- Disabled traffic encryption

  MQTT (over QUIC) client could run unencrypted traffic after QUIC TLS1.3 handshake is done.
  
  See: [draft-banks-quic-disable-encryption](https://datatracker.ietf.org/doc/html/draft-banks-quic-disable-encryption)
  
  ::: tip 
  
  This is a draft RFC spec.
  
  :::

## Implementation

### Client

MQTT Client (QUIC client) initiates a QUIC connection to EMQX (QUIC Server).

After the connection handshake is done, MQTT Client then starts a bidirectional stream to EMQX.

Then all the MQTT Messages will be exchanged on this bidirectional stream.

If either side close the stream, the connection should also be closed, just like a TCP connection close.

Reference client:

[emqtt](https://github.com/emqx/emqtt)

### For TLS 1.3 handshake

ALPN: MQTT

## Feature Enabling

QUIC transport listener is disabled by default.

To experiment with QUIC transport, you need to 

1. Enable it via configuration 

    ```
    listeners.quic.default {
    enabled = true
    bind = "0.0.0.0:14567"
    max_connections = 1024000
    keyfile = "/path/to/etc/certs/key.pem"
    certfile = "/path/to/etc/certs/cert.pem"
    }
    ```

    ::: tip
    
    `/path/to/etc` is the emqx etc dir something like `/etc/emqx/`
    
    ::: 

1. restart EMQX.

## Future work

As of now, EMQX has not utilized all the features provided by QUIC, such as multi streams, stream prioritization, flow control and
unreliable datagram...

These features will be addressed in the later releases and, hopefully, become an OASIS standard.

Some hardcoded configurations in the stack are not ready for production use.
