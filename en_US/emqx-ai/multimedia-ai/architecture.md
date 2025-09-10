# Architecture

The following diagram illustrates the architecture of a typical multimedia AI system built using EMQX AI.

```mermaid
flowchart LR
    Devices <-- WebRTC --> MultimediaServer
    MultimediaServer <-- STDIO --> AIAgents
    MultimediaServer <--> ASRTTS
    AIAgents <--> LLM

    Devices[Devices]
    MultimediaServer[Multimedia<br>Server]
    AIAgents[AI Agents]
    ASRTTS[ASR/TTS]
    LLM[LLM]
```

The following diagram illustrates the interaction flow between the components:

```mermaid
sequenceDiagram
  actor Customer as Device
  participant LoginPage as Multimedia Server
  participant P1 as AI Agents
  participant P2 as LLM

  Customer ->>+ LoginPage: WebRTC Audio
  LoginPage ->> LoginPage: ASR
  LoginPage ->> P1: ASR Results
  P1 ->> P2: Process ASR with MCP Tools
  P2 ->> P1: LLM Results
  P1 ->> P1: Process LLM Results
  P1 ->> LoginPage: TTS and send to Device
  LoginPage ->> LoginPage: TTS
  LoginPage ->> Customer: WebRTC Audio
  Customer ->> LoginPage: WebRTC Video
  P1 ->> LoginPage: Realtime Image Analysis
  LoginPage ->> LoginPage: Image Analysis
  LoginPage ->> P1: Image Analysis Result
  P1 ->> P2: Summary the Analysis Reuslt
  P2 ->> P1: Summary
  P1 ->> LoginPage: TTS and send to Device
  LoginPage ->> Customer: WebRTC Audio
  P1 ->> P1: Some other processing
  P1 ->> LoginPage: Send message to Device
  LoginPage ->> Customer: MQTT message
```
