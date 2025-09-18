# EMQX Message Queue Demonstration (Draft)

This guide walks you through a hands-on demonstration of the Message Queue feature introduced in EMQX 6.0. You’ll simulate MQTT clients using MQTTX, manage queues via the EMQX Dashboard and REST API, and observe how durable, asynchronous messaging works in practice.

## Objectives

This demo showcases how EMQX Message Queues can:

- Persist messages even when subscribers are offline
- Support configurable **dispatch strategies**
- Enable **Last-Value Semantics** for message compaction
- Decouple publishers and subscribers in asynchronous workflows

## Prerequisites

Before starting, ensure you have:

- **EMQX 6.0+** running (Message Queue feature enabled)
- **MQTTX** installed (or any MQTT 5.0-capable client)
- Access to the **EMQX Dashboard** (default: `http://localhost:18083`)
- REST API credentials (default user: `admin`, password: `public`)

## Step 1: Create a Message Queue

### Option 1: Use Dashboard

1. Open the **EMQX Dashboard**
2. Go to **Message Queue** in the left-hand menu
3. Click **Create**
4. In the dialog:
   - **Topic Filter**: `demo/topic`
   - **Dispatch Strategy**: `Least Inflight`
   - **Data Retention Period**: `1d`
   - **Last Value Semantics**: **Disabled**
5. Click **Create**

### Option 2: Use REST API

```
curl -u admin:public -X POST http://localhost:18083/api/v5/message_queues \
  -H "Content-Type: application/json" \
  -d '{"topic_filter": "demo/topic", "is_lastvalue": false}'
```

## Step 2: Publish Messages

Use MQTTX to simulate a **publisher**:

1. Open MQTTX and create a client (e.g., `publisher`)
2. Connect to EMQX (`mqtt://localhost:1883`)
3. Publish messages to the topic `demo/topic` with QoS 1:

Example:

```
Topic: demo/topic
QoS: 1
Payload: {"msg": "Hello 1"}
```

Repeat with more payloads: `{"msg": "Hello 2"}`, etc.

At this point, there are no subscribers. Messages will be queued and persisted by EMQX.

## Step 3: Subscribe and Consume Messages

Use MQTTX to simulate a **subscriber**:

1. Open a second client (e.g., `worker-a`)
2. Connect to EMQX
3. Subscribe to the **queue topic**:

```
Topic: $q/demo/topic
QoS: 1
```

You should now receive all previously published messages in the queue.

## Step 4: Add a Second Subscriber

To simulate multiple consumers:

1. Open another MQTTX client (e.g., `worker-b`)
2. Subscribe to the same topic: `$q/demo/topic` (QoS 1)

Observe how messages are distributed across both subscribers, based on the **dispatch strategy** (`Least Inflight`).

To test other strategies:

- Change the queue’s strategy via Dashboard or REST API
- Try `round_robin` or `random` for comparison

Example (change strategy via API):

```
curl -u admin:public -X PUT http://localhost:18083/api/v5/message_queues/queues/demo%2Ftopic \
  -H "Content-Type: application/json" \
  -d '{"dispatch_strategy": "round_robin"}'
```

## Step 5: Test Last-Value Semantics

### 1. Delete the previous queue

```
curl -u admin:public -X DELETE \
http://localhost:18083/api/v5/message_queues/queues/demo%2Ftopic
```

### 2. Create a queue with Last-Value Semantics enabled

```
curl -u admin:public -X POST http://localhost:18083/api/v5/message_queues \
  -H "Content-Type: application/json" \
  -d '{"topic_filter": "device/config", "is_lastvalue": true}'
```

### 3. Publish messages with `mq-key` property

Use MQTTX publisher:

- Topic: `device/config`
- User Property: `mq-key=wifi`
- Payload: `{"ssid": "wifi1"}`
- Repeat with updated SSIDs: `{"ssid": "wifi2"}`

### 4. Subscribe to `$q/device/config`

Use a client to subscribe to:

```
Topic: $q/device/config
QoS: 1
```

Only the **latest message per key** will be delivered — earlier ones with the same key are replaced.

## Step 6: Test Message Expiry (TTL)

### 1. Create a short-lived queue

```
curl -u admin:public -X POST http://localhost:18083/api/v5/message_queues \
  -H "Content-Type: application/json" \
  -d '{"topic_filter": "temp/topic", "is_lastvalue": false, "retention_period": "30s"}'
```

### 2. Publish messages to `temp/topic`

### 3. Wait for 30+ seconds before subscribing

### 4. Subscribe to `$q/temp/topic`

No messages should be delivered — they’ve expired and were garbage collected.

## Step 7: Monitor and Manage Queues

- Go to **Dashboard → Message Queue**
- View all queues: their topic filter, retention, dispatch strategy, etc.
- Click **Settings** to configure global MQ parameters:
  - GC Interval
  - Default Retention Period
- Use REST API for full control

Example: list all queues

```
curl -u admin:public http://localhost:18083/api/v5/message_queues
```