# Remove Legacy Queues and Streams Before Upgrading to EMQX 7.0

This page explains how to find and remove legacy message queues and MQTT streams on EMQX 6.1 before you upgrade to EMQX 7.0.

A legacy queue or stream is one that was created before EMQX 6.1.1, when queues and streams had no name of their own. Clients reach a legacy queue only through the `$q/<topic_filter>` prefix, and a legacy stream only through the `$s/<offset>/<topic_filter>` prefix. EMQX 7.0 removes both prefixes. Complete the steps on this page before you upgrade to EMQX 7.0, while your clients can still use these prefixes.

## Who Is Affected

You are affected when both of the following are true:

- Your cluster created queues or streams on EMQX 6.1.0, and you then upgraded to EMQX 6.1.1 or later.
- You did not delete those queues or streams afterwards.

A cluster that started on EMQX 6.1.1 or later has no legacy queues or streams. EMQX 6.1.1 and later do not create new legacy records, even when a client subscribes with the `$q/` prefix.

## What Happens If You Upgrade Without Cleanup

When you upgrade to EMQX 7.0 with legacy queues or streams in place:

- The legacy record and its stored messages remain, and they continue to use storage.
- No client can subscribe to the record. EMQX 7.0 treats a `$q/...` or `$s/...` subscription as an ordinary MQTT topic filter.
- You cannot reach the record with the `$queue/<name>` or `$stream/<name>` forms, because a legacy name begins with `/` and a valid name must not.

## Prerequisites

- Run EMQX 6.1.1 or later. The commands on this page use the REST API introduced in EMQX 6.1.1. If you run EMQX 6.1.0, upgrade to the latest 6.1 patch release first. The upgrade keeps your existing queues and streams.
- Create an API key with the administrator role. See [Create API Keys](../../guides/api.md#create-api-keys). The examples on this page use `key:secret` as the API key and secret, and `localhost:18083` as the Dashboard listener address. Replace them with your own values.
- Install [jq](https://jqlang.org/). The examples use it to filter the API responses and to URL-encode names.

## Find Legacy Queues and Streams

EMQX 6.1.1 and later assign each legacy record a name derived from its topic filter: `/<topic_filter>`. For example, a legacy queue on the topic filter `sensor/+/temp` has the name `/sensor/+/temp`.

A name created through the Dashboard, the REST API, or a `$queue/` or `$stream/` subscription may contain only letters, digits, `_`, `-` and `.`. Such a name never begins with `/`. A name that begins with `/` therefore always identifies a legacy record.

List the legacy queues:

```bash
curl -s -u key:secret "http://localhost:18083/api/v5/queues?limit=1000" \
  | jq -r '.data[] | select(.name | startswith("/")) | [.name, .topic_filter, .is_lastvalue] | @tsv'
```

List the legacy streams:

```bash
curl -s -u key:secret "http://localhost:18083/api/v5/streams?limit=1000" \
  | jq -r '.data[] | select(.name | startswith("/")) | [.name, .topic_filter, .is_lastvalue] | @tsv'
```

Each output line shows the name, the topic filter, and whether the record uses last-value semantics. For example:

```
/sensor/+/temp	sensor/+/temp	false
```

When a command prints nothing, you have no legacy records of that type.

::: tip

The response field `meta.hasnext` is `true` when more records exist than `limit` returned. In that case, repeat the request and add the `cursor` value from `meta` as a query parameter, for example `?limit=1000&cursor=<cursor>`.

:::

You can also see legacy records in the Dashboard. Open **Queues** or **Streams** in the left menu. A legacy record shows a name that begins with `/`.

## Choose How to Handle Each Record

For each legacy record, decide whether you need its stored messages.

### Keep the Data

A new queue or stream receives only messages published after you create it. EMQX does not copy stored messages from the legacy record. To keep the data, let your clients consume the remaining messages from the legacy record before you delete it:

1. Create a named queue or stream with the same topic filter and settings as the legacy record. For example, for the legacy queue `/sensor/+/temp`:

   ```bash
   curl -s -u key:secret -X POST -H "Content-Type: application/json" \
     http://localhost:18083/api/v5/queues \
     -d '{"name": "sensor_temp", "topic_filter": "sensor/+/temp", "is_lastvalue": false}'
   ```

   For the legacy stream `/factory/#`:

   ```bash
   curl -s -u key:secret -X POST -H "Content-Type: application/json" \
     http://localhost:18083/api/v5/streams \
     -d '{"name": "factory", "topic_filter": "factory/#", "is_lastvalue": false}'
   ```

   From this point, each new matching message goes to both the legacy record and the named one.

2. Switch your consumers to the new subscription form, for example `$queue/sensor_temp` instead of `$q/sensor/+/temp`, or `$stream/factory` instead of `$s/<offset>/factory/#`. Update your authorization rules to allow the new topic filters. See [Message Queue](../../develop/message-queue/message-queue-concept.md) and [MQTT Streams](../../develop/mqtt-stream/mqtt-stream-concept.md) for the subscription formats.

3. Keep at least one consumer on the legacy `$q/` or `$s/` subscription until it has processed the stored messages it needs. Messages older than the data retention period are removed by EMQX in any case. The default retention period is 7 days.

4. Delete the legacy record as described in [Delete Legacy Queues and Streams](#delete-legacy-queues-and-streams).

### Discard the Data

When you do not need the stored messages, delete the legacy record directly. Deleting a queue or stream also deletes all messages stored in it. You cannot undo this.

Before you delete a legacy record, move the clients that still subscribe with `$q/` or `$s/` to a named queue or stream. After the deletion, such a client stops receiving messages on that subscription. EMQX does not create the legacy record again. The subscription still succeeds, so the client receives no error. For a `$q/` subscription, EMQX logs an `mq_auto_create_error` entry with `reason: invalid_name`. Use this log entry to find clients that still use the `$q/` prefix.

## Delete Legacy Queues and Streams

### URL-Encode the Name

A legacy name contains `/` and can contain `+` and `#`. URL-encode the name before you put it in the request path. Without encoding, EMQX returns `404 Not Found`, and `#` ends the URL path in most HTTP clients.

Encode a name with jq:

```bash
jq -rn --arg name '/sensor/+/temp' '$name | @uri'
```

The command prints `%2Fsensor%2F%2B%2Ftemp`. For the stream name `/factory/#`, it prints `%2Ffactory%2F%23`.

### Delete One Record

Delete the legacy queue `/sensor/+/temp`:

```bash
curl -s -u key:secret -X DELETE \
  http://localhost:18083/api/v5/queue/%2Fsensor%2F%2B%2Ftemp
```

Delete the legacy stream `/factory/#`:

```bash
curl -s -u key:secret -X DELETE \
  http://localhost:18083/api/v5/stream/%2Ffactory%2F%23
```

A successful request returns HTTP status `204` with no body. When the record does not exist, EMQX returns `404` with the code `NOT_FOUND`.

### Delete All Legacy Records

To delete every legacy queue, run:

```bash
for name in $(curl -s -u key:secret "http://localhost:18083/api/v5/queues?limit=1000" \
  | jq -r '.data[] | select(.name | startswith("/")) | .name | @uri'); do
  curl -s -u key:secret -X DELETE "http://localhost:18083/api/v5/queue/$name" -w "$name %{http_code}\n"
done
```

To delete every legacy stream, run:

```bash
for name in $(curl -s -u key:secret "http://localhost:18083/api/v5/streams?limit=1000" \
  | jq -r '.data[] | select(.name | startswith("/")) | .name | @uri'); do
  curl -s -u key:secret -X DELETE "http://localhost:18083/api/v5/stream/$name" -w "$name %{http_code}\n"
done
```

Each line of output shows an encoded name and the HTTP status. Check that every status is `204`.

### Delete from the Dashboard

1. Open **Queues** or **Streams** in the left menu.
2. Find the record whose name begins with `/`.
3. Click **Delete** in the **Actions** column, then click **Confirm**.

### Verify

Run the commands in [Find Legacy Queues and Streams](#find-legacy-queues-and-streams) again. When they print nothing, the cluster has no legacy records left, and you can continue with the upgrade to EMQX 7.0.
