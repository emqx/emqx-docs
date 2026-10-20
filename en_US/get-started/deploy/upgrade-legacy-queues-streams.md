# Remove Legacy Queues and Streams Before Upgrading to EMQX 7.0

This page explains how to find and remove legacy message queues and MQTT streams on EMQX 6.1 before you upgrade to EMQX 7.0.

A legacy queue or stream is one that was created before EMQX 6.1.1, when queues and streams had no name of their own. Clients access a legacy queue only through the `$q/<topic_filter>` prefix, and a legacy stream only through the `$s/<offset>/<topic_filter>` prefix. EMQX 7.0 removes both prefixes. Complete the steps on this page before you upgrade to EMQX 7.0, while your clients can still use these prefixes.

## Who Is Affected

You are affected when both of the following are true:

- Your cluster created queues or streams on EMQX 6.1.0, and you then upgraded to EMQX 6.1.1 or later.
- You did not delete those queues or streams afterwards.

A cluster that started on EMQX 6.1.1 or later has no legacy queues or streams. EMQX 6.1.1 and later do not create new legacy records, even when a client subscribes with the `$q/` prefix.

## What Happens If You Upgrade Without Cleanup

When you upgrade to EMQX 7.0 with legacy queues or streams in place:

- The legacy record and its stored messages remain, and they continue to use storage.
- No client can subscribe to the record. EMQX 7.0 treats a `$q/...` or `$s/...` subscription as an ordinary MQTT topic filter.
- You cannot reach the record with the `$queue/<name>` or `$stream/<name>` forms, because a legacy name begins with `/`, and a valid name cannot begin with `/`.

## Prerequisites

- Run EMQX 6.1.1 or later. The commands on this page use the REST API introduced in EMQX 6.1.1. If you run EMQX 6.1.0, upgrade to EMQX 6.1.1 or a later 6.1 patch release first. The upgrade keeps your existing queues and streams.
- Create an API key with the administrator role. See [Create API Keys](../../guides/api.md#create-api-keys). The examples on this page use `key:secret` as the API key and secret key, and `localhost:18083` as the Dashboard listener address. Replace them with your own values.
- Install [jq](https://jqlang.org/). The examples use it to filter the API responses and to URL-encode names.

## Find Legacy Queues and Streams

EMQX 6.1.1 and later assign each legacy record a name derived from its topic filter: `/<topic_filter>`. For example, a legacy queue on the topic filter `sensor/+/temp` has the name `/sensor/+/temp`.

A name created through the Dashboard, the REST API, or a `$queue/` or `$stream/` subscription may contain only letters, digits, `_`, `-` and `.`. Such a name never begins with `/`. A name that begins with `/` therefore always identifies a legacy record.

List the legacy queues:

```bash
queues_response=$(curl -fsS -u key:secret "http://localhost:18083/api/v5/queues?limit=1000")
printf '%s\n' "$queues_response" \
  | jq -r '.data[] | select(.name | startswith("/")) | [.name, .topic_filter, .is_lastvalue] | @tsv'
printf '%s\n' "$queues_response" | jq '.meta'
```

List the legacy streams:

```bash
streams_response=$(curl -fsS -u key:secret "http://localhost:18083/api/v5/streams?limit=1000")
printf '%s\n' "$streams_response" \
  | jq -r '.data[] | select(.name | startswith("/")) | [.name, .topic_filter, .is_lastvalue] | @tsv'
printf '%s\n' "$streams_response" | jq '.meta'
```

Each output line shows the name, the topic filter, and whether the record uses last-value semantics. For example:

```
/sensor/+/temp	sensor/+/temp	false
```

Each command also prints the response `meta` object. When no page prints a legacy record and the final page has `meta.hasnext` set to `false`, you have no legacy records of that type.

::: tip

The response field `meta.hasnext` is `true` when another page of records is available. In that case, repeat the corresponding request and add the `cursor` value from `meta` as a query parameter, for example `?limit=1000&cursor=<cursor>`. Continue until `meta.hasnext` is `false`, and check every page for legacy records.

:::

You can also see legacy records in the Dashboard. Open **Queues** or **Streams** in the left menu. A legacy record shows a name that begins with `/`.

## Choose How to Handle Each Record

For each legacy record, decide whether you need its stored messages.

### Preserve Stored Messages

A new queue or stream receives only messages published after you create it. EMQX does not copy stored messages from the legacy record. To preserve the stored messages, let your clients consume the remaining messages from the legacy record before you delete it:

1. Create a named queue or stream with the same topic filter and settings as the legacy record. The following commands retrieve the complete configuration of the legacy record, change only its name, and use the result to create the named record.

   For the legacy queue `/sensor/+/temp`, create the named queue `sensor_temp`:

   ```bash
   curl -fsS -u key:secret \
     http://localhost:18083/api/v5/queue/%2Fsensor%2F%2B%2Ftemp \
     | jq '.name = "sensor_temp"' \
     | curl -fsS -u key:secret -X POST \
       -H "Content-Type: application/json" \
       http://localhost:18083/api/v5/queues \
       -d @-
   ```

   For the legacy stream `/factory/#`, create the named stream `factory`:

   ```bash
   curl -fsS -u key:secret \
     http://localhost:18083/api/v5/stream/%2Ffactory%2F%23 \
     | jq '.name = "factory"' \
     | curl -fsS -u key:secret -X POST \
       -H "Content-Type: application/json" \
       http://localhost:18083/api/v5/streams \
       -d @-
   ```

   These commands preserve settings such as the data retention period, limits, key expression, and queue dispatch strategy.

   From this point, each new matching message goes to both the legacy record and the named one.

2. Switch your consumers to the new subscription form, for example `$queue/sensor_temp` instead of `$q/sensor/+/temp`, or `$stream/factory` instead of `$s/<offset>/factory/#`. Update your authorization rules to allow the new topic filters. See [Message Queue](../../develop/message-queue/message-queue-concept.md) and [MQTT Streams](../../develop/mqtt-stream/mqtt-stream-concept.md) for the subscription formats.

3. Keep at least one consumer on the legacy `$q/` or `$s/` subscription until it has processed the stored messages it needs. Messages older than the data retention period are removed by EMQX in any case. The default retention period is 7 days.

4. Delete the legacy record as described in [Delete Legacy Queues and Streams](#delete-legacy-queues-and-streams).

### Discard Stored Messages

When you do not need the stored messages, delete the legacy record directly. Deleting a queue or stream also deletes all messages stored in it. You cannot undo this.

Before you delete a legacy record, move the clients that still subscribe with `$q/` or `$s/` to a named queue or stream. After the deletion, such a client stops receiving messages on that subscription. EMQX does not create the legacy record again. The subscription still succeeds, so the client receives no error. For a `$q/` subscription, EMQX logs an `mq_auto_create_error` entry with `reason: invalid_name`. Use this log entry to find clients that still use the `$q/` prefix.

## Delete Legacy Queues and Streams

### URL-Encode the Name

A legacy name contains `/` and can contain `+` and `#`. URL-encode the name before you put it in the request path. Without encoding, EMQX returns `404 Not Found`. In addition, an unencoded `#` starts a URL fragment, which is not sent to the server.

Encode a name with jq:

```bash
jq -rn --arg name '/sensor/+/temp' '$name | @uri'
```

The command prints `%2Fsensor%2F%2B%2Ftemp`. For the stream name `/factory/#`, it prints `%2Ffactory%2F%23`.

### Delete One Record

Delete the legacy queue `/sensor/+/temp`:

```bash
curl -fsS -u key:secret -X DELETE \
  http://localhost:18083/api/v5/queue/%2Fsensor%2F%2B%2Ftemp
```

Delete the legacy stream `/factory/#`:

```bash
curl -fsS -u key:secret -X DELETE \
  http://localhost:18083/api/v5/stream/%2Ffactory%2F%23
```

A successful request returns HTTP status `204` with no body. When the record does not exist, EMQX returns `404` with the code `NOT_FOUND`.

### Delete All Legacy Records

To delete every legacy queue, run:

```bash
queues_response=$(curl -fsS -u key:secret "http://localhost:18083/api/v5/queues?limit=1000")
printf '%s\n' "$queues_response" | jq '.meta'
for name in $(printf '%s\n' "$queues_response" \
  | jq -r '.data[] | select(.name | startswith("/")) | .name | @uri'); do
  curl -fsS -u key:secret -X DELETE "http://localhost:18083/api/v5/queue/$name" -w "$name %{http_code}\n"
done
```

To delete every legacy stream, run:

```bash
streams_response=$(curl -fsS -u key:secret "http://localhost:18083/api/v5/streams?limit=1000")
printf '%s\n' "$streams_response" | jq '.meta'
for name in $(printf '%s\n' "$streams_response" \
  | jq -r '.data[] | select(.name | startswith("/")) | .name | @uri'); do
  curl -fsS -u key:secret -X DELETE "http://localhost:18083/api/v5/stream/$name" -w "$name %{http_code}\n"
done
```

Each block deletes the legacy records returned on one API page and prints that page's `meta` object. If `meta.hasnext` is `true`, save its `cursor` value, repeat the block with `&cursor=<cursor>` added to the request URL, and continue until `meta.hasnext` is `false`. Each deletion output line shows an encoded name and the HTTP status. Check that every status is `204`.

### Delete from the Dashboard

1. Open **Queues** or **Streams** in the left menu.
2. Find the record whose name begins with `/`.
3. Click **Delete** in the **Actions** column, then click **Confirm**.

### Verify

Run the commands in [Find Legacy Queues and Streams](#find-legacy-queues-and-streams) again and check every page. Only when no page prints a legacy record and the final queue and stream responses both have `meta.hasnext` set to `false` does the cluster have no legacy records left. You can then continue with the upgrade to EMQX 7.0.
