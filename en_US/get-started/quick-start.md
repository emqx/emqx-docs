---
description: "Run EMQX Enterprise locally, publish a message, and receive it with an MQTT client."
prev: false
---

# Quick Start: Send Your First MQTT Message

Start EMQX Enterprise on your computer, publish an MQTT message, and receive it in a second client.

Tested environment: EMQX Enterprise 6.3.1; Docker Engine 29.7.2; MQTTX CLI 1.13.0 (Linux ARM64 container on macOS).

## Prerequisites

You need:

- Docker running Linux containers on your computer, with Docker Engine 28 or later. Check the Server version with `docker version`.
- [MQTTX CLI 1.13.0](https://github.com/emqx/MQTTX/releases/tag/v1.13.0) installed on your computer. Use the `mqttx-cli` binary for your operating system and architecture; follow the [CLI installation guide](https://mqttx.app/docs/cli/downloading-and-installation) to add it to your PATH. Check with `mqttx --version`.
- Two terminal windows and available ports `1883` and `18083`.

The package includes a single-node Community License. See [license terms and limits](deploy/license.md) before planning a production deployment. For installation packages or Kubernetes, use [Choose a deployment method](deploy/install-enterprise.md); this tutorial follows one disposable Docker setup.

::: warning Local Evaluation Only
This disposable node uses the `legacy` security profile and accepts MQTT connections without credentials. Its published ports bind to `127.0.0.1`. Complete the [security checklist](../guides/access-control/security-checklist.md) before exposing a deployment to other computers.
:::

Run all commands below in terminals on your computer.

## Start EMQX

In terminal 1, run:

```bash
docker run -d --name emqx-quickstart --hostname quickstart.emqx.local -e EMQX_NODE_NAME=emqx@quickstart.emqx.local -e EMQX_SECURITY_PROFILE=legacy -p 127.0.0.1:1883:1883 -p 127.0.0.1:18083:18083 emqx/emqx-enterprise:6.3.1
```

Docker downloads the image if needed, starts the container, and prints its ID. Check that EMQX has finished starting:

```bash
docker exec emqx-quickstart emqx ctl status
```

Continue when the output reports that the node `emqx@quickstart.emqx.local` is started. If it is still starting, wait a few seconds and repeat the status command. For a startup error, inspect `docker logs --tail 100 emqx-quickstart`.

## Subscribe to a Topic

In terminal 1, start a client that waits for messages on `emqx/quickstart`:

```bash
mqttx sub -h 127.0.0.1 -p 1883 -l mqtt -V 5.0 -i emqx-quickstart-sub -t emqx/quickstart
```

Wait for `Subscribed to emqx/quickstart`, then leave this terminal running.

## Publish and Receive a Message

In terminal 2, send a message to the same topic:

```bash
mqttx pub -h 127.0.0.1 -p 1883 -l mqtt -V 5.0 -i emqx-quickstart-pub -t emqx/quickstart -m hello-emqx-001
```

Return to terminal 1. The received message should identify the topic `emqx/quickstart` and include this payload:

```text
hello-emqx-001
```

You have sent a message through your local EMQX broker to a subscriber. To try again, publish a different payload and look for the new value in Terminal 1.

Continue with [Connect a Client](connect-client.md) to understand and verify the connection settings, then [Publish and Subscribe](publish-subscribe.md) to explore message delivery in more detail. Both tasks reuse this local broker.

## Optional: Open the Dashboard

Open the [EMQX Dashboard](http://127.0.0.1:18083). Sign in with username `admin` and password `public`, then change the password when prompted. The [Dashboard](../guides/dashboard/introduction.md) lets you inspect clients, subscriptions, and broker activity.

## If the Message Does Not Arrive

| What You See | What to Check |
| --- | --- |
| `Connection refused` or repeated reconnect attempts | Check the container state with `docker ps -a --filter name=emqx-quickstart`. If it is running, run `docker exec emqx-quickstart emqx ctl status` and wait until the node is started. For startup failures, inspect `docker logs --tail 100 emqx-quickstart`. |
| Connected, but no message | Keep the subscriber running in Terminal 1 and wait for `Subscribed to emqx/quickstart`, then publish again in Terminal 2. In both MQTTX commands, use `-h 127.0.0.1`, `-p 1883` (or your replacement MQTT host port), and `-t emqx/quickstart`. Keep the distinct Client IDs `emqx-quickstart-sub` and `emqx-quickstart-pub` in the respective `-i` options. |
| Docker reports a port or container-name conflict | Check the Docker error for the conflicting port or name. Change the host port, keeping `127.0.0.1` and the container port unchanged; update MQTTX `-p` for MQTT or the browser URL for Dashboard. For a name conflict, change `--name` and use that name in subsequent Docker commands. Remove an existing container only if it is your disposable test container and its data is no longer needed; see [Clean Up](#clean-up). |

<!-- Migration follow-up: After the detailed troubleshooting pages are migrated, add links here to the approved destinations for content/en/troubleshoot/client-disconnections.md and content/en/troubleshoot/docker-startup.md. Verify final routes and anchors before adding reader-facing links. -->

## Clean Up

If you are continuing with [Connect a Client](connect-client.md), stop the subscriber but keep the broker running. Complete the cleanup below after finishing the learning path. If you have already removed the container, repeat [Start EMQX](#start-emqx) before continuing. The recreated container starts with fresh data.

Press Ctrl+C in Terminal 1 to stop the subscriber. When you have finished the learning path, stop and remove the `emqx-quickstart` container. Removal deletes its data and any anonymous volumes attached to it:

```bash
docker stop emqx-quickstart
```

```bash
docker rm -v emqx-quickstart
```

To keep an installation's data, follow the [Docker deployment guide](deploy/install-docker.md).

## Next Steps

- Continue with [Connect a Client](connect-client.md) to understand connection settings, then [Publish and Subscribe](publish-subscribe.md) to explore topics, QoS, and message verification.
- [Connect an application](../develop/connect-emqx/introduction.md) with an MQTT client library.
- Secure client access: [review authentication options](../guides/access-control/authn/authn.md), [add a username and password](password-authentication.md), and [control topic access](topic-permissions.md) before accepting device connections.
- [Store messages in PostgreSQL](../develop/data-integration/data-bridge-pgsql.md) and verify them in a database.
- [Choose a test client](../develop/clients/test-clients.md) for MQTTX Desktop or WebSocket testing, or [get support](support/technical-support.md) when a problem persists.
