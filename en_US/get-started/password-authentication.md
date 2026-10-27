---
description: "Configure username and password authentication on a local EMQX broker and verify accepted and rejected MQTT connections."
---

# Authenticate with a Username and Password

Configure username and password authentication on your local Quick Start broker. Create an MQTT user and verify that valid credentials allow a connection while invalid or missing credentials are rejected. Keep this account for [Control Access to Topics](topic-permissions.md).

## Prerequisites

Complete [Understand QoS](qos.md) and keep the [Quick Start broker](quick-start.md) running with its ports bound to `127.0.0.1`. Open the [EMQX Dashboard](http://127.0.0.1:18083) and keep administrator access. Complete the sign-in and password change described in Quick Start if needed.

Use MQTTX CLI from Quick Start on your computer. These tests use host `127.0.0.1`, port `1883`, MQTT over TCP, and MQTT 5. Replace `1883` if you changed the MQTT host port. Unencrypted credentials are acceptable only for this disposable loopback test; use TLS for remote connections.

Close earlier test clients before changing authentication. Use only this disposable broker and ensure another authenticator cannot accept the test user. MQTT accounts are separate from Dashboard administrators and API keys.

<!-- include: ../develop/authentication/built-in-database.md#create-the-authenticator -->

<!-- include: ../develop/authentication/built-in-database.md#add-an-mqtt-user -->

<!-- include: ../develop/authentication/built-in-database.md#verify-accepted-and-rejected-connections -->

## Continue Learning

Disconnect the test clients, but keep the authenticator and non-superuser account `mqtt-user` with its password. Next, [control access to topics](topic-permissions.md).

Subsequent clients must supply the MQTT credentials. The anonymous commands from Quick Start and MQTT Essentials no longer apply unchanged.

When you finish the learning path, follow [Quick Start: Clean Up](quick-start.md#clean-up). Removing the disposable container deletes its local users, rules, and other data. For provisioning, password migration, and connection failures, see [Authenticate Clients with the Built-in Database](../develop/authentication/built-in-database.md).
