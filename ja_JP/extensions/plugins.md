# Plugins

EMQX plugins allow users to extend core functionality or integrate custom logic into the broker using Erlang. This system is ideal for implementing features such as custom authentication, authorization, logging, metrics, or protocol translation.

This section introduces what plugins are and how they are developed, customized, and managed.

## What is a Plugin?

A plugin in EMQX is an Erlang/OTP application that runs inside EMQX nodes. Plugins interact with the EMQX core through hookpoints, which are predefined events such as client connection, message publish, authentication, and more.

To be used in EMQX, a plugin must be:

1. **Built as a release** – a `.tar.gz` package containing the compiled plugin and metadata.
2. **Installed** via the Dashboard, REST API, or CLI.
3. **Managed** through configuration and lifecycle operations (start, stop, uninstall).

Upon startup, a plugin typically registers some of its functions as EMQX *callbacks* to modify or extend the behavior of EMQX. Plugins operate in isolation from EMQX core and from other plugins, ensuring they don’t interfere with each other. They are also lifecycle-aware, meaning they follow a defined process for installation, startup, shutdown, and removal. This makes them safer and easier to manage in production environments.

## Key Concepts

- **Hookpoints**: Extension points in EMQX’s workflow where plugins can inject custom logic.
- **Callbacks**: Functions in the plugin module that are registered to hookpoints.
- **Configuration schema**: An optional [Avro-based schema](https://avro.apache.org/) used to validate plugin configurations and render UI forms dynamically in the EMQX Dashboard.

## Plugin Workflow Overview

1. Develop the plugin using the official [plugin template](https://github.com/emqx/emqx-plugin-template).
2. Customize its logic to suit your use case (e.g., implement authentication).
3. Build and package the plugin into a `.tar.gz` file using `make rel`.
4. Install and start the plugin in EMQX using Dashboard, CLI, or API.
5. Update configuration or stop/uninstall as needed.

## Learn More

- [**Develop EMQX Plugins**](./plugin-development): Learn how to build a plugin from scratch using the official template.
- [**Customize Plugin Logic**](./plugin-example.md): Explore examples of how to implement hook callbacks for authentication, authorization, and more.
- [**Manage Plugins**](./plugin-management): Understand how to install, configure, start/stop, and uninstall plugins across EMQX nodes and clusters.
