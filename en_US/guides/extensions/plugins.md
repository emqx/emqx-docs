# Plugins

EMQX plugins allow users to extend core functionality or integrate custom logic into the broker using Erlang. This system is ideal for implementing features such as custom authentication, authorization, logging, metrics, or protocol translation.

This section introduces what plugins are and how they are developed, customized, and managed.

## What is a Plugin?

A plugin in EMQX is an Erlang/OTP application that runs inside EMQX nodes. Plugins interact with the EMQX core through hookpoints, which are predefined events such as client connection, message publish, authentication, and more.

To use a plugin in EMQX, first build it as a release package. Then choose how each node manages it:

- **Cluster-managed plugin**: Install the `.tar.gz` package through the Dashboard, REST API, or CLI. EMQX distributes the package and manages its lifecycle across the cluster.
- **Node-pinned plugin**: Starting from EMQX 6.1.6, extract the package on a specific node and list its name and version in `node.pinned_plugins`. The node runs the plugin independently of the cluster plugin configuration.

Upon startup, a plugin typically registers some of its functions as EMQX *callbacks* to modify or extend the behavior of EMQX. Plugins operate in isolation from EMQX core and from other plugins, ensuring they don’t interfere with each other. Cluster-managed plugins follow a defined lifecycle for installation, startup, shutdown, and removal. Node-pinned plugins are pre-deployed on specific nodes and can be started or stopped locally. To remove one, delete its entry from `node.pinned_plugins`, remove the plugin from the node's deployment artifact, and then restart the node.

## Key Concepts

- **Hookpoints**: Extension points in EMQX’s workflow where plugins can inject custom logic.
- **Callbacks**: Functions in the plugin module that are registered to hookpoints.
- **Configuration schema**: An optional [Avro-based schema](https://avro.apache.org/) used to validate plugin configurations and render UI forms dynamically in the EMQX Dashboard.

## Plugin Workflow Overview

1. Develop the plugin using the official [plugin template](https://github.com/emqx/emqx-plugin-template).
2. Customize its logic to suit your use case (e.g., implement authentication).
3. Build and package the plugin into a `.tar.gz` file using `make rel`.
4. Install and start the plugin through the Dashboard, CLI, or API, or deploy and configure it as a node-pinned plugin.
5. Configure and maintain the plugin according to the selected management mode.

## Learn More

- [**Develop EMQX Plugins**](./plugin-development.md): Learn how to build a plugin from scratch using the official template.
- [**Customize Plugin Logic**](./plugin-example.md): Explore examples of how to implement hook callbacks for authentication, authorization, and more.
- [**Manage Plugins**](./plugin-management.md): Understand how to install, configure, start/stop, and uninstall plugins across EMQX nodes and clusters.
- [**Plugin Catalog**](./plugin-catalog.md): Browse ready-to-use plugins maintained outside the built-in product documentation.
