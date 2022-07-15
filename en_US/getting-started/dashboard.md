# Dashboard

## Introduction

EMQX Broker provides Dashboard to facilitate users to monitor and manage the broker itself and configure some functions such as client access control.

The main functionalites accessible from the dashboard user interface are:

* Basic monitoring of the running EMQX cluster.
* View, search and manage connected clients, even kick some off.
* View, search subscriptions and topics.
* Manage username/password for clients or configure an external resource to perfrom authentication for clients. For example databases or web servers.
* Create or update data integration rules (a.k.a Rule Engine) to extract, filter or transform data to or from external data platforms such as another MQTT broker, databases or web servers.
* Manage extensions such as gateways and gRPC endpoints, as well as plugins.
* Diagnostic tools such as a built-in websocket MQTT client, online tracing etc.

## Start Dashboard

EMQX dashboard is a web application which is by default served on port 18083.
After EMQX is started successfully, you can visit [http://localhost:18083/](http://localhost:18083/)
(replace localhost with your actual IP address) through a browser to access dashboard.

EMQX dashboard listeners can be both turned off, so it runs headlessly
however this may significantly impact the operability of the service as a lot of the configuration updates are
quite easy to make from the dashboard but not from editing the config files manually.

## Configure Dashboard

The default dashboard listener in EMQX serves `http`. You can enable HTTPS for dashboard or use other listener ports.

You can find more information on how to configure the dashboard listeners in [Configuration Doc](../admin/cfg.md#dashboard).

<!--TODO: maybe add some screenshots-->
