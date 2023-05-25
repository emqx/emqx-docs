# Ingest Data into RabbitMQ

RabbitMQ is a widely used open-source message broker that implements the Advanced Message Queuing Protocol (AMQP). It provides a robust and scalable platform for messaging between distributed systems.

EMQX supports integration with RabbitMQ, allowing you to forward MQTT messages and events to [RabbitMQ](https://www.rabbitmq.com/).

{% emqxce %}
:::tip
The RabbitMQ bridge is an EMQX Enterprise Edition feature. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. Experience the benefits of this [enterprise-ready MQTT messaging platform](https://www.emqx.com/en/try?product=enterprise) today.
:::
{% endemqxce %}

::: tip Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)

- Knowledge about [data bridges](./data-bridges.md)

- Basic knowledge of UNIX terminal and commands

:::

## Feature List

- [Connection pool](./data-bridges.md)
- [Async mode](./data-bridges.md)
- [Batch mode](./data-bridges.md)
- [Buffer mode](./data-bridges.md)

## Quick Start Tutorial

This section introduces how to use the RabbitMQ bridge with a practical tutorial, covering topics like how to create a RabbitMQ server, how to set up a bridge, and how to set up a rule for forwarding data to the bridge and testing that it all works.

This tutorial assumes that you run both EMQX and RabbitMQ on the local machine. If you have RabbitMQ and EMQX running remotely, please adjust the settings accordingly.

### Start a RabbitMQ Server

This section introduces how to start a RabbitMQ server using [Docker](https://www.docker.com/).

Run the following command to start a RabbitMQ server with the management plugin enabled (the management plugin allows you to inspect RabbitMQ with a web interface):

```bash
docker run -it --rm --name rabbitmq -p 127.0.0.1:5672:5672 -p 127.0.0.1:15672:15672 rabbitmq:3.11-management
```

You can find more information about running [RabbitMQ in Docker on Docker Hub](https://hub.docker.com/_/rabbitmq).


### Create a RabbitMQ Test Exchange and Queue

After setting up the RabbitMQ server using the Docker image, you can create a test exchange and a queue using the RabbitMQ management web interface. Here we will describe how to set this up using the RabbitMQ RabbitMQ Management Web Interface. You can skip this if you already have an exchange and queue to test with.

1. **Access the RabbitMQ Management Web Interface**: Open your web browser and navigate to http://localhost:15672/. You should see the RabbitMQ management web interface login page. Enter the default credentials: Username: guest and Password: guest, then click on "Log in".
2. **Create a Test Exchange**: After logging in, click on the "Exchanges" tab in the top menu. In the "Add a new exchange" section at the bottom of the page, input the following information:
   * **Name**: test_exchange
   * **Type**: Choose direct from the drop-down list
   * **Durability**: Choose Durable to make the exchange persistent
   * **Auto-delete**: No
   * **Internal**: No
3. Click on the "Add exchange" button to create the test exchange.
4. Create a Test Queue: Click on the "Queues" tab in the top menu. In the "Add a new queue" section at the bottom of the page, input the following information:
   * **Name**: test_queue
   * **Durability**: Choose Durable to make the queue persistent
5. Click on the "Add queue" button to create the test queue.
6. **Bind the Test Queue to the Test Exchange**: After creating the test queue, click on its name in the "Queues" tab to open its details page. Click on the "Bindings" tab under the queue name. In the "Add binding from an exchange" section, input the following information:
   * **From exchange**: test_exchange
   * **Routing key**: test_routing_key
   * **Arguments**: Leave empty
7. Click on the "Bind" button to bind the test queue to the test exchange with the specified routing key.

### Create a RabbitMQ Data Bridge

Next, you can start creating an EMQX data bridge to RabbitMQ.

1. Go to the EMQX Dashboard, click **Data Integration -> Data Bridge**.
2. Click Create on the top right corner of the page.
3. In the **Create Data Bridge page**, click to select *RabbitMQ*, and then click Next.
4. Input a name for the data bridge. The name should be a combination of upper/lower case letters and numbers.
5. Input the connection information:
   * **Server**: Input localhost or the actual hostname/IP if the RabbitMQ server is running remotely.
   * **Port**: Input 5672 or the actual port if different.
   * **Username**: Input the RabbitMQ username, default is guest.
   * **Password**: Input the RabbitMQ password, default is guest.
   * **Virtual Host**: Input the RabbitMQ virtual host, default is /.
   * **Exchange**: Input the RabbitMQ exchange to which the messages will be published (`test_exchange` if you followed the instructions above).
   * **Routing Key**: Input the RabbitMQ routing key to be used when publishing messages (`test_routing_key` if you followed the instructions above).
   * In the **Delivery Mode** dropdown, choose between `non_persistent` and `persistent`:
       * `non_persistent` (default): Messages are not persisted to disk and may be lost if RabbitMQ restarts or crashes.
       * `persistent`: Messages are persisted to disk, providing durability in case RabbitMQ restarts or crashes. Notice that you may also need to set the queue and exchange as durable to prevent messages from being lost in case RabbitMQ is restarted. See the documentation of RabbitMQ for more information.
   * **Wait for Publish Confirmations**: To ensure that messages are successfully published to RabbitMQ, you can enable publish confirmations. This feature ensures that the RabbitMQ broker acknowledges the receipt of a published message before considering it successfully published. Enabling publish confirmations can help improve the reliability of your message delivery. To enable publish confirmations, set the "Wait for Publish Confirmations" configuration option to true. By default, this option is set to true, so publish confirmations are enabled by default.
   * **Set Publish Confirmation Timeout**: The publish confirmation timeout determines the duration the publisher will wait for the broker's acknowledgment before considering the publish operation a failure.
   * **Payload Template**: The payload template field allows you to define a custom message payload format that will be sent to the RabbitMQ exchange. You can use placeholders within the template to dynamically include data from the incoming MQTT messages. These placeholders are enclosed in `${}` and will be replaced by the actual values from the message when it is forwarded to the RabbitMQ server. For example, if you want to include the MQTT message payload and its timestamp in the RabbitMQ message, you can use the template like this: {"payload": "${payload}", "timestamp": ${timestamp}}. This template will produce a JSON-formatted message containing the payload and timestamp of the incoming MQTT message. The default value for the payload template field is an empty string, which means the message payload will be forwarded to RabbitMQ without any modification.
6. Then click **Create** to finish the creation of the data bridge.

Now the RabbitMQ data bridge should appear in the data bridge list (**Data Integration -> Data Bridge**) with Resource Status as Connected. You can continue to create a rule to forward data to the new RabbitMQ bridge.

### Create a Rule for the RabbitMQ Bridge

1. Go to the EMQX Dashboard, click Data **Integration -> Rules**.
2. Click **Create** on the top right corner of the page.
3. Input a rule ID, for example, `my_rule`.
4. Input the following statement in the SQL editor, which will forward the MQTT messages matching the topic pattern t/#:
   
   ```
   SELECT 
     payload as data,
     now_timestamp() as timestamp
   FROM
     "t/#"
   
   ```
   
5. Then click the **Add Action** button, select **Forwarding with Data Bridge** from the dropdown list, and then select the data bridge you just created under **Data Bridge**.
6. Click the **Add** button to finish the setup.
7. Click the **Create** button at the bottom of the page to finish the setup.

Now a rule to forward data to RabbitMQ via a RabbitMQ bridge is created. You can click **Data Integration -> Flows** to view the topology. It can be seen that the messages under the topic `t/#` are sent and saved to RabbitMQ.

### Test the Rule and Bridge

You can use the built-in WebSocket client in the EMQX dashboard to test our rule and bridge.

Click **Diagnose -> WebSocket** Client in the left navigation menu of the Dashboard to access the WebSocket Client.

1. Fill in the connection information for the current EMQX instance. If you are running EMQX locally, you can use the default values unless you have changed EMQX's default configuration (for example, you might have configured authentication which may require you to type in a username and password).
2. Click **Connect** to connect the client to the EMQX instance.
3. Scroll down to the publish area and type in the following:
   * **Topic**: t/test
   * **Payload**: Hello World RabbitMQ from EMQX
   * **QoS**: 2
4. Click **Publish** to send the message.

If everything has gone according to plan, a message should have been published to the specified exchange in the RabbitMQ server with the specified routing key. You can check this by visiting the RabbitMQ Management Console at http://localhost:15672 (use guest as both username and password if you haven't changed the default settings) and navigating to the Queues section to verify the message has been routed to the appropriate queue(s). To see the message content, click on the the queue to see details and then on the "Get Message(s)" button.

If everything is working correctly, you should see the message Hello World RabbitMQ from EMQX in the appropriate queue(s) in RabbitMQ.

![bridge_igress](./assets/rabbitmq/rabbit_mq_management_ui_got_message.png)

