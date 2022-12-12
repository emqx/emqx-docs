# Introduction to Data Bridges

Data bridge is a channel used to connect emqx and external data systems. The external data system can be mysql, mongodb and other databases, or message brokers such as Kafka and RabbitMQ, or event an HTTP server.

Through data bridges, users can send messages from emqx to the external data system in real time, or pull data from the external data system and send it to a topic in emqx.

{% emqxce %}
::: tip
Only MQTT bridge and Webhook are supported in the emqx open source version. 
For the list of data bridges supported in the enterprise version, see:
[data integration with emqx enterpise](https://www.emqx.com/en/integrations)
:::
{% endemqxce %}

<!-- TODO sync zh -->