# Save Offline Messages

Offline message storage primarily logs operations like client connectivity status, topic subscriptions, message content, and delivery receipts into databases like Redis, MySQL, PostgreSQL, MongoDB, or Cassandra. Although users can achieve similar functions by subscribing to relevant topics, EMQX Enterprise edition has built-in support for these, boosting efficiency and reducing developer workload.

- Client Connection State: EMQX supports retaining the client's connection state in Redis or DB.
- Client Subscription by Broker: EMQX Persistence supports subscription by broker. When a client goes online, the persistence module loads the subscriptions of the client from Redis or Databases.

## How It Works

### One-to-One Message Persistence

1. PUB publishes a message;
2. Backend records this message in DB;
3. SUB subscribes to a topic;
4. Backend retrieves the messages of this topic from DB;
5. Messages are sent to SUB;
6. Once the SUB acknowledged/received the message, backend removes
   the message from DB.

<img src="./assets/backends_1.png" alt="image" style="zoom: 67%;" />

### Many-to-Many Message Persistence

1. PUB publishes a message;
2. Backend records the message in DB;
3. SUB1 and SUB2 subscribe to a topic;
4. The backend retrieves the messages of this topic;
5. Messages are sent to SUB1 and SUB2;
6. The backend records the read position of SUB1 and SUB2, the next
   message’s retrieval starts from this position.

<img src="./assets/backends_2.png" alt="image" style="zoom:67%;" />

## Supported Data Systems

EMQX supports saving offline messages to the following databases. 

- [Cassandra](./offline_msg_to_cassandra.md)
- [ClickHouse](./offline_msg_to_clickhouse.md)
- [MongoDB](./offline_msg_to_mongodb.md)
- [MySQL](./offline_msg_to_mysql.md)
- [PostgreSQL](./offline_msg_to_pgsql.md)
- [Redis](./offline_msg_to_redis.md)

## Configuration Steps

EMQX supports various types of database persistence. While the specifics may vary, configuring any persistence type generally involves two main steps:

- Connection Setup: This involves configuring the database connection details like server address, database name, username, and password. The configuration may differ across databases.
- Event Registration and Actions: Depending on the event, you can set up corresponding actions in the configuration file or with Dashboard.

