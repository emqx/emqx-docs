# v4.1

## 4.1.5

*Release Date: 2020-08-30*

EMQ X 4.1.5 is released now, it fixes a bug in MQTT message parser.

## 4.1.4

*Release Date: 2020-08-29*

EMQ X 4.1.4 is released now, it mainly includes the following changes:

### emqx

**Bug fixes:**

- Fix the issue of abnormal memory growth caused by the topic metrics feature

  Github PR: [emqx#3680](https://github.com/emqx/emqx/pull/3680)

### emqx-bridge-mqtt

**Enhancements:**

- The clientid configuration item supports `${node}` placeholders to optimize the user experience under the cluster

  Github PR: [emqx-bridge-mqtt#99](https://github.com/emqx/emqx-bridge-mqtt/pull/99)

### emqx-management

**Bug fixes:**

- Fix the issue that the data migration function is not available under Windows

  Github PR: [emqx-management#262](https://github.com/emqx/emqx-management/pull/262)

### emqx-lua-hook

**Bug fixes:**

- Fix the issue that the Username field cannot be obtained

  Github PR: [emqx-lua-hook#115](https://github.com/emqx/emqx-lua-hook/pull/115)

## 4.1.3

*Release Date: 2020-07-24*

EMQ X 4.1.3 is released now, it mainly includes the following changes:

### emqx-management

**Bug fixes:**

- Add type checking for the payload field in PUBLISH API

  Github PR: [emqx/emqx-management#250](https://github.com/emqx/emqx-management/pull/250)

### emqx-retainer

**Bug fixes:**

- Fix the issue that the retained message will not be sent when the subscription topic contains both '+' and '#'

  Github PR: [emqx/emqx-retainer#146](https://github.com/emqx/emqx-retainer/pull/146)

## 4.1.2

*Release Date: 2020-08-08*

- Fixes some known issues

## 4.1.1

*Release Date: 2020-08-07*

1. rule_engine adds Pulsar consumer group resources
2. rule_engine add Kafka consumer group resources
3. rule_engine Add data and save to TDengine database
4. rule_engine Add offline message save to MySQL action
5. rule_engine Add offline message save to PostgreSQL action
6. rule_engine Add offline message and save to Cassandra action
7. rule_engine Add offline message and save to MongoDB action
8. rule_engine Add to get subscription relationship from MySQL
9. rule_engine Add to get subscription relationship from PostgreSQL
10. rule_engine Add to get subscription relationship from Cassandra
11. rule_engine Add to get subscription relationship from MongoDB
12. rule_engine Save data to MongoDB Action support message template
13. Fix the bug that the HTTP Publish API cannot support the json format of the payload

## 4.1.0

*Release Date: 2020-07-18*

1. Built-in preview version license, you can start emqx directly without registering on the official website to get the license
2. Modify the license expiration policy, the emqx service will not stop, but the new connection cannot log in
3. Rule engine add MQTT subscription resources
4. Rule engine MQTT message bridge support pool
5. Rule engine MQTT message bridge fixes the cluster cannot use the bug
6. Rule engine Add data and save to ClickHouse database
7. InfluxDB supports http/https connection
8. Enterprise edition multi-language development supports northbound message processing
9. Rule engine Add offline message and save to redis action
10. Rule engine add to get subscription relationship from redis
