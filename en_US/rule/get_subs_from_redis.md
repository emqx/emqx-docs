# Auto Subscribe with Redis

## Set up the Environment

Set up the Redis environment, and take MacOS X as an example:

```bash
 $ wget http://download.redis.io/releases/redis-4.0.14.tar.gz
$ tar xzf redis-4.0.14.tar.gz
$ cd redis-4.0.14
$ make && make install

# Start redis
$ redis-server
```

## Create Rules

Open [EMQX Dashboard](http://127.0.0.1:18083/#/rules) and select the "Rules" tab on the left.

Then fill in the rule SQL:

```bash
SELECT * FROM "$events/client_connected"
```

<img src="./assets/rule-engine/redis_sub_1.png" alt="image-20230523152321040" style="zoom:50%;" />



## Add an Action

Select "Add Action" on the "Response Action" interface, and then select "Get Subscription List from Redis" in the "Add Action" drop-down box.

<img src="./assets/rule-engine/redis_add_sub.png" alt="image-20230523152508102" style="zoom:50%;" />

Fill in the action parameters:

The action of "Get subscription list from Redis" requires one parameter:

1). Associated resources. The resource drop-down box is empty now, and you can click "Create" in the upper right corner to create a Redis resource:

Select "Redis single-node mode resources".

<img src="./assets/rule-engine/redis_sub.png" alt="image-20230523152801350" style="zoom:50%;" />

Fill in the resource configuration:

Fill in the real Redis server address and keep other configurations at default values. Then, click the "Test" button to ensure that the connection test is successful.

Finally, click the "Confirm" button.

Return to the response action interface and click "Confirm".

<img src="./assets/rule-engine/redis_sub_5.png" alt="image-20230523152849709" style="zoom:50%;" />

Return to the rule creation interface and click "Create".

<img src="./assets/rule-engine/redis_sub_7.png" alt="image-20230523153039950" style="zoom:50%;" />

## Test the Rule

The rule has been created, and you can insert a subscription relationship into Redis through  Redis CLI:

```bash
HSET mqtt:sub:test t1 1
```

<img src="./assets/rule-engine/redis_sub_8.png" style="zoom: 50%;" />

Log in to the device (with clientid test) via Dashboard and 

![image-20230523153725483](./assets/rule-engine/redis_sub_9.png)

Check the subscription list, and you can see that the **test** device has subscribed to the **t1** topic:

![image-20230523153908018](./assets/rule-engine/redis_sub_10.png)

