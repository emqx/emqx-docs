# Hierarchical rate limiter

The hierarchical rate limiter is a multi-level rate and flow control system based on the producer/consumer model, which can flexibly and accurately control the usage rate of the corresponding resources in the EMQX node.


<a id="org00546cf"></a>

# Terminologies


<a id="orgf5b4d69"></a>

## Resource

The system currently supports rate control services for the following resources:

- **bytes\_in:** the incoming byte rate of the message
- **message\_in:** the incoming rate of messages
- **connection:** connection rate
- **message\_routing:** rate of message routing
- **batch:** rate of batch operations within the node
    
    Note: Currently only `retainer` uses this feature


<a id="org700e263"></a>

## Producer/Consumer model

The whole system is divided into two parts: `producer` and `consumer`


<a id="org693255d"></a>

### Producer

The producer is the `rate control service` in the system, which is used to manage and control the generation and allocation of resource tokens


<a id="org0b1fde9"></a>

### Consumer

Consumers are entities that consume resources. Before consuming resources, they must apply for the same amount of tokens.

The system provides three `rate control levels` for resource consumption


<a id="org310b56a"></a>

## Rate control levels

- **Node level:** resource rate control on the current EMQX node
- **Service level:** rate control for resources on a certain type of service/module

    There are currently two types of services: `listener` and `retainer`

    Among them, `listener` supports the following resources:

    - bytes\_in
    - message\_in
    - connection
    - message\_routing

    `retainer` only supports `batch` resources
- **User level:** rate control of resources on each user
    - **User:** User refers to any entity that consumes resources under a `service`, such as a connection or process, etc.


<a id="org66b1ab7"></a>

# Useage


<a id="org7e70e34"></a>

## Setting

1. Open the setting interface on Dashboard and select the type to be set
2. Set the settings as required, then click `Save`

![image](./assets/limiter_page.png)


<a id="org8e67475"></a>

### Levels Correspondence

Each resource has three levels of rate control. Use `message_in` as an example, the correspondence between levels and settings is as follows:

- **Node:** The rate-setting at the top of the interface controls the inflow rate of messages on the current node

![image](./assets/limiter_node_level.png)
- **Service level:** The `bucket` in the setting interface corresponds to the `service level`, which is used to control the total message inflow rate of all connections under a listener

![image](./assets/limiter_fun_level.png)
    Note: The number of `buckets` is not fixed, you can set multiple `buckets` according to required, and each `bucket` can be used to control the total rate limit of all connections under different listeners
- **User level:** The `Per Client` in the `bucket` corresponds to the `user level`, which is used to control the message inflow rate of each connection under the listener

![image](./assets/limiter_cli_level.png)


<a id="orgc8037f1"></a>

## Use

After the rate control service is set, it will not affect other functions. If a service (listener, retainer, etc.) wants to use the rate control service, it needs to be set in the corresponding place.


<a id="org94c5efd"></a>

### Listener

1. Open the `Listener` interface on the dashboard

    ![image](./assets/limiter_open_listener.png)

    Select the listener that needs to use the rate control service, click `Settings`
2. In the `Basic Info` field, click `Rate Limiter`

    ![image](./assets/limiter_set_limiter_in_listener.png)

3. Select the corresponding bucket under the resource that needs to be rate controlled

    ![image](./assets/limiter_chose_bucket.png)


    Note: Multiple selections are supported here, as shown in the figure
    ![image](./assets/limiter_multi_chose.png)


<a id="org1d79363"></a>

### Retainer

1. Open the `retainer` interface on the dashboard

    ![image](./assets/limiter_open_retain.png)

2. Fill in the `Limiter` in the `Flow Control` column with the bucket corresponding to the `batch` rate control service, and then update it

    ![image](./assets/limiter_set_retainer_bucket.png)