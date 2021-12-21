# Table of Contents

1.  [Slow subscribers statistics](#org10c5186)
    1.  [Create module](#org395e425)
    2.  [Implementation note](#orgdd4d5d6)
        1.  [Latency calculation method](#org693a7e9)
        2.  [Average latency calculation method](#org712747a)
    3.  [Configuration description](#orgcd5b688)
    4.  [Slow subscribers record](#orgb3ce581)


<a id="org10c5186"></a>

# Slow subscribers statistics

This function ranks subscribers in descending order according to the average latency of message transmission

<a id="org395e425"></a>

## Create module

Open EMQ X Dashboard, click on the "Module" on the left. Then, select "add module":

![image](../../en_US/modules/assets/slow_subscribers_statistics_1.png)

Select the **Slow Subscribers Statistics** module, and then click *Start*

<a id="orgdd4d5d6"></a>

## Implementation note

This function will track the time consumption of QoS1 and QoS2 messages to complete the whole process of message transmission after arriving at EMQX, and then use the exponential moving average algorithm to calculate the average message transmission latency of the subscriber, and then rank the subscribers according to the latency.

Since QoS1 and QoS2 messages may fail to complete the transmission process due to various reasons at the same time, this function will also try to add subscribers to the ranking according to the time of expiration.

Note: In order to avoid performance overhead, the minimum latency for statistical ranking is 100ms


<a id="org693a7e9"></a>

### latency calculation method

-   QoS1
    It starts from *publishing* messages to EMQX, until EMQX receives *puback*
-   QoS2
    It starts from *publishing* messages to EMQX, until EMQX receives *pubcomp* 

<a id="org712747a"></a>

### Average latency calculation method

The average latency adopts [Exponential Moving Average Algorithm](https://en.wikipedia.org/wiki/Moving_average#Exponential_moving_average) to weight the transmission latency of each message, avoiding the influence of single jitter or historical extreme value on the average Value.

The configuration of the number of samples is *`emqx.zone.latency_samples`*. The minimum value of this value in the *emqx.conf* is 1. when it is 1, all historical values will be ignored, which will make it impossible to avoid the impact of single transmission jitter on the results. This is not recommended.

The larger the value, the greater the influence of the historical value. If it is too large, the average latency update may not reflect the current real latency in time.

<a id="orgcd5b688"></a>

## Configuration description

![image](../../en_US/modules/assets/slow_subscribers_statistics_2.png)

-   latency threshold
    *latency threshold* is used to determine whether subscribers can participate in statistics. If the latency of subscribers is lower than this value, they will not be counted
-   Maximum number of statistics
    This field determines the upper limit of the number in the statistical record table
-   Effective duration
    *Effective duration* controls the effective time of each piece of data in the statistical record. If the data has not been updated within this time range, it will be removed. (For example, after a message is sent, it is added to the statistics record because of the long latency. If the message is not sent again for a long time that exceeds this value, it will be cleared)
-   Push interval
    Slow subscribers statistics can be pushed to the system message*\(SYS/brokers/\)(node)/slow<sub>subs</sub>*. *Push interval* is used to control the time interval of the push, if set to 0, no push will be made
-   Push QoS
    QoS when pushing messages to the system 
-   Number of push batches
    The batch mode is used to push messages to the system. If the slow subscribers statistics are large, pushing all messages at one time may lead to a block. This value can be appropriately reduced.


<a id="orgb3ce581"></a>

## Slow subscribers record

![image](../../en_US/modules/assets/slow_subscribers_statistics_3.png)
Under this tab, the subscriber information will be displayed in descending order according to the time latency. After Clicking *Client ID*, it will display the subscriber details, where you can analyze and find the problem.

![image](../../en_US/modules/assets/slow_subscribers_statistics_4.png)

