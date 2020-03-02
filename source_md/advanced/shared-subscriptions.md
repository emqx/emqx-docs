---
# 标题
title: 共享订阅
# 编写日期
date: 2020-02-20 11:57:30
# 作者 Github 名称
author: terry-xiaoyu
# 关键字
keywords:
# 描述
description:
# 分类
category: 
# 引用
ref: undefined
---

# 共享订阅

共享订阅是在多个订阅者之间实现负载均衡的订阅方式：

```
                                                   [subscriber1] got msg1
             msg1, msg2, msg3                    /
[publisher]  ---------------->  "$share/g/topic"  -- [subscriber2] got msg2
                                                 \
                                                   [subscriber3] got msg3
```

上图中，共享 3 个 subscriber 用共享订阅的方式订阅了同一个主题 `$share/g/topic`，其中`topic` 是它们订阅的真实主题名，而  `$share/g/` 是共享订阅前缀。EMQ X Broker 支持两种格式的共享订阅前缀：

| 示例            | 前缀        | 真实主题名 |
| --------------- | ----------- | ---------- |
| $queue/t/1      | $queue/     | t/1        |
| $shared/abc/t/1 | $shared/abc | t/1        |

### 带群组的共享订阅

以 `$share/<group-name>` 为前缀的共享订阅是带群组的共享订阅：

group-name 可以为任意字符串，属于同一个群组内部的订阅者将以负载均衡接收消息，但 EMQ X Broker 会向不同群组广播消息。

例如，假设订阅者 s1，s2，s3 属于群组 g1，订阅者 s4，s5 属于群组 g2。那么当 EMQ X Broker 向这个主题发布消息 msg1 的时候：

- EMQ X Broker 会向两个群组 g1 和 g2 同时发送 msg1

- s1，s2，s3 中只有一个会收到 msg1
- s4，s5 中只有一个会收到 msg1

```
                                       [s1]
           msg1                      /
[emqx]  ------>  "$share/g1/topic"    - [s2] got msg1
         |                           \
         |                             [s3]
         | msg1
          ---->  "$share/g2/topic"   --  [s4]
                                     \
                                      [s5] got msg1
```

### 不带群组的共享订阅

以 `$queue/` 为前缀的共享订阅是不带群组的共享订阅。它是 `$share` 订阅的一种特例，相当与所有订阅者都在一个订阅组里面：

```
                                       [s1] got msg1
        msg1,msg2,msg3               /
[emqx]  --------------->  "$queue/topic" - [s2] got msg2
                                     \
                                       [s3] got msg3
```

### 共享订阅的均衡策略

EMQ X Broker 的共享订阅支持以下几种均衡策略：

|    策略     |             描述             |
| :---------- | :--------------------------- |
| random      | 在所有订阅者中随机选择       |
| round_robin | 按照订阅顺序                 |
| sticky      | 一直发往上次选取的订阅者     |
| hash        | 按照发布者 ClientID 的哈希值 |
