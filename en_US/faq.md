# FAQ 

## What is EMQ X suitable for? 

*EMQ X* is designed for helping build large-scale IoT platforms and applications, as it supports most of the popular IoT protocols: MQTT, MQTT-SN, CoAP, LwM2M, and STOMP. 

And another use case is Instant Messaging via MQTT. Use *EMQ X* for the notifications and also for the signaling server of WebRTC. 

## Do I have to learn Erlang programming language to use EMQ X? 

Not necessary. The config file is self-explained and thus easy to understand. There are several plugins developed by the *EMQ X* team, which should be sufficient for most use cases. 

You could always write your own plugins if necessary, then a basic knowledge of Erlang is 'good to have'. 

## I found the port 4369 and another random port(63703) are open, is this secure? 
    
    
    tcp 0 0 0.0.0.0:4369 0.0.0.0:* LISTEN 13736/epmd
    tcp 0 0 0.0.0.0:8083 0.0.0.0:* LISTEN 16745/beam.smp
    tcp 0 0 0.0.0.0:8883 0.0.0.0:* LISTEN 16745/beam.smp
    tcp 0 0 0.0.0.0:63703 0.0.0.0:* LISTEN 16745/beam.smp
    tcp 0 0 0.0.0.0:1883 0.0.0.0:* LISTEN 16745/beam.smp

These TCP Ports are opened by *EMQ X* nodes. 

The TCP port 4369 is for erlang port mapping, and the *random ports* are communication ports for distributed erlang. All of these ports must be allowed to other nodes in the same cluster, by configuring your firewall. 

You may need to limit the *random ports* in a range, after that you can allow this range of TCP ports in your firewall. For example, you could limit the range to 6369~6379 by following config in the emqx.conf: 
    
    
    node.dist_listen_min = 6369
    node.dist_listen_max = 6379

The communication via these ports are secured by the cookie, so that one cannot access your cluster if he does not know the cookie: 
    
    
    node.cookie = emqxsecretcookie

It is recommended that only keep your cluster in a sub-network behind a firewall, not across data-centers. But if you do want to, you could secure you distributed erlang via SSL, by configuring the  etc/ssl_dist.conf  . For more information, see [ ssl_distribution ](http://erlang.org/doc/apps/ssl/ssl_distribution.html)

## How do I config the ACL? 

Follow this doc: [ emqx_guide#acl ](https://developer.emqx.io/docs/broker/v3/en/guide.html#acl)

Start with the simplest one using etc/acl.conf: 
    
    
    %% Allow user with username 'dashboard' to subscribe '$SYS/#'
    {allow, {user, "dashboard"}, subscribe, ["$SYS/#"]}.
    
    %% Allow clients from localhost to subscribe and publish to any topics
    {allow, {ipaddr, "127.0.0.1"}, pubsub, ["$SYS/#", "#"]}.
    
    %% Deny clients to subscribe topics which matches '$SYS/#' and the topic exactly equals to 'abc/#'. But this doesn't deny topics such as 'abc' or 'abc/d'
    {deny, all, subscribe, ["$SYS/#", {eq, "abc/#"}]}.
    
    %% Allow all by default
    {allow, all}.

ACL via databases such as mysql and mongodb should be similar. It is recommended to test your ideas for better understanding. 

## Is EMQ X ready for production? 

Yes. The core features are solid and stable. A full-time team and many contributors from community are developing this project. You could submit issues for any feature requests or bug reports. 

## Benchmark and performance issue 

Check out the benchmark report here: 

[ emqx-benchmark ](https://emq-xmeter-benchmark-en.readthedocs.io/en/latest/)

## Is 'session' identified by ClientID? What will happen when session is expired? 

Yes, the session is identified by ClientID. 

When a client connects to broker with 'clean session = false', a session identified by ClientID will be created. The session will expire after 48 hours(configured in etc/emqx.config) if no client connections bind with it, and all queued messages and subscriptions will be dropped. 

## Is config 'max_mqueue_len' means queue for one session or one topic? 

For one session. Topic just dispatches messages to clients or sessions that match the subscriptions. 

## What would happen when a session has too many offline messages on one topic? 

If the offline messages in the session exceed the 'max_mqueue_len', the older offline messages are dropped from the queue. 

## How do I configure EMQ X to only keep the latest retained message of a topic? 

The broker only keeps the latest retained message of a topic, as specified by MQTT specifications. 
