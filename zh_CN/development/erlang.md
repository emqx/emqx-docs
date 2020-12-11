---
# 编写日期
date: 2020-02-07 17:15:26
# 作者 Github 名称
author: wivwiv
# 关键字
keywords:
# 描述
description:
# 分类
category: 
# 引用
ref:
---


# MQTT Erlang 客户端库

[emqtt](https://github.com/emqx/emqtt) 是开源 MQTT Broker EMQ X 官方提供的客户端库，适用于 Erlang 语言。

Erlang 生态有多个 MQTT Broker 实现，如通过插件支持 MQTT 的 RabbitMQ ，VerenMQ、EMQ X 等。但受限于语言小众性，可用的 MQTT 客户端库几乎没有选择的余地，MQTT 社区收录的 Erlang 客户端库中 [emqtt](https://github.com/emqx/emqtt) 是最佳选择。

emqtt 完全由 Erlang 实现，完成整支持 MQTT v3.1.1 和 MQTT v5.0 协议版本，支持 SSL 单双向认证与 WebSocket 连接。另一款 MQTT 基准测试工具 [emqtt_bench](https://github.com/emqx/emqtt-bench) 就基于该客户端库构建。

## emqtt 使用示例

本示例包含 Erlang 的 emqtt 客户端库连接 EMQ X Broker，并进行消息收发完整代码：

```erlang
ClientId = <<"test">>.
{ok, ConnPid} = emqtt:start_link([{clientid, ClientId}]).
{ok, _Props} = emqtt:connect(ConnPid).
Topic = <<"guide/#">>.
QoS = 1.
{ok, _Props, _ReasonCodes} = emqtt:subscribe(ConnPid, {Topic, QoS}).
{ok, _PktId} = emqtt:publish(ConnPid, <<"guide/1">>, <<"Hello World!">>, QoS).
%% If the qos of publish packet is 0, `publish` function would not return packetid.
ok = emqtt:publish(ConnPid, <<"guide/2">>, <<"Hello World!">>, 0).

%% Recursively get messages from mail box.
Y = fun (Proc) -> ((fun (F) -> F(F) end)((fun(ProcGen) -> Proc(fun() -> (ProcGen(ProcGen))() end) end))) end.
Rec = fun(Receive) -> fun()-> receive {publish, Msg} -> io:format("Msg: ~p~n", [Msg]), Receive(); _Other -> Receive() after 5 -> ok end end end.
(Y(Rec))().

%% If you don't like y combinator, you can also try named function to recursively get messages in erlang shell.
Receive = fun Rec() -> receive {publish, Msg} -> io:format("Msg: ~p~n", [Msg]), Rec(); _Other -> Rec() after 5 -> ok end end.
Receive().

{ok, _Props, _ReasonCode} = emqtt:unsubscribe(ConnPid, <<"guide/#">>).

ok = emqtt:disconnect(ConnPid).
```



## emqtt MQTT 5.0 支持

目前 emqtt 已经完整支持 MQTT 5.0。