# MQTT Erlang client library

[emqtt](https://github.com/emqx/emqtt) is the official client library provided by the open source MQTT Broker EMQX, which is suitable for Erlang language.

The Erlang ecosystem has multiple MQTT Broker implementations, such as RabbitMQ, VerenMQ, EMQX that support MQTT through plugins. However, due to the language minority, there is almost no choice for available MQTT client library. Among the Erlang client libraries included in the MQTT community, [emqtt](https://github.com/emqx/emqtt) is the best choice .

emqtt is completely implemented by Erlang. It supports MQTT v3.1.1 and MQTT v5.0 protocol versions completely, and supports SSL one-way and two-way authentication and WebSocket connection. Another MQTT benchmark test tool [emqtt_bench](https://github.com/emqx/emqtt-bench) is built based on this client library.

## emqtt usage example

This example contains the complete code for Erlang's emqtt client library connecting to EMQX Broker, sending and receiving messages:

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



## emqtt MQTT 5.0 support

Currently, emqtt has fully supported MQTT 5.0.
