# Hooks

## Definition

**Hooks** is a mechanism provided by EMQX Broker, which modifies or extends system functions by intercepting function calls, message passing, and event passing between modules.

In simple terms, the purpose of this mechanism is to enhance the flexibility of the software system, facilitate integration with the 3rd-party systems, or change the original default behavior of the system. Such as:

![Hooks-In-System](./assets/hooks_in_system.png)

When the **Hooks** mechanism does not exist in the system, the entire event processing flow (from the input of the event, to the handler and the result) is invisible and cannot be modified for the external system .

In the process, if a HookPoint where a function can be mounted is added, it will allow external plugins to mount multiple callback functions to form a call chain. Then, the internal event processing  can be extended and modified.
There are several functions that are implemented using the hook feature as follows.
1. You can use the hook system to perform multi-step streaming processing (encoding/decoding, etc.) of messages when they are published. That is, the rules engine provides the message codec function.
2. Caching of messages at message publishing time according to configuration.
3. Delayed message publishing using the hook's blocking mechanism.

The authentication/authorization commonly used in the system is implemented according to this logic. Take the [ExHook](./lang-exhook.md) as an example:

When only the `Built-in Database` authentication is enabled, according to the processing logic of the event (see figure above), the logic of the authentication module at this time is:

1. Receive user authentication request (Authenticate)
2. Execute the hook of the authentication event with `ClientInfo` and default `AccIn`.
```erlang
%% Default AccIn
{ok, #{is_superuser => false}}
```
3. Call back to the `emqx_exhook` module, assume this authentication is valid, and get **allow, is_superuser** result.
```erlang
%% AuthNResult
{ok, #{is_superuser => true}}
```
4. Return **Authentication succeeded**, and successfully access the system as a superuser

It is shown in the following figure:

```
                     EMQX Core          Hooks & Plugins
                |<---  Scope  --->|<-------  Scope  -------->|
                |                 |       emqx_exhook        |
  Authenticate  |    InitAcc      |      (user's code)       |   Authenticate
 =============> > - - - - - - - - > - - - - - - - - - - - - Yes ==============> Success
     Request    |  (Init Result)  |      authenticate?       |      Result
                |                 |                          |
                +-----------------+--------------------------+
```

Therefore, in EMQX Broker, the mechanism of **Hooks** greatly enhances the flexibility of the system. We can only hook a function on **HookPoint** that EMQX provided on specific location, to extend EMQX Broker with various behaviors. And no need to modify the [emqx](https://github.com/emqx/emqx) core code.

The implementer of the plugin only needs to pay attention to:

1. The location of **HookPoint**: Including its role, timing of execution, and how to mount and cancel mount.
2. Implementation of **callback function**: including the number of input parameters, role, data structure of the callback function, and the meaning of the returned value.
3. Understand the mechanism of callback function execution on the **chain**: including the order in which callback functions are executed, and how to terminate the execution of the chain in advance.

If you used hooks in the development of extension plugin, you should be able to fully understand these three above points, **and try not to use blocking functions inside the hooks, which will affect system throughput.**


## Callback Functions Chain

There may be multiple plugins on a single **HookPoint** that need to care about the event and perform the corresponding operation, so there may be multiple callback functions on each **HookPoint**.

We call this chain composed of multiple callback functions executed sequentially **Callback Functions Chain**.

 **Callback Functions Chain** is Currently implemented according to the concept of [Chain-of-Responsibility](https://en.wikipedia.org/wiki/Chain-of-responsibility_pattern). In order to satisfy the functionality and flexibility of the hook, it have the following attributes:

- **Ordered**: The callback functions on the **Callback Functions Chain** must be executed in certain order.
- **In Parameter** There must be one/some initialization parameters and, optionally, a cumulative value for modification by the **Callback Functions Chain**.
- **Output Result** Each function in the chain must have an output, and `ok` should be used as the function return value for callback functions that do not care about the result of execution. For example, in a notification class event, "A client has successfully logged in" does not require a return value.
- **Transitive** The result of callback functions in the chain is transitive. And to allow more flexibility in the use of hooks, we have designed **two modes** of handling the return values of callback functions in the chain.
  - **Result Transitive**</br>
    It means that each callback function in the chain accepts the entry of the chain, and the return of the previous callback function (which can be interpreted as a cumulative value) as arguments. Until the last function, the return value of the last function is then the return value of the whole chain. Specially, the chain is called with an initial value for the cumulative value to be used by the first callback function in the chain.
  - **Result Transparent**</br>
    Each function in the chain only cares about the chain's entry, and will ignore the return value of the previous callback function. And the chain will have a fixed return value of `ok`.</br>
    This is actually a special case of the first type of **Result Transitive**. That is, the initial accumulation value is `ok`, and each callback function in the chain only cares about the incoming parameters of the chain and keeps the accumulation value as `ok` unchanged.</br>
    Most of notification event follows this logic. So that we provide the general **Callback Functions Chain** execution module.
- **Callback Functions Chain** needs to allow the functions with it to *terminate the chain in advance* and *ignore this operation.*
  - **Termination in advance:** After the execution of this function is completed, the execution of the chain is directly terminated. All subsequent callback functions on the chain are ignored.</br>
    For example, an authentication believes that such clients do not need to check other authentication plug-ins after they are allowed to log in, so they need to be terminated in advance.
  - **Ignore this operation:** Do not modify the processing result on the chain, and pass it directly to the next callback function.</br>
    For example, when there are multiple authentication plug-ins, an authentication plug-in believes that such clients do not belong to its authentication scope, and it does not need to modify the authentication results. This operation should be ignored and the returned value of the previous function should be passed directly to the next function on the chain.

Therefore, we can obtain two program flow diagrams for the execution chain based on the two ways of handling the return value of the callback function on the chain.

### Result Transitiv
```
Chain execution direction ===>>>

                                 ┌------ ok ------┐                 ┌------ ok ------┐                 ┌------ ok ------┐
                                 |   (Args, Acc)  |                 |   (Args, Acc)  |                 |   (Args, Acc)  |
(Args, InitAcc) -----> Fun1 ---->┤                ├-----> Fun2 ---->┤                ├-----> Fun3 ---->┤                ├-> [NO MORE FUNCS]
                        |        |                |        |        |                |        |        |                |        |
                        |        └- {ok, NewAcc} -┘        |        └- {ok, NewAcc} -┘        |        └- {ok, NewAcc} -┘        |
                  ┌-----┴-----┐    (Args, NewAcc)    ┌-----┴-----┐    (Args, NewAcc)    ┌-----┴-----┐    (Args, NewAcc)    ┌-----┴-----┐
                  |           |                      |           |                      |           |                      |           |
                stop     {stop, NewAcc}            stop     {stop, NewAcc}            stop     {stop, NewAcc}             ok      {ok, NewAcc}
                  |           |                      |           |                      |           |                      |           |
                 Acc       NewAcc                   Acc        NewAcc                  Acc        NewAcc                  Acc        NewAcc
                  └-----┬-----┘                      └-----┬-----┘                      └-----┬-----┘                      └-----┬-----┘
                        |                                  |                                  |                                  |
 Result <---------------┴<---------------------------------┴<---------------------------------┴<---------------------------------┘
```
The meaning of the figure is:
1. A total of three callback functions are registered on the chain in the figure,`Fun1` `Fun2` `Fun3` , which are executed in the order indicated
2. The callback function execution order is determined by the priority, and the same priority is executed in the order of mounting
3. The input parameters of the chain are read-only `Args` and the parameter `InitAcc` for function modification on the chain.
4. Regardless of how the execution of chain is terminated, the chain always returns a value, which depends on the return form.
   - The callback function returns with:
     - `ok`: ignore this operation, continue the chain execution with read-only `Args` and `Acc` returned by the previous function
     - `{ok, NewAcc}`: perform some operations, modify Acc content, continue chain execution with read-only `Args` and new ` NewAcc`
   - The callback function also returns with:
     - `stop`: Stop the transfer of the chain and immediately return the result of ` Acc` from the previous function
     - `{stop, NewAcc}`: it means to stop the transfer of the chain and immediately return the result of `NewAcc` from this modification

### Result Transparent
```
Chain execution direction ===>>>

(Args) --------------> Fun1 -----------> ok ------------> Fun2 -----------> ok ------------> Fun3 -----------> ok --------> [NO MORE FUNCS]
                        |              (Args)              |              (Args)              |              (Args)              |
                       stop                               stop                               stop                               ok
                        |                                  |                                  |                                  |
                       ok                                 ok                                 ok                                 ok
 Result <---------------┴<---------------------------------┴<---------------------------------┴<---------------------------------┘
```

Comparing this with the first execution mode, you can see that the execution mode that ignores the return value in the chain is actually a special case of the pass return value mode.
This is equivalent to the case where the `InitAcc` value is `ok` and every callback function mounted on the chain returns `ok | {ok, ok} | stop | {stop, ok}`.

The above is the main design concept of the callback function chain, which regulates the execution logic of the callback function on the hook.

In the following two sections of [HookPoint](#hookpoint) and [callback function](#callback), all operations on hooks depend on  Erlang code-level API provided by [emqx](https://github.com/emqx/emqx). They are the basis for the entire hook logic implementation.
- For hooks and other language applications, Refer to: [Extension Hook](./lang-exhook.md)

## HookPoint

EMQX Broker is based on a client's key activities during its life cycle, and presets a large number of **HookPoints**. The preset mount points in the system are:

| Name                 | Description                 | Execution Timing                                                                            |
|----------------------|-----------------------------|---------------------------------------------------------------------------------------------|
| client.connect       | Process connection packet   | When the server receives the connection packet from the client                              |
| client.connack       | Issue connection response   | When the server is ready to issue a connection response message                             |
| client.connected     | Connection succeed          | After client authentication is completed and successfully connected to the system           |
| client.disconnected  | Disconnect                  | Connection layer of client is ready to close                                                |
| client.authenticate  | Connection authentication   | After `client.connect` is executed                                                          |
| client.authorize     | Pub/Sub authorization       | Before `publish/subscribe` operation is executed                                            |
| client.subscribe     | Subscribe to topic          | After receiving the subscription message, and before executing `client.authorize`           |
| client.unsubscribe   | Unsubscribe                 | After receiving the unsubscribe packet                                                      |
| session.created      | Session creation            | When a `client.connected` is completed and a new session is created                         |
| session.subscribed   | Session subscription topics | After the subscription operation is completed                                               |
| session.unsubscribed | Session unsubscription      | After the unsubscription operation is completed                                             |
| session.resumed      | Session resume              | when `client.connected` is executed and the old session information is successfully resumed |
| session.discarded    | Session discarded           | After the session was terminated due to **discarded**                                       |
| session.takenover    | Session takenover           | After the session was terminated due to **takenover**                                       |
| session.terminated   | Session terminated          | After the session was terminated due to other reason                                        |
| message.publish      | Message published           | Before the server publishes (routes) the message                                            |
| message.delivered    | Message delivered           | Before the message is ready to be delivered to the client                                   |
| message.acked        | Message acked               | After the message ACK is received from the client                                           |
| message.dropped      | Message dropped             | After the published messages are discarded                                                  |


::: tip
- **The session is discarded:** When the client logs in with the method of  `clean session`, if the client's session already exists on the server, the old session will be discarded.
- **The Session is taken over:** When the client logs in with the method of `Reserved Session`, if the client's session already exists on the server, the old session will be taken over by the new connection
:::

### Hook and unhook

EMQX Broker provides an API for the operation of hooking and unhooking.

**Hook:**

```erlang
%% Name: name of hook(hook point), such as 'client.authenticate'
%% {Module, Function, Args}: Modules, methods, and additional parameters for callback functions
%% Priority：integar, 0 by default
emqx:hook(Name, {Module, Function, Args}, Priority).
```

After the hook is completed, the callback functions will be executed in the order of priority, or the order of hook for the same priority. All official plugin mount hooks have a priority of `0`.

**Unhook**：

```erlang
%% Name:name of hook(hook point), such as'client.authenticate'
%% {Module, Function}: Modules and methods for callback functions
emqx:unhook(Name, {Module, Function}).
```


## Callback function
The input parameters and returned value of the callback function are shown in the following table:

(For parameter data structure, see:[emqx_types.erl](https://github.com/emqx/emqx/tree/master/apps/emqx/src/emqx_types.erl))

| Name                 | input parameter                                                                                                                                                          | Returned value     |
|----------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------|--------------------|
| client.connect       | `ConnInfo`：Client connection layer parameters<br>`Props`：Properties of MQTT v5.0 connection packets                                                                    | New `Props`        |
| client.connack       | `ConnInfo`：Client connection layer parameters <br>`Rc`：returned code<br>`Props`: Properties of MQTT v5.0 connection response packets                                   | New `Props`        |
| client.connected     | `ClientInfo`:  Client information parameters<br>`ConnInfo`： Client connection layer parameters                                                                          | -                  |
| client.disconnected  | `ClientInfo`：Client information parameters<br>`ConnInfo`：Client connection layer parameters<br>`ReasonCode`：Reason code                                               | -                  |
| client.authenticate  | `ClientInfo`：Client information parameters<br>`AuthNResult`：Authentication results                                                                                     | New `AuthNResult`  |
| client.authorize     | `ClientInfo`：Client information parameters<br>`Topic`：Publish/subscribe topic<br>`PubSub`:  Publish/subscribe<br>`AuthZResult`：Authentication result                  | New `AuthZResult`  |
| client.subscribe     | `ClientInfo`：Client information parameters<br/>`Props`：Properties parameters of MQTT v5.0 subscription messages<br>`TopicFilters`：List of topics of subscription      | New `TopicFilters` |
| client.unsubscribe   | `ClientInfo`：Client information parameters<br/>`Props`：Properties parameters of MQTT v5.0 unsubscription messages<br/>`TopicFilters`：List of topics of unsubscription | New `TopicFilters` |
| session.created      | `ClientInfo`：Client information parameters<br/>`SessInfo`：Session information                                                                                          | -                  |
| session.subscribed   | `ClientInfo`：Client information parameters<br/>`Topic`：subscribed topic<br>`SubOpts`：Configuration options for subscribe operations                                   | -                  |
| session.unsubscribed | `ClientInfo`：Client information parameters<br/>`Topic`：unsubscribed topic<br/>`SubOpts`：Configuration options for unsubscribe operations                              | -                  |
| session.resumed      | `ClientInfo`：Client information parameters<br/>`SessInfo`：Session information                                                                                          | -                  |
| session.discarded    | `ClientInfo`：Client information parameters<br/>`SessInfo`：Session information                                                                                          | -                  |
| session.takenover    | `ClientInfo`：Client information parameters<br/>`SessInfo`：Session information                                                                                          |                    |
| session.terminated   | `ClientInfo`：Client information parameters<br/>`Reason`：Termination reason <br>`SessInfo`：Session information                                                         | -                  |
| message.publish      | `Message`：Message object                                                                                                                                                | New `Message`      |
| message.delivered    | `ClientInfo`：Client information parameters<br/>`Message`：Message object                                                                                                | New `Message`      |
| message.acked        | `ClientInfo`：Client information parameters<br/>`Message`：Message object                                                                                                | -                  |
| message.dropped      | `Message`：Message object<br>`By`：Dropped by<br>`Reason`：Drop reason                                                                                                   | -                  |

For the application of these hooks, see:[emqx_plugin_template](https://github.com/emqx/emqx-plugin-template)
