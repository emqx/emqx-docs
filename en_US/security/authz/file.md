# ACL file

The built-in ACL sets rules through files, which is simple and lightweight to use. It is suitable for projects with predictable number of rules, no change or small change requirements.

ACL rules file:

```bash
etc/acl.conf
```

::: tip
The built-in ACL has the lowest priority and can be overridden by the ACL plugin. If you want to disable it, you can comment all the rules. After the rules file is changed, EMQX Broker needs to be restarted for them to take effect.
:::


## Define ACL

The file ACL is the lowest priority rule table. If it is not hit after all the ACL checks are completed, the default ACL rule is checked.

The rules file is described in Erlang syntax:

```erlang
%% Allow "dashboard" users to subscribe to "$SYS/#" topics
{allow, {user, "dashboard"}, subscribe, ["$SYS/#"]}.

%% Allow users with IP address "127.0.0.1" to publish/subscribe to topics "$SYS/#", "#"
{allow, {ipaddr, "127.0.0.1"}, pubsub, ["$SYS/#", "#"]}.

%% Deny "All Users" subscribe to "$SYS/#" "#" Topics
{deny, all, subscribe, ["$SYS/#", {eq, "#"}]}.

%% Allow any other publish/subscribe operation
{allow, all}.
```

1. The first rule allows clients to publish and subscribe to all topics
2. The second rule prohibits all clients from subscribing to the topics `$SYS/#` and `#`
3. The third rule allows clients with IP address `127.0.0.1` to publish / subscribe to the topics ` $SYS/# `and `#`, which makes a special case for the second rule
4. The fourth rule allows clients with the username `dashboard` to subscribe to the topic ` $SYS/#`, which makes a special case for the second rule

The file ACL is mainly to restrict the client's permissions on the system topic `$SYS/#` and the all wildcard topic `#`.


## acl.conf Writing rules

The rules in the `acl.conf` file are matched from top to bottom in writing order.

The syntax rules of `acl.conf` are included in the comments at the top. Those familiar with Erlang syntax can read the comments at the top of the file directly or refer to the following descriptions:

- Line comments are expressed as `%%`.
- Each rule consists of a tuple with four elements and ends with `.`.
- The first position of the tuple indicates that after the rule is successfully hit, the permission control operation is performed. The possible values are:
    * `allow`
    * `deny`
- The second position of the tuple indicates the user to which the rule takes effect. The format that can be used is:
    * `{user, "dashboard"}`：The rule only takes effect for users whose Username is dashboard
    * `{clientid, "dashboard"}`：The rule only takes effect for users whose ClientId is dashboard
    * `{ipaddr, "127.0.0.1"}`：The rule only takes effect for users whose Source Address is "127.0.0.1"
    * `all`：The rule takes effect for all users
- The third position of the tuple indicates the operation controlled by the rule with the possible value:
    * `publish`：The rule applies to PUBLISH operations
    * `subscribe`：The rule applies to SUBSCRIBE operations
    * `pubsub`：The rule applies to both PUBLISH and SUBSCRIBE operations
- The fourth position of the tuple means the list of topics restricted by the rule. The content is given in the form of an array. For example:
    * `"$SYS/#"`：a **Topic Filter** which means that the rule is applied to topics that match `$SYS/#`; for example rules created for "$SYS/#" applies to publish/subscribe actions on topic "$SYS/a/b/c", and subscribe actions on topic "$SYS/#"
    * `{eq, "#"}`：It indicates full equivalence of characters. The rule is only applied for topic `#` but not for `/a/b/c`, etc.
- In addition, there are two special rules:
    - `{allow, all}`：Allow all operations
    - `{deny, all}`：Deny all operations

After the `acl.conf` modification is completed, it will not be automatically loaded into the EMQX Broker system. Loading needs to be performed manually:

```bash
./bin/emqx_ctl modules reload emqx_mod_acl_internal
```

## Placeholders

The built-in `acl.conf` supports only the following placeholders in the subject's field (the 4th position of the tuple).

- `%c`: For Client ID, which is replaced by the client ID when the rule takes effect.
- `%u`: For username, which is replaced by the client's username when the rule takes effect.

E.g:

```erlang
{allow, all, pubsub, ["sensor/%c/ctrl"]}.
```

Means that a client with ID 'light' is **Allowed** to **Subscribe and Publish** to the `sensor/light/ctrl` topic.


::: tip
The file ACL backend is handy when there are only a small number of general rules, or a small number of username/ClientID based rules.
For large number of per username/CliendID rules, another database ACL backend is recommended as an extension or replacement of the file backend.
:::
