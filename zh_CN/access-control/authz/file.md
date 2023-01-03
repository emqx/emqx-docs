# File

文件 Authorizer 通过为每个发布或订阅请求扫描文件中定义的一系列规则来确定该请求是否有权限执行想要的操作。

::: tip Tip
从 5.0 开始，文件内容的更新可以在仪表盘的界面上直接操作。
:::

文件 Authorizer 简单且轻量。非常适用于定义通用的规则。如果规则数量庞大，例如成百上千条，那么建议使用其他带数据库后端的 Authorizer。
通常，与其他 Authorizer 配合使用时，文件 Authorizer 可以作为最后一道防线，放在授权链的最后面。

## 配置

文件 Authorizer 在配置文件中，由 `type = file` 这个字段来识别和定位。

例子：

```
authorization {
  deny_action = ignore
  no_match = allow
  sources = [
    {
      type = file
      enable = true
      path = "etc/acl.conf"
    }
  ]
}
```

::: warning Warning
`path` 字段指定的文件对于EMQX是只读的。
如果通过仪表盘或 REST API 对规则进行过修改，那么 EMQX 会把新的文件保存到 `data/authz/acl.conf`，
并且以后不再读取原始配置中的文件。
:::

## 文件格式

ACL 文件的内容，是一个 Erlang[元组](https://www.erlang.org/doc/reference_manual/data_types.html#tuple)数据的列表。
（注意每条规则后面必需有一个点号 `.`）。
元组是用花括号包起来的一个列表，各个元素用逗号分隔。

以百分号开始的 `%` 行是注释，在解析过程中会被丢弃。

例如

```erlang
%% 允许用户名是 dashboard 的客户端订阅 "$SYS/#" 这个主题
{allow, {user, "dashboard"}, subscribe, ["$SYS/#"]}.

%% 允许来自127.0.0.1 的用户发布和订阅 "$SYS/#" 以及 "#"
{allow, {ipaddr, "127.0.0.1"}, pubsub, ["$SYS/#", "#"]}.

%% 拒绝其他所有用户订阅 "$SYS/#" 和 "#" 主题
{deny, all, subscribe, ["$SYS/#", {eq, "#"}]}.

%% 前面的规则都没有匹配到的话，允许所有操作
{allow, all}.
```

规则的匹配遵循自顶向下的顺序。当一个规则匹配到当前客户端时，规则允许或拒绝的动作就会生效，后面的规则不再继续检查。

- 规则元组的第一个元素表示匹配到该规则时应执行的相应动作，可以是：
    * `allow`
    * `deny`
- 第二个元素指定对客户端的匹配规则。下面使用了一些例子来列举所有可以用在这里的匹配规则：
    * `{username, "dashboard"}` — 用户名是 `dashboard` 的客户端.
    * `{username, {re, "^dash"}}` — 用户名匹配到[正则表达式](https://www.erlang.org/doc/man/re.html#regexp_syntax) `^dash` 的客户的。
    * `{user, ...}` — 跟 `{username, ...}` 一样。
    * `{clientid, "dashboard"}` — ID 为 `dashboard` 的客户端。
    * `{clientid, {re, "^dash"}}` — ID 匹配到[正则表达式](https://www.erlang.org/doc/man/re.html#regexp_syntax) `^dash` 的客户端。
    * `{client, ...}` — 跟 `{clientid, ...}`一样。
    * `{ipaddr, "127.0.0.1"}` — 源 IP 地址是 `127.0.0.1` 的客户端。也可以使用 CIDR 地址格式。注意，如果 EMQX 部署在负载均衡器后面，建议为 EMQX 的监听器开启 `proxy_protocol` 配置，否则 EMQX 可能会使用负载均衡器的源地址。
    * `{ipaddrs, ["127.0.0.1", ..., ]}` — 同上，但是允许匹配多个源地址。
    * `all` — 匹配所有客户端.
    * `{'and', First, Second}` — ‘与’操作，同时能够匹配 `First` 和 `Second` 两个规则。
    * `{'or', First, Second}` — ‘或’操作，能够匹配 `First` 或 `Second` 的其中一个即可。
- 元组的第三个元素用来匹配客户端当前想要执行的动作请求。
    * `publish` — 当客户端想要发布一个消息。
    * `subscribe` — 当客户端想要订阅一个主题。
    * `all` — 任意，即：当客户端想要发布或者订阅时。
- 元组的第四个元素用于指定当前规则适用的 MQTT 主题或主题过滤器。
    * 字符串 `"$SYS/#"`。这是一个通用的主题过滤器（或通配符订阅），主题的匹配跟 [MQTT协议](http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/errata01/os/mqtt-v3.1.1-errata01-os-complete.html#_Toc442180920)规范的一致。例如，`$SYS/#` 可以为发布动作匹配到 `$SYS/foo` 和 `$SYS/foo/bar`，也可以为订阅动作匹配到
    `$SYS/foo`，`$SYS/foo/#`，和`$SYS/#`。也可以使用[主题占位符](./authz.md#主题占位符)。
    * 主题过滤器与配合 `eq` 字段构成一个元祖，例如 `{eq, "foo/#"}`，可用于表示不将该主题当作通配符来使用。该例子中，如果一个客户端订阅 `foo/#` 这个主题，这该规则匹配，而如果订阅的是主题是 `foo/bar` 匹配不到。

另外还有 2 种特殊的规则：
    - `{allow, all}` — 允许所有请求。
    - `{deny, all}` — 拒绝所有请求。
这些规则通常用在ACL文件的末尾作为默认规则使用。
