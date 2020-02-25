---
# 标题
title: 内置 ACL 
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
ref: undefined
---

# 内置 ACL

内置 ACL 通过文件设置规则，使用上足够简单轻量，适用于规则数量可预测、无变动需求或变动较小的项目。

ACL 规则文件：

```bash
etc/acl.conf
```

{% hint style="info" %}
内置 ACL 优先级最低，可以被 ACL 插件覆盖，如需禁用全部注释即可。规则文件更改后需重启 EMQ X 以应用生效。
{% endhint %}

## 设置 ACL

内置 ACL 使用 Erlang 语法进行规则定义，规则**从下到上依次覆盖**，定义方式为：

```bash
[权限：允许|拒绝] [对象：IP 地址|用户名|Client ID] [动作：发布|订阅|发布订阅] [目标：主题]
```

默认规则文件：`etc/acl.conf`：

```bash
# etc/acl.conf

{allow, {user, "dashboard"}, subscribe, ["$SYS/#"]}.

{allow, {ipaddr, "127.0.0.1"}, pubsub, ["$SYS/#", "#"]}.

{deny, all, subscribe, ["$SYS/#", {eq, "#"}]}.

{allow, all}.
```

EMQ X 启动时将读取 ACL 文件并将数据加载到内存中，节点上的 ACL 数据会在此阶段同步至集群。

你可以在**目标**主题中使用以下占位符，请求时 EMQ X 将自动填充为客户端信息：

- %u：用户名
- %c：Client ID
- %a：客户端 IP 地址
