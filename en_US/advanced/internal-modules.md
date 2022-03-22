---
# 编写日期
date: 2022-03-21 17:15:26
# 作者 Github 名称
author: tigercl
# 关键字
keywords:
# 描述s
description:
# 分类
category: 
# 引用
ref:
---

# Internal Modules

EMQX provides features such as topic rewrit and proxy subscriptions in the form of internal modules, allowing users to start and stop modules at any time to start and stop corresponding features. The internal modules currently support the following features:

| Module Name              | Feature                                |
| ------------------------ | -------------------------------------- |
| `emqx_mod_delayed`       | [Delay Publish](./delay-publish.md)         |
| `emqx_mod_topic_metrics` | [Topic Metrics](./metrics-and-stats.md) |
| `emqx_mod_subscription`  | [Proxy Subscriptions](./proxy-subscriptions.md)    |
| `emqx_mod_acl_internal`  | [Internal ACL](./acl-file.md)                |
| `emqx_mod_rewrite`       | [Topic Rewrite](./topic-rewrite.md)         |
| `emqx_mod_presence`      | [Client Online and Offline Events](./system-topic.md)  |

EMQX provides [Command Line Interface](./cli.md#endpoint-modules) and [HTTP API](./http-api.md#endpoint-modules) for internal modules, users can easily pass these interfaces to start and stop modules, for example:

```bash
$ ./emqx_ctl modules load emqx_mod_delayed
Module emqx_mod_delayed loaded successfully.
```

```bash
$ curl -i --basic -u admin:public -X PUT "http://localhost:8081/api/v4/nodes/emqx@127.0.0.1/modules/emqx_mod_delayed/load"

{"code":0}
```

Of course, users can also complete these operations on the Dashboard, including viewing module status, which is also more commonly used.

By default, EMQX will start the two modules `emqx_mod_acl_internal` and `emqx_mod_presence`, that is, the internal ACL and client online and offline notification features are enabled by default. Users can modify the `loaded_modules` file in the EMQX `data` directory to change the default startup modules.

***Does not start by default***

```erlang
{emqx_mod_rewrite, false}.
```

***Start by default***

```erlang
{emqx_mod_rewrite, true}.
```
