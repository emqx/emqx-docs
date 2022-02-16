---
# 编写日期
date: 2021-03-02 17:19:10
# 作者 Github 名称
author: z8674558
# 关键字
keywords:
# 描述
description:
# 分类
category: 
# 引用
ref: undefined
---

# Configuration from environment variable

By default, EMQX maps environment variables with prefix ``EMQX_``
to key-value pairs in configuration files.

Mapping rules from environment variable name to config key

- Prefix ``EMQX_`` is removed
- Upper case letters are mapped to lower case letters
- Double underscore ``__`` is mapped to ``.``

## Examples

```bash
# management.listener.http = 9000
$ export EMQX_MANAGEMENT__LISTENER__HTTP=9000
$ _build/emqx/rel/emqx/bin/emqx console

...

Starting emqx on node emqx@127.0.0.1
Start http:management listener on 9000 successfully.
```

::: tip Tip
Configuration values from environment variables do not persist.
After a restart from other shell, previous values do not have any effect.
:::
