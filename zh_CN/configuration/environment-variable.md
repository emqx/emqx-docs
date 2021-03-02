---
# 编写日期
date: 2021-03-02 17:19:10
# 作者 Github 名称
author: zmstone
# 关键字
keywords:
# 描述
description:
# 分类
category:
# 引用
ref: undefined
---

# 使用环境变量修改配置

默认情况下 EMQ X 使用带有 `EMQX_` 的前缀的环境变量来覆盖配置文件中的配置项

环境变量名称到配置文件键值名称映射规则如下:

- 将 ``EMQX_`` 前缀移除
- 大写字符替换成小写
- 双下划线 ``__`` 替换成点 ``.``


## 使用默认环境变量前缀

```bash
# management.listener.http = 9000
$ export EMQX_MANAGEMENT__LISTENER__HTTP=9000
$ _build/emqx/rel/emqx/bin/emqx console

...

Starting emqx on node emqx@127.0.0.1
Start http:management listener on 9000 successfully.
```

## 自定义环境变量名前缀

Set `CUTTLEFISH_ENV_OVERRIDE_PREFIX` to alter the mapping prefix.

```bash
$ export CUTTLEFISH_ENV_OVERRIDE_PREFIX=DEV_
$ export DEV_MANAGEMENT__LISTENER__HTTP=9001
$ _build/emqx/rel/emqx/bin/emqx console

...

Starting emqx on node emqx@127.0.0.1
Start http:management listener on 9001 successfully.
```

::: tip Tip
环境变量只在运行 EMQ X 的终端中有效，且没有持久化,
在一个终端中设置的环境变量无法在另一个终端中使用。
:::
