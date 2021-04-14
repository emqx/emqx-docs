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
ref: undefined
---

# Upgrade guide

## Upgrade to version 4.3

`emqx_auth_mnesia` plugin now supports rules based on both `clientid` and `username`.
Previously only one type of filter was supported, as configured in `etc/plugins/emqx_auth_mnesia.conf` file.
In order to import data from the previous EMQ X versions, it is necessary to specify the value of this parameter by passing it as a CLI option:

```bash
$ emqx_ctl data import <filename> --env '{"auth.mnesia.as":"username"}'
```

or

```bash
$ emqx_ctl data import <filename> --env '{"auth.mnesia.as":"clientid"}'
```

Or by editing the import file using the same format.
