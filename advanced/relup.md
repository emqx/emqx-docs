---
# 标题
title: 版本热升级
# 编写日期
date: 2020-07-25 14:50:09
# 作者 Github 名称
author: terry-xiaoyu
# 关键字
keywords:
# 描述
description:
# 分类
category:
# 引用
ref: undefined
---

## 版本热升级

EMQX Broker 自 4.2.0 之后支持版本热升级。

目前 EMQX Broker 仅支持 Patch 版本（Patch 版本是版本号的第三位，又叫小版本号）的热升级。
即，目前支持 4.2.0 到 4.2.1，4.2.2，... 等的热升级，但无法热升级到 4.3.0 或者 5.0。

使用版本热升级功能，用户可以快速、安全地升级生产环境的 EMQX Broker，并避免了重启服务导致的系统可用性降低。

## 手动升级方式

1. 查看当前 EMQX Broker 的版本号。

```bash

$ emqx versions

```

2. 从 EMQX 官网下载要升级的软件包。

访问 https://www.emqx.io/downloads#broker，选择对应的版本和操作系统类型，然后选择 **"zip"** 包类型。

3. 找到 EMQX 的安装目录：

```bash

$ EMQX_ROOT_DIR=$(emqx root_dir)

$ echo ${EMQX_ROOT_DIR}
"/usr/lib/emqx"

```

4. 然后将下载的 zip 包放到 EMQX 的安装目录下的 `release` 目录下：

```bash

$ cp emqx-4.2.1.zip ${EMQX_ROOT_DIR}/release

```

5. 热升级到指定版本:

```bash

emqx upgrade 4.2.1

```

6. 查看版本:

```bash

$ emqx versions

```
