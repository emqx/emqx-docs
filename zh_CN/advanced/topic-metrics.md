---
# 编写日期
date: 2020-09-30 09:45:01
# 作者 Github 名称
author: tigercl
# 关键字
keywords:
# 描述
description:
# 分类
category: 
# 引用
ref:
---

# 主题指标统计

EMQX Broker 提供了主题指标统计功能，可以统计指定主题下的消息收发数量、速率等指标。您可以通过 Dashboard 的 Topic Metrics 页面查看和使用这一功能，也可以通过 HTTP API 完成相应操作。

## Dashboard

Topic Metrics 页面位于 Dashboard 的 Analysis 标签下，由于主题指标统计功能以插件形式实现，且默认关闭，因此首次使用时需要点击 Topic Metrics 页面右上角的 *Enable* 按钮以开启此功能。

![image-20200930095122959](./assets/image-20200930095122959.png)

主题指标统计功能开启后，你可以点击页面右上角的 *Create* 按钮来创建新的主题指标统计。以下是已经创建了 `a/c` 与 `a/b` 主题指标统计之后的页面，你将可以看到这两个主题下消息流入流出、丢弃的总数和当前速率。

![image-20200930110511638](./assets/image-20200930110511638.png)

> 出于整体性能考虑，目前主题指标统计功能仅支持主题名，即不支持带有 `+` 或 `#` 通配符的主题过滤器，例如 `a/+` 等。也许将来有一天我们会实现它，如果我们解决了性能问题。

## HTTP API

我们为您提供了与 Dashboard 操作一致的 HTTP API，以便您与自己的应用进行集成。相关 HTTP API 的具体使用方法，请参见 [HTTP API - 主题指标统计](./http-api.md#主题统计指标)。