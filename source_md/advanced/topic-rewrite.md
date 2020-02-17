---
# 标题
title: 主题重写
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

# 主题重写

EMQ X 的主题重写功能可以在客户端订阅主题、发布消息、取消订阅的时候将符合规则的 A 主题重写为 B 主题。

EMQ X 的主题重写功能默认关闭，开启 EMQ X 的主题重写功能需要配置 `etc/emqx.conf` 文件。
```
## Rewrite Module

## Enable Rewrite Module.
##
## Value: on | off
module.rewrite = on

## {rewrite, Topic, Re, Dest}
module.rewrite.rule.1 = x/# ^x/y/(.+)$ z/y/$1
module.rewrite.rule.2 = y/+/z/# ^y/(.+)/z/(.+)$ y/z/$2
```

每条重写规则是由 主题过滤器、正则表达式、目标表达式三部分组成的，当主题匹配规则中的主题过滤器的时候，使用正则替换将符合正则表达式的内容替换为目标表达式的内容。

`module.rewrite.rule.1 = x/# ^x/y/(.+)$ z/y/$1` 这条规则表示当主题匹配 `x/#` 主题过滤器时，执行正则表达式匹配，并把匹配出来的第一个元素带入 `z/y/$1` 中。

同理，`module.rewrite.rule.2 = y/+/z/# ^y/(.+)/z/(.+)$ y/z/$2` 这条规则表示当主题匹配 `x/#` 主题过滤器时，执行正则表达式匹配，并把匹配出来的第二个元素带入 `y/z/$2` 中。

举个例子:

分别订阅 `x/y/2`、`x/1/2`、`y/a/z/b`、`y/def` 四个主题

+ `x/y/2` 匹配 `x/#` 主题过滤器，执行正则匹配，将匹配出来的元素 "2" 带入 `z/y/$1` 中，实际订阅了 `z/y/2` 主题。
+ `x/1/2` 匹配 `x/#` 主题过滤器，执行正则匹配，未匹配到元素，不执行主题重写，实际订阅 `x/1/2` 主题。
+ `y/a/z/b` 匹配 `y/+/z/#` 主题过滤器，执行正则匹配，匹配出元素 “[a、b]” ，将匹配出来的第二个元素带入 `y/z/$2`，实际订阅了 `y/z/b` 主题。
+ `y/def` 不匹配任何一个主题过滤器，不执行主题重写，实际订阅 `y/def` 主题。
