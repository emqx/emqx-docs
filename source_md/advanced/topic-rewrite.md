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

EMQ X 的主题重写功能可以将符合规则的 A 主题重写为 B 主题

开启 EMQ X 的主题重写功能需要配置 `etc/emqx.conf` 文件.
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

`module.rewrite.rule.1 = x/# ^x/y/(.+)$ z/y/$1` 的意思是如果订阅的主题匹配 `x/#` 主题过滤器, 那么就执行正则替换把 `^x/y/(.+)$` 替换为 `z/y/$1`

同理, `module.rewrite.rule.2 = y/+/z/# ^y/(.+)/z/(.+)$ y/z/$2` 的意思是如果订阅的主题匹配 `y/+/z/#` 主题过滤器, 那么就执行正则替换把 `^y/(.+)/z/(.+)$` 替换为 `y/z/$2`

举个例子:

分别订阅 `x/y/2`、`x/1/2`、`y/a/z/b`、`y/def` 四个主题

+ `x/y/2` 匹配 `x/#` 主题过滤器, 执行正则替换把 `^x/y/(.+)$` 替换为 `z/y/$1`, 实际订阅了 `z/y/2` 主题
+ `x/1/2` 匹配 `x/#` 主题过滤器, 不符合正则过滤规则, 不执行主题重写,实际订阅 `x/1/2` 主题
+ `y/a/z/b` 匹配 `y/+/z/#` 主题过滤器, 执行正则替换把 `^y/(.+)/z/(.+)$` 替换为 `y/z/$2`, 实际订阅了 `y/z/b` 主题
+ `y/def` 不匹配任何一个主题过滤器, 实际订阅 `y/def` 主题
