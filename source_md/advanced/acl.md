---
# 标题
title: 发布订阅 ACL
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

# 发布订阅 ACL

EMQ X 默认开启 ACL 白名单，允许不在 ACL 列表中的发布订阅行为，具体配置在 `etc/emqx.conf` 中：

```bash
## ACL nomatch
mqtt.acl_nomatch = allow

## Default ACL File
## etc/acl.conf 文件中配置了基础的 ACL 规则
mqtt.acl_file = etc/acl.conf
```

ACL 访问控制规则定义规则如下:

```bash
允许 (Allow)|拒绝 (Deny)  谁(Who)  订阅 (Subscribe)|发布 (Publish)   主题列表 (Topics)
```

EMQ X  接收到 MQTT 客户端发布 (PUBLISH) 或订阅 (SUBSCRIBE) 请求时，会逐条匹配 ACL 访问控制规则，直到匹配成功返回 allow 或 deny。

- ACL 可以设置超级用户，如果是超级用户客户端，可以进行任意发布 / 订阅操作
- ACL 控制与认证用的是同一个配置文件``plugins/emqx_auth_xxx.conf``，但并不是所有的插件都支持 ACL。

## ACL 缓存

```bash
## 是否缓存 ACL 规则，设定了缓存之后，可以加快获取 ACL 记录的速度
mqtt.cache_acl = true
```

ACL 规则在命中后，会在内存中有缓存，避免下次需要验证 ACL 的时候访问外部存储设备，加快访问的速度。ACL 在内存中的缓存只有在连接建立和存在的时间段内有效，如果连接断开，该连接对应的 ACL 信息会被删除；用户可以通过 EMQ X 提供的 REST API 来删除 ACL 信息。

```json
  {
        "name": "clean_acl_cache",
        "method": "DELETE",
        "path": "/connections/:clientid/acl/:topic",
        "descr": "Clean ACL cache of a connection"
  }
```