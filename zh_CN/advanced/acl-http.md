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
ref:
---

# HTTP ACL

HTTP 认证使用外部自建 HTTP 应用认证授权数据源，根据 HTTP API 返回的数据判定授权结果，能够实现复杂的 ACL 校验逻辑。

插件：

```bash
emqx_auth_http
```

::: tip 
emqx_auth_http 插件同时包含认证功能，可通过注释禁用。
:::


要启用 HTTP ACL，需要在 `etc/plugins/emqx_auth_http.conf` 中配置以下内容：

## ACL 授权原理

EMQ X 在设备发布、订阅事件中使用当前客户端相关信息作为参数，向用户自定义的认证服务发起请求权限，通过返回的 HTTP **响应状态码** (HTTP statusCode) 来处理 ACL 授权请求。

 - 无权限：API 返回 4xx 状态码
 - 授权成功：API 返回 200 状态码
 - 忽略授权：API 返回 200 状态码且消息体 ignore

## HTTP 请求信息

HTTP API 基础请求信息，配置证书、请求头与重试规则。

```bash
# etc/plugins/emqx_auth_http.conf

## 启用 HTTPS 所需证书信息
## auth.http.ssl.cacertfile = etc/certs/ca.pem

## auth.http.ssl.certfile = etc/certs/client-cert.pem

## auth.http.ssl.keyfile = etc/certs/client-key.pem

## 请求头设置
## auth.http.header.Accept = */*

## 重试设置
auth.http.request.retry_times = 3

auth.http.request.retry_interval = 1s

auth.http.request.retry_backoff = 2.0
```

进行发布、订阅认证时，EMQ X 将使用当前客户端信息填充并发起用户配置的 ACL 授权查询请求，查询出该客户端在 HTTP 服务器端的授权数据。

## superuser 请求

首先查询客户端是否为超级用户，客户端为超级用户时将跳过 ACL 查询。

```bash
# etc/plugins/emqx_auth_http.conf

## 请求地址
auth.http.super_req = http://127.0.0.1:8991/mqtt/superuser

## HTTP 请求方法
## Value: post | get | put
auth.http.super_req.method = post

## 请求参数
auth.http.super_req.params = clientid=%c,username=%u
```


## ACL 授权查询请求

```bash
# etc/plugins/emqx_auth_http.conf

## 请求地址
auth.http.acl_req = http://127.0.0.1:8991/mqtt/acl

## HTTP 请求方法
## Value: post | get | put
auth.http.acl_req.method = get

## 请求参数
auth.http.acl_req.params = access=%A,username=%u,clientid=%c,ipaddr=%a,topic=%t,mountpoint=%m

```

## 请求说明

HTTP 请求方法为 GET 时，请求参数将以 URL 查询字符串的形式传递；POST、PUT 请求则将请求参数以普通表单形式提交（content-type 为 x-www-form-urlencoded）。

你可以在认证请求中使用以下占位符，请求时 EMQ X 将自动填充为客户端信息：

- %A：操作类型，'1' 订阅；'2' 发布
- %u：客户端用户名
- %c：Client ID
- %a：客户端 IP 地址
- %r：客户端接入协议
- %m：Mountpoint
- %t：主题

::: danger 
推荐使用 POST 与 PUT 方法，使用 GET 方法时明文密码可能会随 URL 被记录到传输过程中的服务器日志中。
:::
