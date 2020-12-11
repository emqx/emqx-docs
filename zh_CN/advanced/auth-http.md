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

# HTTP 认证

HTTP 认证使用外部自建 HTTP 应用认证数据源，根据 HTTP API 返回的数据判定认证结果，能够实现复杂的认证鉴权逻辑。

插件：

```bash
emqx_auth_http
```

::: tip 
emqx_auth_http 插件同时包含 ACL 功能，可通过注释禁用。
:::


## 认证原理

EMQ X 在设备连接事件中使用当前客户端相关信息作为参数，向用户自定义的认证服务发起请求查询权限，通过返回的 HTTP **响应状态码** (HTTP statusCode) 来处理认证请求。

 - 认证失败：API 返回 4xx 状态码
 - 认证成功：API 返回 200 状态码
 - 忽略认证：API 返回 200 状态码且消息体 ignore

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


## 加盐规则与哈希方法

HTTP 在请求中传递明文密码，加盐规则与哈希方法取决于 HTTP 应用。


## 认证请求

进行身份认证时，EMQ X 将使用当前客户端信息填充并发起用户配置的认证查询请求，查询出该客户端在 HTTP 服务器端的认证数据。

```bash
# etc/plugins/emqx_auth_http.conf

## 请求地址
auth.http.auth_req = http://127.0.0.1:8991/mqtt/auth

## HTTP 请求方法
## Value: post | get | put
auth.http.auth_req.method = post

## 请求参数
auth.http.auth_req.params = clientid=%c,username=%u,password=%P
```

HTTP 请求方法为 GET 时，请求参数将以 URL 查询字符串的形式传递；POST、PUT 请求则将请求参数以普通表单形式提交（content-type 为 x-www-form-urlencoded）。

你可以在认证请求中使用以下占位符，请求时 EMQ X 将自动填充为客户端信息：

- %u：用户名
- %c：Client ID
- %a：客户端 IP 地址
- %r：客户端接入协议
- %P：明文密码
- %p：客户端端口
- %C：TLS 证书公用名（证书的域名或子域名），仅当 TLS 连接时有效
- %d：TLS 证书 subject，仅当 TLS 连接时有效

::: danger 
推荐使用 POST 与 PUT 方法，使用 GET 方法时明文密码可能会随 URL 被记录到传输过程中的服务器日志中。
:::

