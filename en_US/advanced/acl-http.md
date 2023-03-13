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

# HTTP ACL

An external self-built HTTP application authentication data source is used for HTTP authentication, and the authentication result is judged based on the data returned by the HTTP API, which can implement complex ACL verification logic.

Plugin:

```bash
emqx_auth_http
```

::: tip 
The emqx_auth_http plugin also includes authentication function, which can be disabled via comments.
:::

To enable HTTP ACL, the following needs to be configured in `etc/plugins/emqx_auth_http.conf`:

## ACL Authentication principle

EMQX Broker uses the current client related information as parameters in publish/subscribe events, initiates request permissions to user-defined authentication services, and processes ACL authentication requests through the returned HTTP statusCode .

 - Authorization denied: The API returns status codes other than 200
 - Authorization succeeded: API returns 200 status code
 - Authorization ignored: API returns 200 status code with the message body of ignore

## HTTP Request Information

Basic request information, configure certificates, request headers, and retry rules of HTTP API.

```bash
# etc/plugins/emqx_auth_http.conf

## Certificate information required to enable HTTPS
## auth.http.ssl.cacertfile = etc/certs/ca.pem

## auth.http.ssl.certfile = etc/certs/client-cert.pem

## auth.http.ssl.keyfile = etc/certs/client-key.pem

## Request header setup
## auth.http.header.Accept = */*

## Retry setup
auth.http.request.retry_times = 3

auth.http.request.retry_interval = 1s

auth.http.request.retry_backoff = 2.0
```

When performing publish/subscribe authentication, EMQX Broker will use the current client information and initiate a user-configured ACL authorization query request to query the client's authorization data on the HTTP server.

## superuser Request

Check whether the client is a super user at first. If the client is a super user, the ACL query will be skipped.

```bash
# etc/plugins/emqx_auth_http.conf

## Request address
auth.http.super_req = http://127.0.0.1:8991/mqtt/superuser

## HTTP request method
## Value: post | get | put
auth.http.super_req.method = post

## Request parameter
auth.http.super_req.params = clientid=%c,username=%u
```


## ACL authorization query request

```bash
# etc/plugins/emqx_auth_http.conf

## Request address
auth.http.acl_req = http://127.0.0.1:8991/mqtt/acl

## HTTP request method
## Value: post | get | put
auth.http.acl_req.method = get

## Request parameter
auth.http.acl_req.params = access=%A,username=%u,clientid=%c,ipaddr=%a,topic=%t,mountpoint=%m

```

## Request description

When the HTTP request method is GET, the request parameters will be passed in the form of a URL query string; POST and PUT requests will submit the request parameters in the form of a common form (content-type is x-www-form-urlencoded).

You can use the following placeholders in the authentication request, and EMQX Broker will be automatically populated with client information when requested:

- %u:User name
- %c:Client ID
- %a:Client IP address
- %r:Client Access Protocol
- %P:Clear text password
- %p:Client Port
- %C:TLS certificate common name (the domain name or subdomain name of the certificate), valid only for TLS connections
- %d:TLS certificate subject, valid only for TLS connections

::: 
The POST and PUT methods are recommended. When using the GET method, the clear text password may be recorded with the URL in the server log during transmission.

:::