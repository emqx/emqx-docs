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

# HTTP

HTTP authentication uses an external self-built HTTP application authentication data source, and determines the authentication result based on the data returned by the HTTP API, which can implement complex authentication logic.

Plugin:

```bash
emqx_auth_http
```

::: tip 
The emqx_auth_http plugin also includes ACL feature, which can be disabled via comments.
:::


## Authentication principle

EMQ X Broker uses the current client related information as a parameter in the device connection event, initiates a request query permission to the user-defined authentication service, and processes the authentication request through the returned HTTP **statusCode**.

 - Authentication failed: API returns status code of 4xx
 - Authentication succeeded: API returns status code of 200
 - Authentication ignored : API returns status code of 200 with message body of ignore

## Salting rules and hash methods

HTTP passes a clear text password in the request. The salting rules and hash method depend on the HTTP application.

## Authentication request

During authentication, EMQ X Broker will use the current client information to populate and initiate a user-configured authentication query request to query the client's authentication data on the HTTP server.

```bash
# etc/plugins/emqx_auth_http.conf

## Request address
auth.http.auth_req = http://127.0.0.1:80/mqtt/auth

## HTTP request method
## Value: post | get | put
auth.http.auth_req.method = post

## HTTP Request Headers for Auth Request, Content-Type header is configured by default.
## The possible values of the Content-Type header: application/x-www-form-urlencoded, application/json
auth.http.auth_req.headers.content-type = application/x-www-form-urlencoded

## Request parameter
auth.http.auth_req.params = clientid=%c,username=%u,password=%P
```

When the HTTP request method is GET, the request parameters will be passed in the form of a URL query string; Under POST and PUT requests, it will submit the request parameters in the form of Json or ordinary form (determined by the value of content-type).

You can use the following placeholders in the authentication request, and EMQ X Broker will be automatically populated with client information when requested:

- %u：Username
- %c：Client ID
- %a：Client IP address
- %r：Client Access Protocol
- %P：Clear text password
- %p：Client port
- %C：TLS certificate common name (the domain name or subdomain name of the certificate), valid only for TLS connections
- %d：TLS certificate subject, valid only for TLS connections

::: danger 
The POST and PUT methods are recommended. When using the GET method, the clear text password may be recorded with the URL in the server log during transmission.
:::

