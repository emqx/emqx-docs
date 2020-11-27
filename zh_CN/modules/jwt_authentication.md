# JWT 认证

[JWT](https://JWT.io/) 认证是基于 Token 的鉴权机制，不依赖服务端保留客户端的认证信息或者会话信息，在持有密钥的情况下可以批量签发认证信息，是最简便的认证方式。

## 创建模块

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/modules)，点击左侧的 “模块” 选项卡，选择添加：

![image-20200928161310952](./assets/modules.png)

点击"选择",然后选择 JWT 认证模块

![image-20200928141305205](./assets/auth_jwt1.png)

需要配置JWT密钥，认证源，公钥（可选）等基本连接参数表。

![image-20200928141327836](./assets/auth_jwt2.png)

如果需要添加自定义验证字段，可参考如下配置：

![image-20200928141558866](./assets/auth_jwt3.png)

最后点击“添加”按钮模块即可添加成功。

![image-20200928141558866](./assets/auth_jwt4.png)

## 认证原理

客户端使用 Token 作为用户名或密码（取决于模块配置），发起连接时 EMQ X 使用配置中的密钥、证书进行解密，如果能成功解密则认证成功，否则认证失败。

默认配置下启用 JWT 认证后，你可以通过任意用户名+以下密码进行连接：

```bash
eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7ImF1dGhvciI6IndpdndpdiIsInNpdGUiOiJodHRwczovL3dpdndpdi5jb20ifSwiZXhwIjoxNTgyMjU1MzYwNjQyMDAwMCwiaWF0IjoxNTgyMjU1MzYwfQ.FdyAx2fYahm6h3g47m88ttyINzptzKy_speimyUcma4
```

::: tip

上述JWT Token仅做测试使用，可根据自己的业务需求用相关工具生成。此处提供一个在线生成工具：https://www.jsonwebtoken.io/。

:::

## JWT参数含义

- 密钥：JWT密钥，用来加密JWT字符串。

- 认证来源：客户端携带 JWT 的位置，用于配置客户端 JWT 字符串携带位置，可选 username 与 password。
- 公钥文件：需要上传的JWT公钥文件。
- 签名格式：默认是der格式，可选择为JWT原生格式，选项为"raw"。
