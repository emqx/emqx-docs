## 启用 PSK 验证 

PSK（Pre-Shared Key）认证是一种基于预先共享的密钥进行身份验证的认证方式。使用PSK认证，客户端和 EMQX 在建立安全连接之前必须预先共享相同的密钥。在客户端和 EMQX 之间建立 TLS 连接时以及在后续的通信中预共享密钥会对数据进行加密和解密。启用 PSK 认证后，客户端和 EMQX 可以相互验证，并在无需证书或证书颁发机构的情况下建立安全连接。

本页介绍了如何在 EMQX 中启用 PSK 验证。

1. 在任意目录下创建包含 PSK 身份和密钥的文件 `data/psk_file.txt`。

   ::: tip

   密钥支持任意字符串。

   :::

   ```bash
   # 以 PSKIdentity:SharedSecret 的格式设置认证凭证，每行一个数据
   emqx_c:BA0DB2A3-4483-45A3-A13A-91C2ADA44778
   emqx_a:A6FC9EDF-6286-4125-AAE7-658BEAE6170C
   ```

2. 在配置文件 `emqx.conf` 中添加 `psk_authentication` 配置组。

   ```bash
   psk_authentication {
     enable = true
     init_file = data/psk_file.txt
   }
   ```

3. 在配置文件 `emqx.conf` 中配置 SSL 监听器。 在 `listeners.ssl.default` 配置组中添加下列选项。

   - `ssl_options.versions`: 移除 `tlsv1.3` 版本支持, 因为 `tlsv1.3` 版本配置和 PSK 密码套件互斥。
   - `ssl_options.ciphers`: 填入 PSK 密码套件。

   ::: tip

   如果使用了 `RSA-PSK` 密码套件，则仍需要 `RSA` 证书，详情请参见 [RFC4279](https://www.rfc-editor.org/rfc/rfc4279#section-4)。

   :::

   ```bash
   listeners.ssl.default {
     acceptors = 4
     bind = 1883
     ssl_options {
       ciphers = ["RSA-PSK-AES256-GCM-SHA384","RSA-PSK-AES256-CBC-SHA384","RSA-PSK-AES128-GCM-SHA256","RSA-PSK-AES128-CBC-SHA256","RSA-PSK-AES256-CBC-SHA","RSA-PSK-AES128-CBC-SHA"]
       versions = [tlsv1.2, tlsv1.1, tlsv1]
     }
   }
   ```





