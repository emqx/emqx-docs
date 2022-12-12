# PSKFile 认证

如果希望使用 PSKFile 认证，需要将 `emqx.conf` 的 `listener.ssl.external.ciphers` 注释掉，然后配置 `listener.ssl.external.psk_ciphers`：

```bash
#listener.ssl.external.ciphers = ECDHE-ECDSA-AES256-GCM-SHA384,...
listener.ssl.external.psk_ciphers = PSK-AES256-GCM-SHA384,PSK-AES128-GCM-SHA256,PSK-AES256-CBC-SHA384,PSK-AES256-CBC-SHA,PSK-AES128-CBC-SHA256,PSK-AES128-CBC-SHA,RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,RSA-PSK-RC4-SHA

```

由于 PSK 不支持 `tlsv1.3`，因此需要设置 `tls_versions` 为：
```bash
listener.ssl.external.tls_versions = tlsv1.2,tlsv1.1,tlsv1
```

**注意**:

如果使用 `RSA-PSK` 密码套件，则仍然需要 `RSA` 证书，详见 [RFC4279](https://www.rfc-editor.org/rfc/rfc4279#section-4)

## 创建模块

打开 [EMQX Dashboard](http://127.0.0.1:18083/#/modules)，点击左侧的 “模块” 选项卡，选择添加：

![image-20200927213049265](./assets/modules.png)

选择 PSK 认证

![image-20200927213049265](./assets/auth_psk1.png)

配置相关参数

![image-20200927213049265](./assets/auth_psk2.png)

点击添加后，模块添加完成

![image-20200927213049265](./assets/auth_psk3.png)


PSK 的配置文件为 `psk.txt`，使用冒号`:` 分隔 PSK ID 和 PSK：

```bash
client1:1234
client2:abcd
```
