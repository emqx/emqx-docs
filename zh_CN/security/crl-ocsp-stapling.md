# CRL/OCSP Stapling

EMQX 内置了 SSL/TLS 功能，您可以使用 X.509 证书实现客户端接入认证与通信安全加密。

X.509 证书尚未到期前，如果出现私钥泄漏或证书信息有误的情况，您需要将其吊销以确保您的设备不被非法利用，此时您可以使用 CRL 或 OCSP Stapling 功能，以此实现更高级别的安全保障。

## CRL

CRL（Certificate Revocation List，证书吊销列表）是由 CA 机构维护的一个列表，列表中包含已经被吊销的证书序列号和吊销时间。

EMQX 允许配置 CA 的请求端点并定时刷新获取 CRL，并存储在 EMQX 节点上，与浏览器使用 CRL 的方式不同，我们将 MQTT 客户端设计为无需维护 CRL 文件，仅在连接握手时通过 EMQX 对比验证即可完成证书有效性验证。

包括刷新获取 CRL 在内的所有操作都是在 EMQX 内部完成，无需使用额外的定时脚本或对客户端做任何改造。

相关配置参照 [启用 CRL](../configuration/configuration.md#listener-ssl-external-enable-crl-check)。

## OCSP Stapling

OCSP（Online Certificate Status Protocol，在线证书状态协议）是另外一个证书吊销方案，OCSP Stapling 是对 OCSP 技术的最新改进。

启用 OCSP Stapling 后，EMQX 将自行从 OCSP 服务器查询证书并缓存响应结果，当客户端向 EMQX 发起 SSL 握手请求时，EMQX 将证书的 OCSP 信息随证书链一同发送给客户端（Stapling），由客户端对证书有效性进行验证。

相关配置参照 [启用 OCSP Stapling](../configuration/configuration.md#listener-ssl-external-enable-ocsp-stapling)。
