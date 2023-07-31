# 产品路线图

## 2023 路线图

{% emqxce %}

* EMQX Dashboard 数据集成 Flow 设计 
* 新的 Webhook
* [通过 RocksDB 实现会话持久化](https://github.com/emqx/eip/blob/main/active/0023-rocksdb-message-persistence.md)
* 针对客户端的新授权机制
  * QoS 和 Retain 标志的控制权限
  * 支持 OAuth 2.0 授权
* 运维 (DevOps) 和可观测行改进
  * OpenTelemetry 和 Datadog 集成
  * MQTT 数据包的端对端跟踪
  * 提供额外的指标

{% endemqxce %}

{% emqxee %}

* Dashboard 改进
  * 数据集成 Flow 设计 
  * 支持 LDAP 认证 (MS Active Directory)
  * 支持 SAML (Okta)
* 新的 Webhook
* [通过 RocksDB 实现会话持久化](https://github.com/emqx/eip/blob/main/active/0023-rocksdb-message-persistence.md)
* 运维 (DevOps) 和可观测行改进
  * OpenTelemetry 和 Datadog 集成
  * MQTT 数据包的端对端跟踪
  * 提供额外的指标
* 规则引擎支持 SparkplugB 
* 针对客户端的新授权机制
  * QoS 和 Retain 标志的控制权限
  * 支持 OAuth 2.0 授权
* 新增的数据集成
  * Snowflake
  * AWS Kinesis
  * GCP PubSub
  * Azure EventHub
  * HStreamDB
  * SAP EventMesh
* 支持从4.4迁移至5.1的配置和数据迁移

{% endemqxee %}

## 未来版本

* 支持跨数据中心集群链接
* 多云集群
* 全球多区域地理分布式集群
* 默认使用 Elixir 发布版本
* 在后台通信中使用 QUIC 协议
* 在规则引擎中支持其他语言和外部运行时（例如 JavaScript、Python）

