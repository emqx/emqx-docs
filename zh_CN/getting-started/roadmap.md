# 产品路线图

## 2024 路线图

- **会话持久化**：基于 RocksDB 的内置高可用会话持久化功能
- **客户端属性**：为每个 MQTT 客户端设置额外的属性，用于认证授权、数据集成和 MQTT 扩展功能中
- **Kerberos 认证**：客户端接入支持 Kerberos 认证
- **消息队列**：使用持久化队列实现生产/消费模型的消息队列功能
- **Schema 验证**：使用 Avro, Protobuf 和 JSON Schema 验证消息是否符合预期
- **消息转换**：转换消息格式并丰富和重组消息内容
- **规则引擎 Debug 和 Tracing**：规则和数据集成端到端的测试与追踪
- **更灵活的规则引擎**：
  - 错误处理动作
  - 备用动作
  - 条件动作
- **全球多区域地理分布式集群**：将集群部署在不同区域中
- **集群连接（Cluster Linking）**：不同区域中的集群可以连接成一个统一命名空间的联合集群，实现消息复制
- **MQTT Stream**：将发布的消息持久存储到内置流中，供其他服务消费
- **热升级与热补丁**：在 Dashboard 上进行增量升级与补丁安装
- **OIDC SSO**：Dashboard 支持 OIDC SSO
- **多租户**：实现统一命名空间的集群复用功能
- **更多数据集成**
  - Elasticsearch 数据集成
  - Amazon S3 数据集成
  - Azure Blob Storage 数据集成
  - CouchbaseDB 数据集成
  - Snowflake 数据集成
  - 消息桥接支持 SysKeeper 网闸穿透

## 未来版本

- 默认使用 Elixir 发布版本
- EMQX 节点角色拆分
- 在集群通信中使用 QUIC 协议
- 在规则引擎中支持其他语言和外部运行时（例如 JavaScript、Python）
