# 基于 MQTT 的文件传输

EMQX 的 MQTT 文件传输功能使 MQTT 客户端设备具备了将大文件通过 MQTT 协议传输到 EMQX 的能力。这个功能在需要传输文件的物联网应用中非常有用，例如从智能摄像头流式传输视频，将车辆日志文件发送到分析服务器，或将仓库机器人捕获的图像传输到云服务器进行审核。MQTT 协议在用于其他操作的同时也可用于传输文件，于是客户端设备无需再使用 HTTP 或 FTP 等其他协议。

## 传统方式的缺点

在引入 MQTT 文件传输之前，物联网设备必须应用 HTTP 或 FTP 等其他协议来传输文件。然而，HTTP 或 FTP 协议无法支持大规模连接，多个连接通道也不易于设备端的开发和操作。这种传统方法还有其他一些缺点，包括需要实现附加协议及其相关的安全性和身份验证机制。在客户端设备上管理复杂状态并维护处理文件传输的独立服务进一步增加了挑战。

## EMQX 中的 MQTT 文件传输

MQTT 规范没有定义传输文件的标准方式。而 EMQX 扩展了 MQTT 协议、有助于客户端设备进行高效且安全的文件传输。它定义并实现了一个建于 MQTT 之上的简单应用级协议，使客户端设备能简单得处理文件传输。

### 主要特点

以下特点是 MQTT 文件传输功能的亮点：

- 整个文件传输过程都在 MQTT 上进行，使用与其他 MQTT 传输相同的连接。无需额外的连接。

- 文件传输是分块进行的，这样可以避免消耗过多内存，使得即使在轻量级客户端上也可以执行文件传输。客户端设备可以将文件以小块方式发送，EMQX 将会将这些块重新组装成原始文件。

- 文件传输是可恢复的，客户端设备可以从中断的地方恢复传输。

- 文件传输的命令和完成过程被设计为幂等的。这意味着客户端设备可以安全地重试块传输或完成命令，而不必担心在 EMQX 上创建重复的文件。

- 文件传输允许将上传的文件导出到 EMQX 上的专用本地目录中或导出到兼容 S3 对象存储中。

  <!--目前尚未实现在 Dashboard 上列出或下载上传文件。-->