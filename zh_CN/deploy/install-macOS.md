# macOS

本章节将指导您如何通过 zip 包在 macOS 系统中下载安装并启动 EMQX。

支持的操作系统：

- macOS 12
- macOS 11

下文将以 macOS 12 系统为例演示如何下载最新版 EMQX。如希望在 macOS 11 系统中进行安装，只需将下方命令中的版本替换为对应版本即可，或前往 [EMQX 下载页面](https://www.emqx.com/zh/try?product=enterprise) 获取安装信息。

{% emqxce %}

1. 下载 [emqx-5.0.17-macos12-arm64.zip](https://www.emqx.com/zh/downloads/broker/5.0.17/emqx-5.0.17-macos12-arm64.zip)。

```bash
wget https://www.emqx.com/zh/downloads/broker/5.0.17/emqx-5.0.17-macos12-arm64.zip
```

2. 安装 EMQX。

```bash
mkdir -p emqx && unzip emqx-5.0.17-macos12-arm64.zip -d emqx
```

3. 启动

```bash
./emqx/bin/emqx start
```


{% endemqxce %}

{% emqxee %}

1. 下载 [emqx-enterprise-5.0.0-macos12-arm64.zip](https://www.emqx.com/zh/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-macos12-arm64.zip)。

```bash
wget https://www.emqx.com/zh/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-macos12-arm64.zip
```

2. 安装 EMQX。

```bash
mkdir -p emqx && unzip emqx-enterprise-5.0.0-macos12-arm64.zip -d emqx
```

3. 启动

```bash
./emqx/bin/emqx start
```

{% endemqxee %}
