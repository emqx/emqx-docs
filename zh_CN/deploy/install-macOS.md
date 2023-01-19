# macOS

本章节将指导您如何通过 zip 包在 macOS 系统中下载安装并启动 EMQX。

{%emqxee%}
:::tip
此处是 EMQX 开源版安装文档。EMQX 企业版安装文档正在准备中，在此之前请按照企业版[下载页面](https://www.emqx.com/zh/try?product=enterprise)的指引进行安装操作。
:::
{%endemqxee%}

:::: tabs type:card
::: tab Intel Chip

1. 下载 [emqx-5.0.14-macos11-amd64.zip](https://www.emqx.com/downloads/broker/v5.0.14/emqx-5.0.14-macos11-amd64.zip)

```bash
wget https://www.emqx.com/downloads/broker/v5.0.14/emqx-5.0.14-macos11-amd64.zip
```

2. 解压程序包

```bash
unzip emqx-5.0.14-macos11-amd64.zip
```

3. 启动

```bash
cd emqx && ./bin/emqx start
```

:::

::: tab Apple Silicon

1. 下载 [emqx-5.0.14-macos12-arm64.zip](https://www.emqx.com/downloads/broker/v5.0.14/emqx-5.0.14-macos12-arm64.zip)

```bash
wget https://www.emqx.com/en/downloads/broker/v5.0.14/emqx-5.0.14-macos12-arm64.zip
```

2. 解压程序包

```bash
unzip emqx-5.0.14-macos12-arm64.zip
```

3. 启动

```bash
cd emqx && ./bin/emqx start
```

:::
::::
