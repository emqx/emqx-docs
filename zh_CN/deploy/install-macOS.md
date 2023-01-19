# macOS

本章节将指导您如何通过 zip 包在 macOS 系统中下载安装并启动 EMQX。

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
