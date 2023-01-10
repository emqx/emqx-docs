# Amazon Linux

本章节将指导您如何在 Amazon Linux 系统中下载安装并启动 EMQX。

## tag.gz 安装

1. 下载 [emqx-5.0.13-elixir-amzn2-amd64.tar.gz](https://www.emqx.com/downloads/broker/v5.0.13/emqx-5.0.13-elixir-amzn2-amd64.tar.gz)

```bash
wget https://www.emqx.com/downloads/broker/v5.0.13/emqx-5.0.13-elixir-amzn2-amd64.tar.gz
```

1. 解压程序包

```bash
mkdir -p emqx && tar -zxvf emqx-5.0.13-elixir-amzn2-amd64.tar.gz -C emqx
```

3. 启动

```bash
cd emqx && ./bin/emqx start
```
