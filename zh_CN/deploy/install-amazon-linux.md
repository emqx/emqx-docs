# Amazon Linux

本章节将指导您如何在 Amazon Linux 系统中下载安装并启动 EMQX。

{%emqxee%}
:::tip
此处是 EMQX 开源版安装文档。EMQX 企业版安装文档正在准备中，在此之前请按照企业版[下载页面](https://www.emqx.com/zh/try?product=enterprise)的指引进行安装操作。
:::
{%endemqxee%}

## tag.gz 安装

1. 下载 [emqx-5.0.14-elixir-amzn2-amd64.tar.gz](https://www.emqx.com/downloads/broker/v5.0.14/emqx-5.0.14-elixir-amzn2-amd64.tar.gz)

```bash
wget https://www.emqx.com/downloads/broker/v5.0.14/emqx-5.0.14-elixir-amzn2-amd64.tar.gz
```

2. 解压程序包

```bash
mkdir -p emqx && tar -zxvf emqx-5.0.14-elixir-amzn2-amd64.tar.gz -C emqx
```

3. 启动

```bash
cd emqx && ./bin/emqx start
```
