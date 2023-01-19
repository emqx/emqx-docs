# Debian

本章节将指导您如何在 Debian 系统中下载安装并启动 EMQX。

支持的 Debian 版本：Debian10, Debian11。

{%emqxee%}
:::tip
此处是 EMQX 开源版安装文档。EMQX 企业版安装文档正在准备中，在此之前请按照企业版[下载页面](https://www.emqx.com/zh/try?product=enterprise)的指引进行安装操作。
:::
{%endemqxee%}

## deb 安装

1. 下载 [emqx-5.0.14-debian11-amd64.deb](https://www.emqx.com/downloads/broker/v5.0.14/emqx-5.0.14-debian11-amd64.deb)

```bash
wget https://www.emqx.com/downloads/broker/v5.0.14/emqx-5.0.14-debian11-amd64.deb
```

2. 安装

```bash
sudo apt install ./emqx-5.0.14-debian11-amd64.deb
```

3. 启动

- 直接启动：

  ```bash
  $ emqx start
  EMQX 5.0.14 is started successfully!

  $ emqx_ctl status
  Node 'emqx@127.0.0.1' 5.0.14 is started
  ```

- systemctl 启动：

  ```bash
  sudo systemctl start emqx
  ```

- service 启动：

  ```bash
  sudo service emqx start
  ```

4. 卸载

  ```shell
  sudo apt remove --purge emqx
  ```

## tag.gz 安装

1. 下载 [emqx-5.0.14-debian11-amd64.tar.gz](https://www.emqx.com/downloads/broker/v5.0.14/emqx-5.0.14-debian11-amd64.tar.gz)

```bash
wget https://www.emqx.com/downloads/broker/v5.0.14/emqx-5.0.14-debian11-amd64.tar.gz
```

1. 解压程序包

```bash
mkdir -p emqx && tar -zxvf emqx-5.0.14-debian11-amd64.tar.gz -C emqx
```

3. 启动

```bash
cd emqx && ./bin/emqx start
```
