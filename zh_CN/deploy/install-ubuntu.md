# Ubuntu

本章节将指导您如何在 Ubuntu 系统中下载安装并启动 EMQX。

支持的 Ubuntu 版本：Ubuntu18.04, Ubuntu20.04。

## deb 安装

1. 下载 [emqx-5.0.14-ubuntu20.04-amd64.deb](https://www.emqx.com/downloads/broker/v5.0.14/emqx-5.0.14-ubuntu20.04-amd64.deb)

```bash
wget https://www.emqx.com/downloads/broker/v5.0.14/emqx-5.0.14-ubuntu20.04-amd64.deb
```

2. 安装 EMQX：

```bash
sudo apt install ./emqx-5.0.14-ubuntu20.04-amd64.deb
```

3. 我们提供了几种不同的方式启动 EMQX：

- 直接启动：

  ```bash
  $ emqx start
  EMQX 5.0.14 is started successfully!

  $ emqx_ctl status
  Node 'emqx@127.0.0.1' 5.0.14 is started
  ```

- 通过 systemctl 启动：

  ```bash
  sudo systemctl start emqx
  ```

- 以 service 的形式启动：

  ```bash
  sudo service emqx start
  ```

4. 卸载 EMQX：

  ```shell
  sudo apt remove --purge emqx
  ```

## 通过 tag.gz 安装 EMQX

1. 下载 [emqx-5.0.14-ubuntu20.04-amd64.tar.gz](https://www.emqx.com/downloads/broker/v5.0.14/emqx-5.0.14-ubuntu20.04-amd64.tar.gz)：

```bash
wget https://www.emqx.com/downloads/broker/v5.0.14/emqx-5.0.14-ubuntu20.04-amd64.tar.gz
```

1. 解压程序包：

```bash
mkdir -p emqx && tar -zxvf emqx-5.0.14-ubuntu20.04-amd64.tar.gz -C emqx
```

3. 启动 EMQX：

```bash
cd emqx && ./bin/emqx start
```
