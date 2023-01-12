# Ubuntu

本章节将指导您如何在 Ubuntu 系统中下载安装并启动 EMQX。

<<<<<<< HEAD
支持的 Ubuntu 版本：

- Ubuntu18.04
- Ubuntu20.04

## 通过 deb 安装 EMQX

1. 下载 [emqx-5.0.13-ubuntu20.04-amd64.deb](https://www.emqx.com/downloads/broker/v5.0.13/emqx-5.0.13-ubuntu20.04-amd64.deb)：
=======
支持的 Ubuntu 版本：Ubuntu18.04, Ubuntu20.04。

## deb 安装

1. 下载 [emqx-5.0.13-ubuntu20.04-amd64.deb](https://www.emqx.com/downloads/broker/v5.0.13/emqx-5.0.13-ubuntu20.04-amd64.deb)
>>>>>>> 3ed60c6d5e16cae60cc1f5c27c58210ada9c4ca8

```bash
wget https://www.emqx.com/downloads/broker/v5.0.13/emqx-5.0.13-ubuntu20.04-amd64.deb
```

<<<<<<< HEAD
2. 安装 EMQX：
=======
2. 安装
>>>>>>> 3ed60c6d5e16cae60cc1f5c27c58210ada9c4ca8

```bash
sudo apt install ./emqx-5.0.13-ubuntu20.04-amd64.deb
```

<<<<<<< HEAD
3. 我们提供了几种不同的方式启动 EMQX：
=======
3. 启动
>>>>>>> 3ed60c6d5e16cae60cc1f5c27c58210ada9c4ca8

- 直接启动：

  ```bash
  $ emqx start
  EMQX 5.0.13 is started successfully!

  $ emqx_ctl status
  Node 'emqx@127.0.0.1' 5.0.13 is started
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

1. 下载 [emqx-5.0.13-ubuntu20.04-amd64.tar.gz](https://www.emqx.com/downloads/broker/v5.0.13/emqx-5.0.13-ubuntu20.04-amd64.tar.gz)：

```bash
wget https://www.emqx.com/downloads/broker/v5.0.13/emqx-5.0.13-ubuntu20.04-amd64.tar.gz
```

1. 解压程序包：

```bash
mkdir -p emqx && tar -zxvf emqx-5.0.13-ubuntu20.04-amd64.tar.gz -C emqx
```

3. 启动 EMQX：

```bash
cd emqx && ./bin/emqx start
```
