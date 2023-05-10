# macOS

本章节将指导您如何通过 zip 包在 macOS 系统中下载安装并启动 EMQX。

## 安装 EMQX

下文将演示如何下载 EMQX 4.3.19。如希望在其他系统中进行安装，可前往 [EMQX 下载页面](https://www.emqx.io/zh/downloads?os=macOS) 获取详细的安装信息。

1. 下载 [emqx-ee-macos-4.3.19-amd64.zip](https://www.emqx.com/zh/downloads/enterprise/4.3.19/emqx-ee-macos-4.3.19-amd64.zip)

```bash
wget https://www.emqx.com/zh/downloads/enterprise/4.3.19/emqx-ee-macos-4.3.19-amd64.zip
```

2. 安装 EMQX。

```bash
unzip emqx-ee-macos-4.3.19-amd64.zip
```

## 启动 EMQX

您可通过如下三种方式启动 EMQX

- 直接启动：

  ```bash
  $ emqx start
  EMQX 4.3.19 is started successfully!
  
  $ emqx_ctl status
  Node 'emqx@127.0.0.1' 4.3.19 is started
  ```

- systemctl 启动：

  ```bash
  sudo systemctl start emqx
  ```

- service 启动：

  ```bash
  sudo service emqx start
  ```

