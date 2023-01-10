# CentOS/RHEL

This section will guide you on how to install and start EMQX on your CentOS/RHEL system.

Supported versions of CentOS/RHEL: CentOS 7, CentOS 8.

## rpm install

1. Download [emqx-5.0.13-el8-amd64.rpm](https://www.emqx.com/downloads/broker/v5.0.13/emqx-5.0.13-el8-amd64.rpm)

```bash
wget https://www.emqx.com/downloads/broker/v5.0.13/emqx-5.0.13-el8-amd64.rpm
```

2. Install

```bash
sudo yum install ./emqx-5.0.13-el8-amd64.rpm -y
```

3. Run

- Quick Start

  ```bash
  $ emqx start
  EMQX 5.0.13 is started successfully!

  $ emqx_ctl status
  Node 'emqx@127.0.0.1' 5.0.13 is started
  ```

- systemctl

  ```bash
  sudo systemctl start emqx
  ```

- Start as service

  ```bash
  sudo service emqx start
  ```

4. Uninstall

  ```shell
  sudo yum remove emqx
  ```

## tag.gz install

1. Download [emqx-5.0.13-el8-amd64.tar.gz](https://www.emqx.com/downloads/broker/v5.0.13/emqx-5.0.13-el8-amd64.tar.gz)

```bash
wget https://www.emqx.com/downloads/broker/v5.0.13/emqx-5.0.13-el8-amd64.tar.gz
```

1. Unzip the package

```bash
mkdir -p emqx && tar -zxvf emqx-5.0.13-el8-amd64.tar.gz -C emqx
```

3. Run

```bash
cd emqx && ./bin/emqx start
```
