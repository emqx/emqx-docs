# CentOS/RHEL

This section guides you on installing and starting EMQX on CentOS/RHEL system.

Supported versions:

- CentOS 8
- CentOS 7

The section below will take CentOS 7 as an example to illustrate how to download the latest version of EMQX. For other versions, please visit the [EMQX Deployment page](https://www.emqx.com/en/try?product=enterprise).

## Install with rpm

EMQX offers different installation packages for different CPU architectures.

:::: tabs type:card

::: tab amd64

1. Download [emqx-ee-centos7-4.3.19-amd64.rpm](https://www.emqx.com/en/downloads/enterprise/4.3.19/emqx-ee-centos7-4.3.19-amd64.rpm).

   ```bash
   wget https://www.emqx.com/en/downloads/enterprise/4.3.19/emqx-ee-centos7-4.3.19-amd64.rpm
   ```

2. Install EMQX

   ```bash
   sudo yum install emqx-ee-centos7-4.3.19-amd64.rpm -y
   ```

:::

::: tab arm64

1. Download [emqx-ee-centos7-4.3.19-arm64.rpm](https://www.emqx.com/en/downloads/enterprise/4.3.19/emqx-ee-centos7-4.3.19-arm64.rpm)。

   ```bash
   wget https://www.emqx.com/en/downloads/enterprise/4.3.19/emqx-ee-centos7-4.3.19-arm64.rpm
   ```

2. Install EMQX

   ```bash
   sudo yum install emqx-ee-centos7-4.3.19-arm64.rpm -y
   ```

:::

::::

### Start EMQX

EMQX offers 3 different options to start EMQX:

- To start EMQX directly, run:

  ```bash
  $ emqx start
  EMQX 4.3.19 is started successfully!
  
  $ emqx_ctl status
  Node 'emqx@127.0.0.1' 4.3.19 is started
  ```

- To start EMQX with systemctl, run:

  ```bash
  sudo systemctl start emqx
  ```

- To start EMQX as a service, run:

  ```bash
  sudo service emqx start
  ```

## Install with tar.gz

EMQX offers different installation packages for different CPU architectures.

:::: tabs type:card

::: tab amd64

1. Download [mqx-ee-centos7-4.3.19-amd64.zip](https://www.emqx.com/en/downloads/enterprise/4.3.19/emqx-ee-centos7-4.3.19-amd64.zip)

   ```bash
   wget https://www.emqx.com/en/downloads/enterprise/4.3.19/emqx-ee-centos7-4.3.19-amd64.zip
   ```

2. Install EMQX

   ```bash
   unzip emqx-ee-centos7-4.3.19-amd64.zip
   ```

:::

::: tab arm64

1. Download [emqx-ee-centos7-4.3.19-arm64.zip](https://www.emqx.com/en/downloads/enterprise/4.3.19/emqx-ee-centos7-4.3.19-arm64.zip)

   ```bash
   wget https://www.emqx.com/en/downloads/enterprise/4.3.19/emqx-ee-centos7-4.3.19-arm64.zip
   ```

2. Install EMQX

   ```bash
   unzip emqx-ee-centos7-4.3.19-arm64.zip
   ```

:::

::::

### Start EMQX

After the installation, run the command below to start EMQX.

```bash
./emqx/bin/emqx start
```
