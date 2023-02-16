# CentOS/RHEL

This section will guide you on how to install and start EMQX on CentOS/RHEL system.

Supported versions: 

- CentOS 8
- CentOS 7

The section below will take CentOS 8 as an example to illustrate how to download the latest version of EMQX. For other versions, please visit the [EMQX Deployment page](https://www.emqx.com/zh/try?product=enterprise). 

{% emqxce %}

## Install with rpm

EMQX has offered different installation packages for different CPU architectures. 

:::: tabs type:card

::: tab amd64

1. Download [emqx-5.0.17-el8-amd64.rpm](https://www.emqx.com/en/downloads/broker/5.0.17/emqx-5.0.17-el8-amd64.rpm). 

   ```bash
   wget https://www.emqx.com/en/downloads/broker/5.0.17/emqx-5.0.17-el8-amd64.rpm
   ```

2. Install EMQX. 

   ```bash
   sudo yum install emqx-5.0.17-el8-amd64.rpm -y
   ```

:::

::: tab arm64

1. Download [emqx-5.0.17-el8-arm64.rpm](https://www.emqx.com/en/downloads/broker/5.0.17/emqx-5.0.17-el8-arm64.rpm). 

   ```bash
   wget https://www.emqx.com/en/downloads/broker/5.0.17/emqx-5.0.17-el8-arm64.rpm
   ```

2. Install EMQX. 

   ```bash
   sudo yum install emqx-5.0.17-el8-arm64.rpm -y
   ```

:::

::::

### Start EMQX

EMQX offers 3 different options to start EMQX:

- To start EMQX directly, run:

  ```bash
  $ emqx start
  EMQX 5.0.17 is started successfully!
  
  $ emqx_ctl status
  Node 'emqx@127.0.0.1' 5.0.17 is started
  ```

- To start EMQX with systemctl, run:

  ```bash
  sudo systemctl start emqx
  ```

- To start EMQX as a service, run:

  ```bash
  sudo service emqx start
  ```

### Uninstall EMQX

To uninstall EMQX, run:

```
sudo apt remove --purge emqx
```

## Install with tar.gz

EMQX has offered different installation packages for different CPU architectures. 

:::: tabs type:card

::: tab amd64

1. Download [emqx-5.0.17-el8-amd64.tar.gz](https://www.emqx.com/en/downloads/broker/5.0.17/emqx-5.0.17-el8-amd64.tar.gz). 

   ```bash
   wget https://www.emqx.com/en/downloads/broker/5.0.17/emqx-5.0.17-el8-amd64.tar.gz
   ```

2. Install EMQX.

   ```bash
   mkdir -p emqx && tar -zxvf emqx-5.0.17-el8-amd64.tar.gz -C emqx
   ```

:::

::: tab arm64

1. Download [emqx-5.0.17-el8-arm64.tar.gz](https://www.emqx.com/en/downloads/broker/5.0.17/emqx-5.0.17-el8-arm64.tar.gz). 

   ```bash
   wget https://www.emqx.com/en/downloads/broker/5.0.17/emqx-5.0.17-el8-arm64.tar.gz
   ```

2. Install EMQX.

   ```bash
   mkdir -p emqx && tar -zxvf emqx-5.0.17-el8-arm64.tar.gz -C emqx
   ```

:::

::::

### Start EMQX

After the installation, run the command below to start EMQX.

```bash
./emqx/bin/emqx start
```

{% endemqxce %}

{% emqxee %}

## Install with rpm

EMQX has offered different installation packages for different CPU architectures. 

:::: tabs type:card

::: tab amd64

1.  Download [emqx-enterprise-5.0.0-el8-amd64.rpm](https://www.emqx.com/en/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-el8-amd64.rpm). 

   ```bash
   wget https://www.emqx.com/en/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-el8-amd64.rpm
   ```

2. Install EMQX.

   ```bash
   sudo yum install emqx-enterprise-5.0.0-el8-amd64.rpm -y
   ```

:::

::: tab arm64

1. Download [emqx-enterprise-5.0.0-el8-arm64.rpm](https://www.emqx.com/en/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-el8-arm64.rpm)

   ```bash
   wget https://www.emqx.com/en/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-el8-arm64.rpm
   ```

2. Install EMQX.

   ```bash
   sudo yum install emqx-enterprise-5.0.0-el8-arm64.rpm -y
   ```

:::

::::

### Start EMQX

EMQX offers 3 different options to start EMQX:

- To start EMQX directly, run:

  ```bash
  $ emqx start
  EMQX 5.0.0 is started successfully!
  
  $ emqx_ctl status
  Node 'emqx@127.0.0.1' 5.0.0 is started
  ```

- To start EMQX with systemctl, run:

  ```bash
  sudo systemctl start emqx
  ```

- To start EMQX as a service, run:

  ```bash
  sudo service emqx start
  ```

### Uninstall EMQX

To uninstall EMQX, run:

```
sudo apt remove --purge emqx
```

## Install with tar.gz

EMQX has offered different installation packages for different CPU architectures. 

:::: tabs type:card

::: tab amd64

1. Download [emqx-enterprise-5.0.0-el8-amd64.tar.gz](https://www.emqx.com/en/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-el8-amd64.tar.gz). 

   ```bash
   wget https://www.emqx.com/en/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-el8-amd64.tar.gz
   ```

2. Install EMQX.

   ```
   wget https://www.emqx.com/zh/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-debian11-amd64.tar.gz
   ```

:::

::: tab arm64

1. Download [emqx-enterprise-5.0.0-el8-arm64.tar.gz](https://www.emqx.com/en/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-el8-arm64.tar.gz). 

   ```bash
   wget https://www.emqx.com/en/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-el8-arm64.tar.gz
   ```

2. Install EMQX.

   ```bash
   mkdir -p emqx && tar -zxvf emqx-enterprise-5.0.0-debian11-arm64.tar.gz -C emqx
   ```

:::

::::

### Start EMQX

After the installation, run the command below to start EMQX.

```bash
./emqx/bin/emqx start
```

{% endemqxee %}