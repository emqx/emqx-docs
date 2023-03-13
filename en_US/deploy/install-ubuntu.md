# Ubuntu

This section will guide you on installing and starting EMQX on the Ubuntu system.

Supported versions:

{% emqxce %}

- Ubuntu 22.04
- Ubuntu 20.04
- Ubuntu 18.04

{% endemqxce %}

{% emqxee %}

- Ubuntu 20.04
- Ubuntu 18.04

{% endemqxee %}

The section below will take Ubuntu 20.04 as an example to illustrate how to download the latest version of EMQX. For other versions, please visit the [EMQX Deployment page](https://www.emqx.com/zh/try?product=enterprise). 

{% emqxce %}

## Install with deb

EMQX has offered different installation packages for different CPU architectures. 

:::: tabs type:card

::: tab amd64

1. Download [emqx-5.0.17-ubuntu20.04-amd64.deb](https://www.emqx.com/en/downloads/broker/5.0.17/emqx-5.0.17-ubuntu20.04-amd64.deb).

   ```bash
   wget https://www.emqx.com/en/downloads/broker/5.0.17/emqx-5.0.17-ubuntu20.04-amd64.deb
   ```

2. Install EMQX.

   ```bash
   sudo apt install ./emqx-5.0.17-ubuntu20.04-amd64.deb
   ```

:::

::: tab arm64

1. Download [emqx-5.0.17-ubuntu20.04-arm64.deb](https://www.emqx.com/en/downloads/broker/5.0.17/emqx-5.0.17-ubuntu20.04-arm64.deb).

   ```bash
   wget https://www.emqx.com/en/downloads/broker/5.0.17/emqx-5.0.17-ubuntu20.04-arm64.deb
   ```

2. Install EMQX.

   ```bash
   sudo apt install ./emqx-5.0.17-ubuntu20.04-arm64.deb
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

1. Download [emqx-5.0.17-ubuntu20.04-amd64.tar.gz](https://www.emqx.com/en/downloads/broker/5.0.17/emqx-5.0.17-ubuntu20.04-amd64.tar.gz).

   ```bash
   wget https://www.emqx.com/en/downloads/broker/5.0.17/emqx-5.0.17-ubuntu20.04-amd64.tar.gz
   ```

2. Install EMQX.

   ```bash
   mkdir -p emqx && tar -zxvf emqx-5.0.17-ubuntu20.04-amd64.tar.gz -C emqx
   ```

:::

::: tab arm64

1. Download [emqx-5.0.17-ubuntu20.04-arm64.tar.gz](https://www.emqx.com/zh/downloads/broker/5.0.17/emqx-5.0.17-ubuntu20.04-arm64.tar.gz)。

   ```bash
   wget https://www.emqx.com/zh/downloads/broker/5.0.17/emqx-5.0.17-ubuntu20.04-arm64.tar.gz
   ```

2. Install EMQX.

   ```bash
   mkdir -p emqx && tar -zxvf emqx-5.0.17-ubuntu20.04-arm64.tar.gz -C emqx
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

## Install with deb

EMQX has offered different installation packages for different CPU architectures. 

:::: tabs type:card

::: tab amd64

1. Download [emqx-enterprise-5.0.0-ubuntu20.04-amd64.deb](https://www.emqx.com/en/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-ubuntu20.04-amd64.deb).

   ```bash
   wget https://www.emqx.com/en/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-ubuntu20.04-amd64.deb
   ```

2. Install EMQX.

   ```bash
   sudo apt install ./emqx-enterprise-5.0.0-ubuntu20.04-amd64.deb
   ```

:::

::: tab arm64

1. Download [emqx-enterprise-5.0.0-ubuntu20.04-arm64.deb](https://www.emqx.com/en/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-ubuntu20.04-arm64.deb).

   ```bash
   wget https://www.emqx.com/en/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-ubuntu20.04-arm64.deb
   ```

2. Install EMQX.

   ```bash
   sudo apt install ./emqx-enterprise-5.0.0-ubuntu20.04-arm64.deb
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
  Node 'emqx@127.0.0.1' 5.0.00 is started
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

1.  Download [emqx-enterprise-5.0.0-ubuntu20.04-amd64.tar.gz](https://www.emqx.com/en/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-ubuntu20.04-amd64.tar.gz).

   ```
   wget https://www.emqx.com/en/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-ubuntu20.04-amd64.tar.gz
   ```

2. Install EMQX.

   ```
   mkdir -p emqx && tar -zxvf emqx-enterprise-5.0.0-ubuntu20.04-amd64.tar.gz -C emqx
   ```

:::

::: tab arm64

1. Download [emqx-enterprise-5.0.0-ubuntu20.04-arm64.tar.gz](https://www.emqx.com/en/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-ubuntu20.04-arm64.tar.gz)

   ```bash
   wget https://www.emqx.com/en/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-ubuntu20.04-arm64.tar.gz
   ```

2. Install EMQX.

   ```bash
   mkdir -p emqx && tar -zxvf emqx-enterprise-5.0.0-ubuntu20.04-arm64.tar.gz -C emqx
   ```

:::

::::

### Start EMQX

After the installation, run the command below to start EMQX.

```
./emqx/bin/emqx start
```

{% endemqxee %}