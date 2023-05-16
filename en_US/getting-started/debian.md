# Debian

This section guides you on installing and starting EMQX  Debian system.

Supported versions:

- Debian 10
- Debian 9

The section below will take  Debian 10 as an example to illustrate how to download the latest version of EMQX. For other versions, please visit the [EMQX Deployment page](https://www.emqx.com/en/try?product=enterprise).

## Install with rpm

EMQX offers different installation packages for different CPU architectures.

:::: tabs type:card

::: tab amd64

1. Download [emqx-ee-debian10-4.3.19-amd64.deb](https://www.emqx.com/en/downloads/enterprise/4.3.19/emqx-ee-debian10-4.3.19-amd64.deb)

   ```bash
   wget https://www.emqx.com/en/downloads/enterprise/4.3.19/emqx-ee-debian10-4.3.19-amd64.deb
   ```

2. Install EMQX

   ```bash
   sudo apt install ./emqx-ee-debian10-4.3.19-amd64.deb
   ```

:::

::: tab arm64

1. Download [emqx-ee-debian10-4.3.19-amd64.zip](https://www.emqx.com/en/downloads/enterprise/4.3.19/emqx-ee-debian10-4.3.19-amd64.zip)

   ```bash
   wget https://www.emqx.com/en/downloads/enterprise/4.3.19/emqx-ee-debian10-4.3.19-arm64.deb
   ```

2. Install EMQX

   ```bash
   sudo apt install ./emqx-ee-debian10-4.3.19-arm64.deb
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

1. Download [emqx-ee-debian10-4.3.19-amd64.zip](https://www.emqx.com/en/downloads/enterprise/4.3.19/emqx-ee-debian10-4.3.19-amd64.zip)

   ```bash
   wget https://www.emqx.com/en/downloads/enterprise/4.3.19/emqx-ee-debian10-4.3.19-amd64.zip
   ```

2. Install EMQX

   ```bash
   unzip emqx-ee-debian10-4.3.19-amd64.zip
   ```

:::

::: tab arm64

1. Download [emqx-ee-debian10-4.3.19-arm64.zip](https://www.emqx.com/en/downloads/enterprise/4.3.19/emqx-ee-debian10-4.3.19-arm64.zip)

   ```bash
   wget https://www.emqx.com/en/downloads/enterprise/4.3.19/emqx-ee-debian10-4.3.19-arm64.zip
   ```

2. Install EMQX。

   ```bash
   unzip emqx-ee-debian10-4.3.19-arm64.zip
   ```

:::

::::

### Start EMQX

After the installation, run the command below to start EMQX.

```bash
./emqx/bin/emqx start
```