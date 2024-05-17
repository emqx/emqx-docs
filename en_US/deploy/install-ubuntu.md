{% emqxee %}
# Install EMQX Enterprise on Ubuntu
{% endemqxee %}

{% emqxce %}
# Install EMQX on Ubuntu
{% endemqxce %}


This page guides you on installing and starting EMQX on the Ubuntu system.

Supported versions:

- Ubuntu 22.04
- Ubuntu 20.04
- Ubuntu 18.04

{% emqxce %}

## Install with Apt Source

EMQX supports installing with Apt source to provide our users with a convenient and reliable way to manage EMQX installation and updates. Here is how to install EMQX with Apt source:

1. Download the EMQX repository:

   ```bash
   curl -s https://assets.emqx.com/scripts/install-emqx-deb.sh | sudo bash
   ```

2. Install EMQX:

   ```bash
   sudo apt-get install emqx
   ```

3. Start EMQX:

   ```bash
   sudo systemctl start emqx
   ```

## Install with Package

The section below will take Ubuntu 22.04 as an example to illustrate how to download the latest version of EMQX. For other versions, please visit the [EMQX Deployment page](https://www.emqx.io/downloads?os=Ubuntu). 

## Install with deb

EMQX offers different installation packages for different CPU architectures. 

:::: tabs type:card

::: tab amd64

1. Download [emqx-@CE_VERSION@-ubuntu22.04-amd64.deb](https://www.emqx.com/en/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-ubuntu22.04-amd64.deb).

   ```bash
   wget https://www.emqx.com/en/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-ubuntu22.04-amd64.deb
   ```

2. Install EMQX.

   ```bash
   sudo apt install ./emqx-@CE_VERSION@-ubuntu22.04-amd64.deb
   ```

:::

::: tab arm64

1. Download [emqx-@CE_VERSION@-ubuntu22.04-arm64.deb](https://www.emqx.com/en/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-ubuntu22.04-arm64.deb).

   ```bash
   wget https://www.emqx.com/en/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-ubuntu22.04-arm64.deb
   ```

2. Install EMQX.

   ```bash
   sudo apt install ./emqx-@CE_VERSION@-ubuntu22.04-arm64.deb
   ```

:::

::::

### Start EMQX

Start EMQX as a systemctl service.

```bash
sudo systemctl start emqx
```

### Uninstall EMQX

To uninstall EMQX, run:

```bash
sudo apt remove --purge emqx
```

## Install with tar.gz 

EMQX offers different installation packages for different CPU architectures. 

:::: tabs type:card

::: tab amd64

1. Download [emqx-@CE_VERSION@-ubuntu22.04-amd64.tar.gz](https://www.emqx.com/en/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-ubuntu22.04-amd64.tar.gz).

   ```bash
   wget https://www.emqx.com/en/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-ubuntu22.04-amd64.tar.gz
   ```

2. Install EMQX.

   ```bash
   mkdir -p emqx && tar -zxvf emqx-@CE_VERSION@-ubuntu22.04-amd64.tar.gz -C emqx
   ```

:::

::: tab arm64

1. Download [emqx-@CE_VERSION@-ubuntu22.04-arm64.tar.gz](https://www.emqx.com/en/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-ubuntu22.04-arm64.tar.gz)。

   ```bash
   wget https://www.emqx.com/en/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-ubuntu22.04-arm64.tar.gz
   ```

2. Install EMQX.

   ```bash
   mkdir -p emqx && tar -zxvf emqx-@CE_VERSION@-ubuntu22.04-arm64.tar.gz -C emqx
   ```

:::

::::

### Start EMQX

After the installation, run the command below to start EMQX.

```bash
./emqx/bin/emqx foreground
```

{% endemqxce %}

{% emqxee %}

The section below will take Ubuntu 22.04 as an example to illustrate how to download the latest version of EMQX. For other versions, please visit the [EMQX Deployment page](https://www.emqx.com/en/try?product=enterprise). 

## Install with deb

1. Go to the [official site for EMQX](https://www.emqx.com/en/try?product=enterprise&currentVersion=@EE_VERSION@&currentOS=Ubuntu=currentOS=Ubuntu22&utm_source=docs.emqx.com&utm_medium=referral&utm_campaign=enterprise-docs-install-to-try-enterprise).

2. Select `@EE_VERSION@` for **Version** and `Ubuntu` for **OS**, and click the **Download** button.

3. On the Downloads and Install page, select `deb` as the **Install Method** and select the proper **CPU Architecture** that matches your system. Click **Download Now**.

   You can also follow the command instructions on the page.

### Start EMQX

Start EMQX as a systemd service.

```bash
sudo systemctl start emqx
```

#### Uninstall EMQX

To uninstall EMQX, run:

```
sudo apt remove --purge emqx
```

## Install with tar.gz

1. Go to the [official site for EMQX](https://www.emqx.com/en/try?product=enterprise&currentVersion=@EE_VERSION@&currentOS=Ubuntu=currentOS=Ubuntu22&utm_source=docs.emqx.com&utm_medium=referral&utm_campaign=enterprise-docs-install-to-try-enterprise).
2. Select `@EE_VERSION@` for **Version** and `Ubuntu` for **OS**, and click the **Download** button.
3. On the Downloads and Install page, select `tar.gz` as the **Install Method** and select the proper **CPU Architecture** that matches your system. Click **Download Now**.

   You can also follow the command instructions on the page.

### Start EMQX

After the installation, run the command below to start EMQX.

```bash
./emqx/bin/emqx foreground
```

{% endemqxee %}
