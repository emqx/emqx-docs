# Install EMQX on Debian


This page guides you on installing and starting the latest EMQX on the Debian system.

Supported versions:

- Debian 12
- Debian 11
- Debian 10

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

The section below illustrate how to download the latest version of EMQX. For other versions, visit the [EMQX Open Source download site](https://www.emqx.com/en/downloads-and-install/broker). 

### Install with deb

1. Go to the download site and select the [**Debian** tab](https://www.emqx.com/en/downloads-and-install/broker?os=Debian). Then, select **Package**.
2. Select the latest version `@CE_VERSION@`. From the **Package Type** dropdown, select the `deb` package according to the Debian version and CPU architecture as you need.
3. Click the link below for downloading. You can also follow the command instructions on the page.

#### Start EMQX

Start EMQX as a systemd service, run:

```bash
sudo systemctl start emqx
```

#### Uninstall EMQX

To uninstall EMQX, run:

```
sudo apt remove --purge emqx
```

### Install with tar.gz

1. Go to the download site and select the [**Debian** tab](https://www.emqx.com/en/downloads-and-install/broker?os=Debian). Then, select **Package**.
2. Select the latest version `@CE_VERSION@`. From the **Package Type** dropdown, select the `tar.gz` package according to the Debian version and CPU architecture as you need.
3. Click the link below for downloading. You can also follow the command instructions on the page.

#### Start EMQX

After the installation, run the command below to start EMQX.

```bash
./emqx/bin/emqx foreground
```

