# Install EMQX on Ubuntu

This page guides you on installing and starting EMQX on the Ubuntu system.

Supported versions:

- Ubuntu 24.04
- Ubuntu 22.04

::: tip
Starting from EMQX 6.3.0, installing EMQX through Apt or from a DEB package provides the same `/opt/emqx/...` paths as the Docker image. For the path mappings, see [Files and Directories](./install.md#files-and-directories).
:::

## Install with Apt Package Manager

EMQX supports installing with Apt Package Manager to provide our users with a convenient and reliable way to manage EMQX installation and updates. Here is how to install EMQX with apt:

1. Install the EMQX apt repository:

   ```bash
   curl -s https://packagecloud.io/install/repositories/emqx/emqx-enterprise5/script.deb.sh | sudo bash
   ```

2. Install EMQX:

   ```bash
   sudo apt-get install emqx
   ```

3. Start EMQX:

   ```bash
   sudo systemctl start emqx
   ```

## Manual package installation

EMQX supports installation via deb packages or tar.gz packages. For installation on other supported systems or to try other versions, please visit the [EMQX Enterprise download site](https://www.emqx.com/en/downloads-and-install/enterprise) for installation information.

### Install with deb

1. Go to the official download page and select the [**Ubuntu** tab](https://www.emqx.com/en/downloads-and-install/enterprise?os=Ubuntu).
2. Select the latest version `@EE_VERSION@` and select the `deb` package according to the required version and CPU architecture in the **Package Type** dropdown.
3. Click the link below to download. You can also follow the command-line guide steps for downloading and installing.

#### Start EMQX

Start EMQX as a systemd service.

```bash
sudo systemctl start emqx
```

::: tip
Starting from EMQX 6.3.0, set boot-time environment variables such as `EMQX_SECURITY_PROFILE` in `/etc/emqx/emqx.env`. The `emqx` command loads this file whenever it runs, including during a service start, a foreground start, and `emqx ctl`. Package upgrades preserve your changes to this file. Restart the EMQX node to apply changes to boot-time environment variables. See [Boot-Time Environment Variables](../configuration/configuration.md#boot-time-environment-variables).
:::

#### Uninstall EMQX

To uninstall EMQX, run:

```
sudo apt remove --purge emqx
```

### Install with tar.gz

1. Go to the official download page and select the [**Ubuntu** tab](https://www.emqx.com/en/downloads-and-install/enterprise?os=Ubuntu).
2. Select the latest version `@EE_VERSION@` and select the `tar.gz` package according to the required version and CPU architecture in the **Package Type** dropdown.
3. Click the link below for downloading. You can also follow the command-line guide steps for downloading and installing.

#### Start EMQX

After the installation, run the command below to start EMQX.

```bash
./emqx/bin/emqx foreground
```
