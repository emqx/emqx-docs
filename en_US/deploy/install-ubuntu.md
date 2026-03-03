# Install EMQX on Ubuntu

This page guides you on installing and starting EMQX on the Ubuntu system.

Supported versions:

- Ubuntu 24.04
- Ubuntu 22.04
- Ubuntu 20.04

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
3. Click the link below for downloading. You can also follow the command-line guide steps for downloading and installing.

#### Start EMQX

Start EMQX as a systemd service.

```bash
sudo systemctl start emqx
```

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
