# Install EMQX Enterprise on Debian

This page guides you on installing and starting the latest EMQX on the Debian system.

Supported versions:

- Debian 12
- Debian 11
- Debian 10

For installation on other systems or to install other versions, visit the [EMQX Enterprise download site](https://www.emqx.com/en/downloads-and-install/enterprise). 

## Install with deb

1. Go to the download site and select the [**Debian** tab](https://www.emqx.com/en/downloads-and-install/enterprise?os=Debian).
2. Select the latest version `@EE_VERSION@`. From the **Package Type** dropdown, select the `deb` package according to the Debian version and CPU architecture as you need.
3. Click the link below for downloading. You can also follow the command instructions on the page.


### Start EMQX

Start EMQX as a systemd service, run:

```bash
sudo systemctl start emqx
```

### Uninstall EMQX

To uninstall EMQX, run:

  ```shell
sudo apt remove --purge emqx
  ```

## Install with tar.gz

1. Go to the download site and select the [**Debian** tab](https://www.emqx.com/en/downloads-and-install/enterprise?os=Debian).
2. Select the latest version `@EE_VERSION@`. From the **Package Type** dropdown, select the `tar.gz` package according to the Debian version and CPU architecture as you need.
3. Click the link below for downloading. You can also follow the command instructions on the page.

### Start EMQX

After the installation, run the command below to start EMQX.

```bash
./emqx/bin/emqx foreground
```
