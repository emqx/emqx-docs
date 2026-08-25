# Install EMQX on Debian

This page guides you on installing and starting the latest EMQX on the Debian system.

Supported versions:

- Debian 13
- Debian 12
- Debian 11

For installation on other systems or to install other versions, visit the [EMQX Enterprise download site](https://www.emqx.com/en/downloads-and-install/enterprise). 

## Install with deb

1. Go to the download site and select the [**Debian** tab](https://www.emqx.com/en/downloads-and-install/enterprise?os=Debian).
2. Select the latest version `@EE_VERSION@`. From the **Package Type** dropdown, select the `deb` package according to the Debian version and CPU architecture as needed.
3. Click the link below to download. You can also follow the command instructions on the page.


### Start EMQX

Start EMQX as a systemd service, run:

```bash
sudo systemctl start emqx
```

::: tip
Starting from EMQX 6.3.0, set boot-time environment variables such as `EMQX_SECURITY_PROFILE` in `/etc/emqx/emqx.env`. The `emqx` command loads this file whenever it runs, including during a service start, a foreground start, and `emqx ctl`. Package upgrades preserve your changes to this file. Restart the EMQX node to apply changes to boot-time environment variables. See [Boot-Time Environment Variables](../configuration/configuration.md#boot-time-environment-variables).
:::

### Uninstall EMQX

To uninstall EMQX, run:

  ```shell
sudo apt remove --purge emqx
  ```

## Install with tar.gz

1. Go to the download site and select the [**Debian** tab](https://www.emqx.com/en/downloads-and-install/enterprise?os=Debian).
2. Select the latest version `@EE_VERSION@`. From the **Package Type** dropdown, select the `tar.gz` package according to the Debian version and CPU architecture as needed.
3. Click the link below to download. You can also follow the command instructions on the page.

### Start EMQX

After the installation, run the command below to start EMQX.

```bash
./emqx/bin/emqx foreground
```
