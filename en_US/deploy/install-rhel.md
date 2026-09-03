# Install EMQX on CentOS/RHEL

This page guides you on installing and starting EMQX on a CentOS/RHEL system.

Supported versions:

- Amazon Linux 2023
- Rocky Linux 10 (RHEL 10)
- Rocky Linux 9 (RHEL 9)
- Rocky Linux 8 (RHEL 8)

The following sections demonstrate how to download the latest version of EMQX from the official download site. For installation on other systems or to install other versions, visit the [EMQX Enterprise download site](https://www.emqx.com/en/downloads-and-install/enterprise) for detailed information.

::: tip
Starting from EMQX 6.3.0, RPM installations support the same `/opt/emqx/...` paths as the Docker image. For the path mappings, see [Files and Directories](./install.md#files-and-directories).
:::

## Install with rpm

1. Go to the official download page and select the [**CentOS/RHEL** tab](https://www.emqx.com/en/downloads-and-install/enterprise?os=RHEL).
2. Select the latest version `@EE_VERSION@`. From the **Package Type** dropdown, select the `rpm` package that matches your RHEL-compatible distribution version and CPU architecture.
3. Click the link below to download. You can also follow the command-line guide steps for downloading and installing.

### Start EMQX

Start EMQX as a systemd service.

```bash
sudo systemctl start emqx
```

::: tip
Starting from EMQX 6.3.0, set boot-time environment variables such as `EMQX_SECURITY_PROFILE` in `/etc/emqx/emqx.env`. The `emqx` command loads this file whenever it runs, including during a service start, a foreground start, and `emqx ctl`. Package upgrades preserve your changes to this file. Restart the EMQX node to apply changes to boot-time environment variables. See [Boot-Time Environment Variables](../configuration/configuration.md#boot-time-environment-variables).
:::
### Uninstall EMQX

To uninstall EMQX, run:

```
sudo yum remove emqx
```

## Install with tar.gz

1. Go to the official download page and select the [**CentOS/RHEL** tab](https://www.emqx.com/en/downloads-and-install/enterprise?os=RHEL).
2. Select the latest version `@EE_VERSION@`. From the **Package Type** dropdown, select the `tar.gz` package that matches your RHEL-compatible distribution version and CPU architecture.
3. Click the link below to download. You can also follow the command-line guide steps for downloading and installing.

### Start EMQX

After the installation, run the command below to start EMQX.

```bash
./emqx/bin/emqx foreground
```
