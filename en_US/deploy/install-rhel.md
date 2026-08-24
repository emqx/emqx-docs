# Install EMQX on CentOS/RHEL

This page guides you on installing and starting EMQX on a CentOS/RHEL system.

Supported versions:

- Amazon Linux 2023
- Rocky Linux 9 (RHEL 9)
- Rocky Linux 8 (RHEL 8)

The following sections demonstrate how to download the latest version of EMQX for installation on the CentOS 8 system as an example. For installation on other systems or to install other versions, visit the [EMQX Enterprise download site](https://www.emqx.com/en/downloads-and-install/enterprise) for detailed information.

## Install with rpm

1. Go to the official download page and select the [**CentOS/RHEL** tab](https://www.emqx.com/en/downloads-and-install/enterprise?os=RHEL).
2. Select the latest version `@EE_VERSION@`. From the **Package Type** dropdown, select the `RHEL 8 (CentOS 8) amd64` or `RHEL 8 (CentOS 8) arm64` -> `rpm` package according to the required CPU architecture.
3. Click the link below to download. You can also follow the command-line guide steps for downloading and installing.

### Start EMQX

Start EMQX as a systemd service.

```bash
sudo systemctl start emqx
```

::: tip
Starting from EMQX 6.3.0, boot-time environment variables such as `EMQX_SECURITY_PROFILE` can be set in `/etc/emqx/emqx.env`. The `emqx` command sources this file on every invocation, and package upgrades keep your edits. See [Environment Variables](../configuration/configuration.md#environment-variables).
:::
### Uninstall EMQX

To uninstall EMQX, run:

```
sudo yum remove emqx
```

## Install with tar.gz

1. Go to the official download page and select the [**CentOS/RHEL** tab](https://www.emqx.com/en/downloads-and-install/enterprise?os=RHEL).
2. Select the latest version `@EE_VERSION@`. From the **Package Type** dropdown, select the `RHEL 8 (CentOS 8) amd64` or `RHEL 8 (CentOS 8) arm64` -> `tar.gz` package according to the required CPU architecture.
3. Click the link below to download. You can also follow the command-line guide steps for downloading and installing.

### Start EMQX

After the installation, run the command below to start EMQX.

```bash
./emqx/bin/emqx foreground
```
