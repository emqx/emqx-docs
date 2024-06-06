# Install EMQX Open Source on CentOS/RHEL


This page guides you on installing and starting the EMQX Open Source edition on CentOS/RHEL system.

Supported versions:

- Amazon Linux 2023
- Amazon Linux 2
- Rocky Linux 9 (RHEL 9)
- Rocky Linux 8 (RHEL 8)
- CentOS 7 (RHEL 7)

## Install with Yum Source

EMQX supports installing with Yum source to provide our users with a convenient and reliable way to manage EMQX installation and updates. Here is how to install EMQX with Yum source:

1. Download the EMQX repository:

   ```bash
   curl -s https://assets.emqx.com/scripts/install-emqx-rpm.sh | sudo bash
   ```

2. Install the following dependencies:

   ```bash
   yum install epel-release -y
   yum install -y openssl11 openssl11-devel
   ```

3. Install EMQX:

   ```bash
   sudo yum install emqx -y
   ```

4. Start EMQX:

   ```bash
   sudo systemctl start emqx
   ```

## Install with rpm

EMQX offers rpm installation packages for different CPU architectures.

This section demonstrates how to download the latest version of EMQX for installation on the CentOS 8 system as an example. For installation on other systems or to install other versions, visit the [EMQX Open Source download site](https://www.emqx.com/en/downloads-and-install/broker) for detailed installation information.

1. Go to the official download page and select the [**CentOS/RHEL** tab](https://www.emqx.com/en/downloads-and-install/broker?os=RHEL). Then, select **Package**.
2. Select the latest version `@CE_VERSION@`. From the **Package Type** dropdown, select the `RHEL 8 (CentOS 8) amd64` or `RHEL 8 (CentOS 8) arm64` -> `rpm` package according to the required CPU architecture.
3. Click the link below for downloading. You can also follow the command-line guide steps for downloading and installing.

### Start EMQX

Start EMQX as a systemd service.

```bash
sudo systemctl start emqx
```

### Uninstall EMQX

To uninstall EMQX, run:

```
sudo yum remove emqx
```

## Install with tar.gz

EMQX offers tar.gz installation packages for different CPU architectures.

This section demonstrates how to download the latest version of EMQX for installation on the CentOS 8 system as an example. For installation on other systems or to install other versions, visit the [EMQX Open Source download site](https://www.emqx.com/en/downloads-and-install/broker) for detailed installation information.

1. Go to the official download page and select the [**CentOS/RHEL** tab](https://www.emqx.com/en/downloads-and-install/broker?os=RHEL). Then, select **Package**.
2. Select the latest version `@CE_VERSION@`. From the **Package Type** dropdown, select the `RHEL 8 (CentOS 8) amd64` or `RHEL 8 (CentOS 8) arm64` -> `tar.gz` package according to the required CPU architecture.
3. Click the link below for downloading. You can also follow the command-line guide steps for downloading and installing.

### Start EMQX

After the installation, run the command below to start EMQX.

```bash
./emqx/bin/emqx foreground
```

