# Install EMQX Enterprise on CentOS/RHEL

This page guides you on installing and starting EMQX on CentOS/RHEL system.

Supported versions:

- Amazon Linux 2023
- Amazon Linux 2
- Rocky Linux 9 (RHEL 9)
- Rocky Linux 8 (RHEL 8)
- CentOS 7 (RHEL 7)

## Install with rpm

1. Go to the [official site for EMQX](https://www.emqx.com/en/try?product=enterprise).

2. Select `@EE_VERSION@` for **Version** and `CentOS` for **OS**, and click the **Download** button.

3. On the Downloads and Install page, select `rpm` as the **Install Method** and select the proper **CPU Architecture** that matches your system. Click **Download Now**.

   You can also follow the command instructions on the page.

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

1. Go to the [official site for EMQX](https://www.emqx.com/en/try?product=enterprise).
2. Select `@EE_VERSION@` for **Version** and `CentOS` for **OS**, and click the **Download** button.
3. On the Downloads and Install page, select `tar.gz` as the **Install Method** and select the proper **CPU Architecture** that matches your system. Click **Download Now**.

   You can also follow the command instructions on the page.

### Start EMQX

After the installation, run the command below to start EMQX.

```bash
./emqx/bin/emqx foreground
```
