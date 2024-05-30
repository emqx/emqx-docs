# Install EMQX Enterprise on Debian

This page guides you on installing and starting the EMQX Enterprise edition on the Debian system.

Supported versions:

- Debian 12
- Debian 11
- Debian 10

The section below will take Debian 11 as an example to illustrate how to download the latest version of EMQX. For other versions, please visit the [EMQX Deployment page](https://www.emqx.com/en/try?product=enterprise). 

## Install with deb

1. Go to the [official site for EMQX](https://www.emqx.com/en/try?product=enterprise&currentVersion=@EE_VERSION@&currentOS=Debian=currentOS=Debian12&utm_source=docs.emqx.com&utm_medium=referral&utm_campaign=enterprise-docs-install-to-try-enterprise).
2. Select `@EE_VERSION@` for **Version** and `Debian` for **OS**, and click the **Download** button.
3. On the Downloads and Install page, select `deb` as the **Install Method** and select the proper **CPU Architecture** that matches your system. Click **Download Now**.

   You can also follow the command instructions on the page.

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

1. Go to the [official site for EMQX](https://www.emqx.com/en/try?product=enterprise&currentVersion=@EE_VERSION@&currentOS=Debian=currentOS=Debian12&utm_source=docs.emqx.com&utm_medium=referral&utm_campaign=enterprise-docs-install-to-try-enterprise).

2. Select `@EE_VERSION@` for **Version** and `Debian` for **OS**, and click the **Download** button.

3. On the Downloads and Install page, select `tar.gz` as the **Install Method** and select the proper **CPU Architecture** that matches your system. Click **Download Now**.

   You can also follow the command instructions on the page.

### Start EMQX

After the installation, run the command below to start EMQX.

```bash
./emqx/bin/emqx foreground
```
