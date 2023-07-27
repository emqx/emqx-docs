# macOS

This section guides you on installing and starting EMQX on macOS with a zip file.

Supported versions:

- macOS 12
- macOS 11

{% emqxce %}

The section takes macOS 12 as an example to illustrate how to download the latest version of EMQX. If you want to install a different version or in a different system, please visit the [EMQX Deployment page](https://www.emqx.io/downloads?os=macOS)

## Install EMQX

1. Download [emqx-@CE_VERSION@-macos12-arm64.zip](https://www.emqx.com/en/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-macos12-arm64.zip). 

   ```bash
   wget https://www.emqx.com/en/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-macos11-amd64.zip
   ```
   
2. Install EMQX. 

   ```bash
   mkdir -p emqx && unzip emqx-@CE_VERSION@-macos11-amd64.zip -d emqx
   ```

## Start EMQX

```bash
./emqx/bin/emqx start
```

{% endemqxce %}

{% emqxee %}

The section below will take macOS 12 as an example to illustrate how to download the latest version of EMQX. If you want to install a different version or in a different system, please visit the [EMQX Deployment page](https://www.emqx.com/en/try?product=enterprise). 

## Install EMQX

1. Go to [EMQ Offical Site](https://www.emqx.com/en/try?product=enterprise&currentVersion=@EE_VERSION@&currentOS=macOS=currentOS=macOS12&utm_source=docs.emqx.com&utm_medium=referral&utm_campaign=enterprise-docs-install-to-try-enterprise), select `@EE_VERSION@` for **Version** and `macOS` for **OS**, and click the **Download** button.

2. On the Downloads and Install page, select `zip` as **Install Method** and select the proper **CPU Architecture**. Download and install the package according to the instruction.

## Start EMQX

```bash
./emqx/bin/emqx start
```

{% endemqxee %}