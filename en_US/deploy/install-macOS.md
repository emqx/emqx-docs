# macOS

This section will guide you on how to install and start EMQX on macOS with a zip file.

Supported versions:

- macOS 12
- macOS 11

The section below will take macOS 12 as an example to illustrate how to download the latest version of EMQX. For other If you want to install a different version or in a different system, please visit the [EMQX Deployment page](https://www.emqx.com/zh/try?product=enterprise). 

{% emqxce %}

1. Download [emqx-5.0.17-macos12-arm64.zip](https://www.emqx.com/en/downloads/broker/5.0.17/emqx-5.0.17-macos12-arm64.zip). 

```bash
wget https://www.emqx.com/en/downloads/broker/5.0.17/emqx-5.0.17-macos12-arm64.zip
```

2. Install EMQX.

```bash
mkdir -p emqx && unzip emqx-5.0.17-macos12-arm64.zip -d emqx
```

3. Start EMQX. 

```bash
./emqx/bin/emqx start
```


{% endemqxce %}

{% emqxee %}

1. Download [emqx-enterprise-5.0.0-macos12-arm64.zip](https://www.emqx.com/en/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-macos12-arm64.zip). 

```bash
wget https://www.emqx.com/zh/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-macos12-arm64.zip
```

2. Install EMQX.

```bash
mkdir -p emqx && unzip emqx-enterprise-5.0.0-macos12-arm64.zip -d emqx
```

3. Start EMQX. 

```bash
./emqx/bin/emqx start
```

{% endemqxee %}