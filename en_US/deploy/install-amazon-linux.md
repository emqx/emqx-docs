# Amazon Linux

This section will guide you on how to install and start EMQX on your Amazon Linux system.

## tag.gz install

1. Download [emqx-5.0.13-amzn2-arm64.tar.gz](https://www.emqx.com/downloads/broker/v5.0.13/emqx-5.0.13-amzn2-arm64.tar.gz)

```bash
wget https://www.emqx.com/downloads/broker/v5.0.13/emqx-5.0.13-amzn2-arm64.tar.gz
```

1. Unzip the package

```bash
mkdir -p emqx && tar -zxvf emqx-5.0.13-amzn2-arm64.tar.gz -C emqx
```

3. Run

```bash
cd emqx && ./bin/emqx start
```
