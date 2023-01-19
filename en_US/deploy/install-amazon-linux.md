# Amazon Linux

This section will guide you on installing and starting EMQX on the Amazon Linux system.

## Install EMQX with tag.gz

1. To download [emqx-5.0.14-amzn2-arm64.tar.gz](https://www.emqx.com/downloads/broker/v5.0.14/emqx-5.0.14-amzn2-arm64.tar.gz), run:

```bash
wget https://www.emqx.com/downloads/broker/v5.0.14/emqx-5.0.14-amzn2-arm64.tar.gz
```

2. To unzip the package, run:

```bash
mkdir -p emqx && tar -zxvf emqx-5.0.14-amzn2-arm64.tar.gz -C emqx
```

3. To start EMQX, run:

```bash
cd emqx && ./bin/emqx start
```
