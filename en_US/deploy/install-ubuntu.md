# Ubuntu

This section will guide you on installing and starting EMQX on the Ubuntu system.

Supported versions: 

- Ubuntu18.04
- Ubuntu20.04

{%emqxee%}
:::tip
This is the EMQX Open source edition installation document. the EMQX Enterprise installation document is coming soon, until then please follow the guidelines on the [EMQX Enterprise download page](https://www.emqx.com/en/try?product=enterprise) to install it.
:::
{%endemqxee%}

## Install EMQX with deb

1. To download [emqx-5.0.14-ubuntu20.04-amd64.deb](https://www.emqx.com/downloads/broker/v5.0.14/emqx-5.0.14-ubuntu20.04-amd64.deb), run:

```bash
wget https://www.emqx.com/downloads/broker/v5.0.14/emqx-5.0.14-ubuntu20.04-amd64.deb
```

2. To install EMQX, run:

```bash
sudo apt install ./emqx-5.0.14-ubuntu20.04-amd64.deb
```

3. We offered different options to start EMQX:

- To start EMQX directly, run:

  ```bash
  $ emqx start
  EMQX 5.0.14 is started successfully!

  $ emqx_ctl status
  Node 'emqx@127.0.0.1' 5.0.14 is started
  ```

- To start EMQX with systemctl, run:

  ```bash
  sudo systemctl start emqx
  ```

- To start EMQX as a service, run:

  ```bash
  sudo service emqx start
  ```

4. To uninstall EMQX, run:

  ```shell
  sudo apt remove --purge emqx
  ```

## Install EMQX with tag.gz

1. To download [emqx-5.0.14-ubuntu20.04-amd64.tar.gz](https://www.emqx.com/downloads/broker/v5.0.14/emqx-5.0.14-ubuntu20.04-amd64.tar.gz), run:

```bash
wget https://www.emqx.com/downloads/broker/v5.0.14/emqx-5.0.14-ubuntu20.04-amd64.tar.gz
```

1. To unzip the package, run:

```bash
mkdir -p emqx && tar -zxvf emqx-5.0.14-ubuntu20.04-amd64.tar.gz -C emqx
```

3. To start EMQX, run:

```bash
cd emqx && ./bin/emqx start
```
