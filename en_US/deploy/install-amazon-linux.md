# Amazon Linux 2

This section guides you on installing and starting EMQX on Amazon Linux 2.

## Check your environment

Erlang VM that powers EMQX relies on system locale settings to enable Unicode support for [filenames](https://www.erlang.org/doc/apps/stdlib/unicode_usage.html#unicode-filenames) and [terminal IO](https://www.erlang.org/doc/apps/stdlib/unicode_usage.html#the-interactive-shell) in interactive Erlang shells, among other things.

Before starting EMQX, it is recommended to verify that UTF-8 locale is enabled in the system environment and enable it if it's not. This may be achieved with [`cloud-init`](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/amazon-linux-ami-basics.html#amazon-linux-cloud-init) configuration.

```bash
cat <<EOF | sudo tee /etc/cloud/cloud.cfg.d/99_locale.cfg
#cloud-config
locale: C.utf8
EOF
```

{% emqxce %}

## Install with rpm

EMQX offers different installation packages for different CPU architectures. 

:::: tabs type:card

::: tab amd64

1. Download [emqx-@CE_VERSION@-amzn2-amd64.rpm](https://www.emqx.com/en/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-amzn2-amd64.rpm). 

   ```bash
   wget https://www.emqx.com/en/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-amzn2-amd64.rpm
   ```

2. Install EMQX. 

   ```bash
   sudo yum install emqx-@CE_VERSION@-amzn2-amd64.rpm -y
   ```

:::

::: tab arm64

1. Download [emqx-@CE_VERSION@-amzn2-arm64.rpm](https://www.emqx.com/en/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-amzn2-arm64.rpm). 

   ```bash
   wget https://www.emqx.com/en/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-amzn2-arm64.rpm
   ```

2. Install EMQX. 

   ```bash
   sudo yum install emqx-@CE_VERSION@-amzn2-arm64.rpm -ys
   ```

:::

::::

### Start EMQX

EMQX offers 3 different options to start EMQX:

- To start EMQX directly, run:

  ```bash
  $ emqx start
  EMQX @CE_VERSION@ is started successfully!
  
  $ emqx_ctl status
  Node 'emqx@127.0.0.1' @CE_VERSION@ is started
  ```

- To start EMQX with systemctl, run:

  ```bash
  sudo systemctl start emqx
  ```

- To start EMQX as a service, run:

  ```bash
  sudo service emqx start
  ```

### Uninstall EMQX

To uninstall EMQX, run:

```
sudo apt remove --purge emqx
```

## Install with tar.gz

EMQX has offered unique installation packages for different CPU architectures. 

:::: tabs type:card

::: tab amd64

1. Download [emqx-@CE_VERSION@-elixir-amzn2-amd64.tar.gz](https://www.emqx.com/en/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-elixir-amzn2-amd64.tar.gz). 

   ```bash
   wget https://www.emqx.com/en/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-elixir-amzn2-amd64.tar.gz
   ```

2. Install EMQX. 

   ```bash
   mkdir -p emqx && tar -zxvf emqx-@CE_VERSION@-elixir-amzn2-amd64.tar.gz -C emqx
   ```

:::

::: tab arm64

1. Download [emqx-@CE_VERSION@-amzn2-arm64.tar.gz](https://www.emqx.com/en/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-amzn2-arm64.tar.gz). 

   ```bash
   wget https://www.emqx.com/en/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-amzn2-arm64.tar.gz
   ```

2. Install EMQX.

   ```bash
   mkdir -p emqx && tar -zxvf emqx-@CE_VERSION@-amzn2-arm64.tar.gz -C emqx
   ```

:::

::::

### Start EMQX

After the installation, run the command below to start EMQX.

```bash
./emqx/bin/emqx start
```

{% endemqxce %}

{% emqxee %}

## Install with rpm

EMQX offers different installation packages for different CPU architectures. 

:::: tabs type:card

::: tab amd64

1. Download [emqx-enterprise-@EE_VERSION@-amzn2-amd64.rpm](https://www.emqx.com/en/downloads/enterprise/@EE_VERSION@/emqx-enterprise-@EE_VERSION@-amzn2-amd64.rpm). 

   ```bash
   wget https://www.emqx.com/en/downloads/enterprise/@EE_VERSION@/emqx-enterprise-@EE_VERSION@-amzn2-amd64.rpm
   ```

2. Install EMQX. 

   ```bash
   sudo yum install emqx-enterprise-@EE_VERSION@-amzn2-amd64.rpm -y
   ```

:::

::: tab arm64

1. Download [emqx-enterprise-@EE_VERSION@-amzn2-arm64.rpm](https://www.emqx.com/en/downloads/enterprise/@EE_VERSION@/emqx-enterprise-@EE_VERSION@-amzn2-arm64.rpm). 

   ```bash
   wget https://www.emqx.com/en/downloads/enterprise/@EE_VERSION@/emqx-enterprise-@EE_VERSION@-amzn2-arm64.rpm
   ```

2. Install EMQX.

   ```bash
   sudo yum install emqx-enterprise-@EE_VERSION@-amzn2-arm64.rpm -y
   ```

:::

::::

### Start EMQX

EMQX offers 3 different options to start EMQX:

- To start EMQX directly, run:

  ```bash
  $ emqx start
  EMQX @EE_VERSION@ is started successfully!
  
  $ emqx_ctl status
  Node 'emqx@127.0.0.1' @EE_VERSION@ is started
  ```

- To start EMQX with systemctl, run:

  ```bash
  sudo systemctl start emqx
  ```

- To start EMQX as a service, run:

  ```bash
  sudo service emqx start
  ```

### Uninstall EMQX

To uninstall EMQX, run:

```
sudo apt remove --purge emqx
```

## Install with tar.gz

EMQX has offered different installation packages for different CPU architectures. 

:::: tabs type:card

::: tab amd64

1. Download [emqx-enterprise-@EE_VERSION@-amzn2-amd64.tar.gz](https://www.emqx.com/en/downloads/enterprise/@EE_VERSION@/emqx-enterprise-@EE_VERSION@-amzn2-amd64.tar.gz). 

   ```bash
   wget https://www.emqx.com/en/downloads/enterprise/@EE_VERSION@/emqx-enterprise-@EE_VERSION@-amzn2-amd64.tar.gz
   ```

2. Install EMQX.

   ```bash
   mkdir -p emqx && tar -zxvf emqx-enterprise-@EE_VERSION@-amzn2-amd64.tar.gz -C emqx
   ```

:::

::: tab arm64

1. Download [emqx-enterprise-@EE_VERSION@-amzn2-arm64.tar.gz](https://www.emqx.com/en/downloads/enterprise/@EE_VERSION@/emqx-enterprise-@EE_VERSION@-amzn2-arm64.tar.gz). 

   ```bash
   wget https://www.emqx.com/en/downloads/enterprise/@EE_VERSION@/emqx-enterprise-@EE_VERSION@-amzn2-arm64.tar.gz
   ```

2. Install EMQX.

   ```bash
   mkdir -p emqx && tar -zxvf emqx-enterprise-@EE_VERSION@-amzn2-arm64.tar.gz -C emqx
   ```

:::

::::

### Start EMQX

After the installation, run the command below to start EMQX.

```
./emqx/bin/emqx start
```

{% endemqxee %}