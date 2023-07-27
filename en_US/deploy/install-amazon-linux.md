# Amazon Linux 2

This page guides you on installing and starting EMQX on Amazon Linux 2.

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
  
  $ emqx ctl status
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

1. Go to [EMQ Offical Site](https://www.emqx.com/en/try?product=enterprise&currentVersion=@EE_VERSION@&currentOS=AmazonLinux2=currentOS=AmazonLinux2&utm_source=docs.emqx.com&utm_medium=referral&utm_campaign=enterprise-docs-install-to-try-enterprise), select `@EE_VERSION@` for **Version** and `Amazon Linux 2` for **OS**, and click the **Download** button.

2. On the Downloads and Install page, select `rpm` as **Install Method** and select the proper **CPU Architecture**. Download and install the package according to the instruction.

### Start EMQX

EMQX offers 3 different options to start EMQX:

- To start EMQX directly, run:

  ```bash
  $ emqx start
  EMQX @EE_VERSION@ is started successfully!
  
  $ emqx ctl status
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


1. Go to [EMQ Offical Site](https://www.emqx.com/en/try?product=enterprise&currentVersion=@EE_VERSION@&currentOS=AmazonLinux2=currentOS=AmazonLinux2&utm_source=docs.emqx.com&utm_medium=referral&utm_campaign=enterprise-docs-install-to-try-enterprise), select `@EE_VERSION@` for **Version** and `Amazon Linux 2` for **OS**, and click the **Download** button.

2. On the Downloads and Install page, select `tar.gz` as **Install Method** and select the proper **CPU Architecture**. Download and install the package according to the instruction.

### Start EMQX

After the installation, run the command below to start EMQX.

```bash
./emqx/bin/emqx start
```

{% endemqxee %}
