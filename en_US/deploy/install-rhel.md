{% emqxee %}
# Install EMQX Enterprise on CentOS/RHEL
{% endemqxee %}

{% emqxce %}
# Install EMQX on CentOS/RHEL
{% endemqxce %}


This page guides you on installing and starting EMQX on CentOS/RHEL system.

Supported versions:

- Amazon Linux 2023
- Amazon Linux 2
- Rocky Linux 9 (RHEL 9)
- Rocky Linux 8 (RHEL 8)
- CentOS 7 (RHEL 7)

{% emqxce %}

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

This section takes CentOS 8 as an example to illustrate how to download the latest version of EMQX. For other system versions, please visit the [EMQX Deployment page](https://www.emqx.io/downloads?os=CentOS). 

:::: tabs type:card

::: tab amd64

1. Download [emqx-@CE_VERSION@-el8-amd64.rpm](https://www.emqx.com/en/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-el8-amd64.rpm). 

   ```bash
   wget https://www.emqx.com/en/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-el8-amd64.rpm
   ```

2. Install EMQX. 

   ```bash
   sudo yum install emqx-@CE_VERSION@-el8-amd64.rpm -y
   ```

:::

::: tab arm64

1. Download [emqx-@CE_VERSION@-el8-arm64.rpm](https://www.emqx.com/en/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-el8-arm64.rpm). 

   ```bash
   wget https://www.emqx.com/en/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-el8-arm64.rpm
   ```

2. Install EMQX. 

   ```bash
   sudo yum install emqx-@CE_VERSION@-el8-arm64.rpm -y
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
sudo yum remove emqx
```

## Install with tar.gz

EMQX offers tar.gz installation packages for different CPU architectures.

This section takes CentOS 8 as an example to illustrate how to download the latest version of EMQX. For other versions, please visit the [EMQX Deployment page](https://www.emqx.io/downloads?os=CentOS). 

:::: tabs type:card

::: tab amd64

1. Download [emqx-@CE_VERSION@-el8-amd64.tar.gz](https://www.emqx.com/en/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-el8-amd64.tar.gz). 

   ```bash
   wget https://www.emqx.com/en/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-el8-amd64.tar.gz
   ```

2. Install EMQX.

   ```bash
   mkdir -p emqx && tar -zxvf emqx-@CE_VERSION@-el8-amd64.tar.gz -C emqx
   ```

:::

::: tab arm64

1. Download [emqx-@CE_VERSION@-el8-arm64.tar.gz](https://www.emqx.com/en/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-el8-arm64.tar.gz). 

   ```bash
   wget https://www.emqx.com/en/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-el8-arm64.tar.gz
   ```

2. Install EMQX.

   ```bash
   mkdir -p emqx && tar -zxvf emqx-@CE_VERSION@-el8-arm64.tar.gz -C emqx
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

1. Go to [EMQ Official Site](https://www.emqx.com/en/try?product=enterprise&currentVersion=@EE_VERSION@&currentOS=CentOS=currentOS=Centos8&utm_source=docs.emqx.com&utm_medium=referral&utm_campaign=enterprise-docs-install-to-try-enterprise).
2. Select `@EE_VERSION@` for **Version** and `CentOS` for **OS**, and click the **Download** button.
3. On the Downloads and Install page, select `rpm` as the **Install Method** and select the proper **CPU Architecture** that matches your system. Download and install the package according to the instructions.

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
sudo yum remove emqx
```

## Install with tar.gz

1. Go to [EMQ Official Site](https://www.emqx.com/en/try?product=enterprise&currentVersion=@EE_VERSION@&currentOS=CentOS=currentOS=Centos8&utm_source=docs.emqx.com&utm_medium=referral&utm_campaign=enterprise-docs-install-to-try-enterprise).
2. Select `@EE_VERSION@` for **Version** and `CentOS` for **OS**, and click the **Download** button.
3. On the Downloads and Install page, select `tar.gz` as the **Install Method** and select the proper **CPU Architecture** that matches your system. Download and install the package according to the instruction.

### Start EMQX

After the installation, run the command below to start EMQX.

```bash
./emqx/bin/emqx start
```

{% endemqxee %}
