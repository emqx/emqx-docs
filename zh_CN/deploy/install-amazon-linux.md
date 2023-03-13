# Amazon Linux 2

本章节将指导您如何在 Amazon Linux 2系统中下载安装并启动 EMQX。

{% emqxce %}

## 通过 rpm 安装

请根据 CPU 架构选择对应安装方式。

:::: tabs type:card

::: tab amd64

1. 下载 [emqx-5.0.17-amzn2-amd64.rpm](https://www.emqx.com/zh/downloads/broker/5.0.17/emqx-5.0.17-amzn2-amd64.rpm)。

   ```bash
   wget https://www.emqx.com/zh/downloads/broker/5.0.17/emqx-5.0.17-amzn2-amd64.rpm
   ```

2. 安装 EMQX。
   ```bash
   sudo yum install emqx-5.0.17-amzn2-amd64.rpm -y
   ```

:::

::: tab arm64
1. 下载 [emqx-5.0.17-amzn2-arm64.rpm](https://www.emqx.com/zh/downloads/broker/5.0.17/emqx-5.0.17-amzn2-arm64.rpm)。

   ```bash
   wget https://www.emqx.com/zh/downloads/broker/5.0.17/emqx-5.0.17-amzn2-arm64.rpm
   ```

2. 安装 EMQX。
   ```bash
   sudo yum install emqx-5.0.17-amzn2-arm64.rpm -y
   ```

:::

::::

安装完成后，可通过如下命令启动 EMQX。

  ```shell
  sudo systemctl start emqx
  ```

## 通过 tar.gz 安装

请根据 CPU 架构选择对应安装方式。

:::: tabs type:card

::: tab amd64

1. 下载 [emqx-5.0.17-elixir-amzn2-amd64.tar.gz](https://www.emqx.com/zh/downloads/broker/5.0.17/emqx-5.0.17-elixir-amzn2-amd64.tar.gz)。

   ```bash
   wget https://www.emqx.com/zh/downloads/broker/5.0.17/emqx-5.0.17-elixir-amzn2-amd64.tar.gz
   ```

2. 安装 EMQX。
   ```bash
   mkdir -p emqx && tar -zxvf emqx-5.0.17-elixir-amzn2-amd64.tar.gz -C emqx
   ```

:::

::: tab arm64
1. 下载 [emqx-5.0.17-amzn2-arm64.tar.gz](https://www.emqx.com/zh/downloads/broker/5.0.17/emqx-5.0.17-amzn2-arm64.tar.gz)。

   ```bash
   wget https://www.emqx.com/zh/downloads/broker/5.0.17/emqx-5.0.17-amzn2-arm64.tar.gz
   ```

2. 安装 EMQX。
   ```bash
   mkdir -p emqx && tar -zxvf emqx-5.0.17-amzn2-arm64.tar.gz -C emqx
   ```

:::

::::

安装完成后，可通过如下命令启动 EMQX。

```bash
./emqx/bin/emqx start
```

{% endemqxce %}

{% emqxee %}

## 通过 rpm 安装

请根据 CPU 架构选择对应安装方式。

:::: tabs type:card

::: tab amd64

1. 下载 [emqx-enterprise-5.0.0-amzn2-amd64.rpm](https://www.emqx.com/zh/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-amzn2-amd64.rpm)。

   ```bash
   wget https://www.emqx.com/zh/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-amzn2-amd64.rpm
   ```

2. 安装 EMQX。
   ```bash
   sudo yum install emqx-enterprise-5.0.0-amzn2-amd64.rpm -y
   ```

:::

::: tab arm64
1. 下载 [emqx-enterprise-5.0.0-amzn2-arm64.rpm](https://www.emqx.com/zh/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-amzn2-arm64.rpm)。

   ```bash
   wget https://www.emqx.com/zh/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-amzn2-arm64.rpm
   ```

2. 安装 EMQX。
   ```bash
   sudo yum install emqx-enterprise-5.0.0-amzn2-arm64.rpm -y
   ```

:::

::::

安装完成后，可通过如下命令启动 EMQX。

```bash
sudo systemctl start emqx
```



## 通过 tar.gz 安装

请根据 CPU 架构选择对应安装方式。

:::: tabs type:card

::: tab amd64

1. 下载 [emqx-enterprise-5.0.0-amzn2-amd64.tar.gz](https://www.emqx.com/zh/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-amzn2-amd64.tar.gz)。

   ```bash
   wget https://www.emqx.com/zh/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-amzn2-amd64.tar.gz
   ```

2. 安装 EMQX。

   ```bash
   mkdir -p emqx && tar -zxvf emqx-enterprise-5.0.0-amzn2-amd64.tar.gz -C emqx
   ```

:::

::: tab arm64

1. 下载 [emqx-enterprise-5.0.0-amzn2-arm64.tar.gz](https://www.emqx.com/zh/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-amzn2-arm64.tar.gz)。

```bash
wget https://www.emqx.com/zh/downloads/enterprise/5.0.0/emqx-enterprise-5.0.0-amzn2-arm64.tar.gz
```

2. 安装 EMQX。

```bash
mkdir -p emqx && tar -zxvf emqx-enterprise-5.0.0-amzn2-arm64.tar.gz -C emqx
```

:::

::::

启动 EMQX。

```bash
./emqx/bin/emqx start
```

{% endemqxee %}
