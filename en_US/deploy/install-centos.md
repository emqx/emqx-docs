# CentOS/RHEL

This section will guide you on how to install and start EMQX on CentOS/RHEL system.

Supported versions: 

- CentOS 7
- CentOS 8

{% emqxce %}

## Install EMQX with rpm

1. To download [emqx-5.0.14-el8-amd64.rpm](https://www.emqx.com/downloads/broker/v5.0.14/emqx-5.0.14-el8-amd64.rpm), run:

```bash
wget https://www.emqx.com/downloads/broker/v5.0.14/emqx-5.0.14-el8-amd64.rpm
```

2. To install EMQX, run:

```bash
sudo yum install ./emqx-5.0.14-el8-amd64.rpm -y
```

3. To run EMQX, run:

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
  sudo yum remove emqx
  ```

## Install EMQX with tag.gz

1. To download [emqx-5.0.14-el8-amd64.tar.gz](https://www.emqx.com/downloads/broker/v5.0.14/emqx-5.0.14-el8-amd64.tar.gz), run:

```bash
wget https://www.emqx.com/downloads/broker/v5.0.14/emqx-5.0.14-el8-amd64.tar.gz
```

2. To unzip the package, run:

```bash
mkdir -p emqx && tar -zxvf emqx-5.0.14-el8-amd64.tar.gz -C emqx
```

3. To start EMQX, run:

```bash
cd emqx && ./bin/emqx start
```


{% endemqxce %}

{% emqxee %}

## Install EMQX with rpm

1. To download [emqx-ee-5.0.0-el8-amd64.rpm](https://www.emqx.com/downloads/enterprise/5.0.0/emqx-ee-5.0.0-el8-amd64.rpm), run:

```bash
wget https://www.emqx.com/downloads/enterprise/5.0.0/emqx-ee-5.0.0-el8-amd64.rpm
```

2. To install EMQX, run:

```bash
sudo yum install ./emqx-ee-5.0.0-el8-amd64.rpm -y
```

3. To run EMQX, run:

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
  sudo yum remove emqx
  ```

## Install EMQX with tag.gz

1. To download [emqx-ee-5.0.0-el8-amd64.tar.gz](https://www.emqx.com/downloads/enterprise/5.0.0/emqx-ee-5.0.0-el8-amd64.tar.gz), run:

```bash
wget https://www.emqx.com/downloads/enterprise/5.0.0/emqx-ee-5.0.0-el8-amd64.tar.gz
```

2. To unzip the package, run:

```bash
mkdir -p emqx && tar -zxvf emqx-ee-5.0.0-el8-amd64.tar.gz -C emqx
```

3. To start EMQX, run:

```bash
cd emqx && ./bin/emqx start
```

{% endemqxee %}