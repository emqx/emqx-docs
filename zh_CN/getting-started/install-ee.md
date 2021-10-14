---
enterprise: true
---
# EMQ X 企业版安装

EMQ X 消息服务器可跨平台运行在 Linux、FreeBSD、macOS 或 openSUSE 服务器上。

::: tip
产品部署建议 Linux 服务器，不推荐 Windows 服务器。
:::

## EMQ X License 文件获取

EMQ X 企业版内置了用于评估测试的 License（禁止用于生产环境），限制为 10 个客户端连接。可以联系商务或登陆 https://www.emqx.com/zh/apply-licenses/emqx 注册账号获取更大规格免费的试用 License。

:::: tabs type:card

::: tab Docker
## Docker

1. 获取 docker 镜像

- 通过 [Docker Hub](https://hub.docker.com/r/emqx/emqx-ee) 获取

```bash
docker pull emqx/emqx-ee:latest
```

- 通过 [emqx.com](https://www.emqx.com/en/downloads/enterprise) 手动下载 docker 镜像，并手动加载

```bash
wget -O emqx-ee-docker.zip https://www.emqx.com/en/downloads/enterprise/v4.3.4/emqx-ee-docker-4.3.4-alpine-amd64.zip
unzip emqx-ee-docker.zip
docker load < emqx-ee-docker
```

2. 启动 docker 容器

```bash
docker run -d \
--name emqx-ee \
-p 1883:1883 \
-p 8081:8081 \
-p 8083:8083 \
-p 8084:8084 \
-p 8883:8883 \
-p 18083:18083 \
emqx/emqx-ee:latest
```

更多关于 EMQ X Docker 的信息请查看 [Docker Hub](https://hub.docker.com/r/emqx/emqx-ee)

:::

::: tab Centos

## CentOS

- CentOS6.X
- CentOS7.X
- CentOS8.X

### 使用储存库安装 EMQ X

1. 删除旧的 EMQ X

```bash
sudo yum remove emqx emqx-edge emqx-ee
```

2. 安装所需要的依赖包

```bash
sudo yum install -y yum-utils device-mapper-persistent-data lvm2
```

3. 使用以下命令设置存储库，以 CentOS7为例

```bash
sudo yum-config-manager --add-repo https://repos.emqx.io/emqx-ee/redhat/centos/7/emqx-ee.repo
```

4. 安装最新版本的 EMQ X

```bash
sudo yum install emqx-ee
```

::: tip
如果提示接受 GPG 密钥，请确认密钥符合 fc84 1ba6 3775 5ca8 487b 1e3c c0b4 0946 3e640d53，如果符合，则接受该指纹。
:::

5. 安装特定版本的 EMQ X

- 查询可用版本

```bash
yum list emqx-ee --showduplicates | sort -r

emqx-ee.x86_64                    4.0.0-1.el7                     emqx-ee-stable
```

- 根据第二列中的版本字符串安装特定版本，例如 4.0.0

```bash
sudo yum install emqx-ee-4.0.0
```

6. 启动 EMQ X

- 直接启动

```bash
emqx start
emqx v4.0.0 is started successfully!

emqx_ctl status
Node 'emqx@127.0.0.1' is started
emqx 4.0.0 is running
```

- systemctl 启动

```bash
sudo systemctl start emqx
```

- service 启动

```bash
sudo service emqx start
```

### 使用 rpm 包安装 EMQ X

1. 通过 [emqx.com](https://www.emqx.com/zh/downloads?product=enterprise) 选择 CentOS 版本，然后下载要安装的 EMQ X 版本的 rpm 包。

2. 安装 EMQ X

```bash
sudo rpm -ivh emqx-ee-centos7-v4.0.0.x86_64.rpm
```

3. 启动 EMQ X

- 直接启动

```bash
emqx start
emqx  is started successfully!

emqx_ctl status
Node 'emqx@127.0.0.1' is started
emqx 4.0.0 is running
```

- systemctl 启动

```bash
sudo systemctl start emqx
```

- service 启动

```bash
sudo service emqx start
```

### 使用 zip 包安装 EMQ X

1. 通过 [emqx.com](https://www.emqx.com/zh/downloads?product=enterprise) 选择 CentOS 版本，然后下载要安装的 EMQ X 版本的 zip 包。

2. 解压程序包

```bash
unzip emqx-ee-centos7-v4.0.0.zip
```

3. 启动 EMQ X

```bash
./bin/emqx start
emqx v4.0.0 is started successfully!

./bin/emqx_ctl status
Node 'emqx@127.0.0.1' is started
emqx 4.0.0 is running
```

:::

::: tab Ubuntu
## Ubuntu

- Focal Fossa 20.04 (LTS)
- Bionic 18.04 (LTS)
- Xenial 16.04 (LTS)

### 使用储存库安装 EMQ X

1. 删除旧的 EMQ X

```bash
sudo apt remove emqx emqx-edge emqx-ee
```

2. 安装所需要的依赖包

```bash
sudo apt update && sudo apt install -y \
apt-transport-https \
ca-certificates \
curl \
gnupg-agent \
software-properties-common
```

3. 添加 EMQ X 的官方 GPG 密钥

```bash
curl -fsSL https://repos.emqx.io/gpg.pub | sudo apt-key add -
```

验证密钥

```bash
sudo apt-key fingerprint 3E640D53

pub   rsa2048 2019-04-10 [SC]
FC84 1BA6 3775 5CA8 487B  1E3C C0B4 0946 3E64 0D53
uid           [ unknown] emqx team <support@emqx.io>
```

4. 添加 EMQ X 存储库。

```bash
sudo add-apt-repository \
"deb [arch=amd64] https://repos.emqx.io/emqx-ee/deb/ubuntu/ \
$(lsb_release -cs) \
stable"
```

5. 更新 apt 包索引

```bash
sudo apt update
```

6. 安装最新版本的 EMQ X

```bash
sudo apt install emqx-ee
```

7. 安装特定版本的 EMQ X

- 查询可用版本

```bash
sudo apt-cache madison emqx-ee

emqx-ee |      4.0.0 | https://repos.emqx.io/emqx-ee/deb/ubuntu bionic/stable amd64 Packages
```

- 使用第二列中的版本字符串安装特定版本，例如

```bash
sudo apt install emqx-ee=4.0.0
```


8. 启动 EMQ X

- 直接启动

```bash
emqx start
emqx v4.0.0 is started successfully!

emqx_ctl status
Node 'emqx@127.0.0.1' is started
emqx 4.0.0 is running
```

- systemctl 启动

```bash
sudo systemctl start emqx
```

- service 启动

```bash
sudo service emqx start
```

### 使用 deb 包安装 EMQ X

1. 通过 [emqx.com](https://www.emqx.com/zh/downloads?product=enterprise) 选择 Ubuntu
版本，然后下载要安装的 EMQ X 版本的 deb 包。

2. 安装 EMQ X

```bash
# for ubuntu
sudo apt install ./emqx-ee-ubuntu18.04-v3.1.0_amd64.deb
# for debian
sudo dpkg -i emqx-ee-ubuntu18.04-v3.1.0_amd64.deb
```

3. 启动 EMQ X

- 直接启动

```bash
emqx start
emqx  is started successfully!

emqx_ctl status
Node 'emqx@127.0.0.1' is started
emqx 4.0.0 is running
```

- systemctl 启动

```bash
sudo systemctl start emqx
```

- service 启动

```bash
sudo service emqx start
```

### 使用 zip 包安装 EMQ X

1. 通过 [emqx.com](https://www.emqx.com/zh/downloads?product=enterprise) 选择 Ubuntu
版本，然后下载要安装的 EMQ X 版本的 zip 包。

2. 解压程序包

```bash
unzip emqx-ee-ubuntu18.04-v4.0.0.zip
```

3. 导入License文件:

```bash
cp /path/to/emqx.lic /path/to/emqx/etc/emqx.lic
```

4. 启动 EMQ X

```bash
./bin/emqx start
emqx v4.0.0 is started successfully!

./bin/emqx_ctl status
Node 'emqx@127.0.0.1' is started
emqx 4.0.0 is running
```

:::

::: tab Debian
## Debian 

- Stretch (Debian 9)
- Jessie (Debian 8)

### 使用储存库安装 EMQ X

1. 删除旧的 EMQ X

```bash
sudo apt remove emqx emqx-edge emqx-ee
```

2. 安装所需要的依赖包

```bash
sudo apt update && sudo apt install -y \
apt-transport-https \
ca-certificates \
curl \
gnupg-agent \
software-properties-common
```

3. 添加 EMQ X 的官方 GPG 密钥

```bash
curl -fsSL https://repos.emqx.io/gpg.pub | sudo apt-key add -
```

验证密钥

```bash
sudo apt-key fingerprint 3E640D53

pub   rsa2048 2019-04-10 [SC]
FC84 1BA6 3775 5CA8 487B  1E3C C0B4 0946 3E64 0D53
uid           [ unknown] emqx team <support@emqx.io>
```

4. 设置 EMQ X 存储库。

```bash
sudo add-apt-repository \
"deb [arch=amd64] https://repos.emqx.io/emqx-ee-ce/deb/debian/ \
$(lsb_release -cs) \
stable"
```

5. 更新 apt 包索引

```bash
sudo apt update
```

6. 安装最新版本的 EMQ X

```bash
sudo apt install emqx-ee
```

7. 安装特定版本的 EMQ X

- 查询可用版本

```bash
sudo apt-cache madison emqx-ee

emqx-ee |      4.0.0 | https://repos.emqx.io/emqx-ee/deb/ubuntu bionic/stable amd64 Packages
```

- 使用第二列中的版本字符串安装特定版本，例如

```bash
sudo apt install emqx-ee=4.0.0
```

8. 启动 EMQ X

- 直接启动

```bash
emqx start
emqx v4.0.0 is started successfully!

emqx_ctl status
Node 'emqx@127.0.0.1' is started
emqx 4.0.0 is running
```

- systemctl 启动

```bash
sudo systemctl start emqx
```

- service 启动

```bash
sudo service emqx start
```

### 使用 deb 包安装 EMQ X

1. 通过 [emqx.com](https://www.emqx.com/zh/downloads?product=enterprise) 选择 Debian 版本，然后下载要安装的 EMQ X 版本的 deb 包。

2. 安装 EMQ X

```bash
# for ubuntu
sudo apt install ./emqx-ee-debian9-v3.1.0_amd64.deb

# for debian 
# 首先确保已安装 libodbc 
sudo dpkg -i emqx-ee-debian9-v3.1.0_amd64.deb
```


3. 启动 EMQ X

- 直接启动

```bash
emqx start
emqx v4.0.0 is started successfully!

emqx_ctl status
Node 'emqx@127.0.0.1' is started
emqx 4.0.0 is running
```

- systemctl 启动

```bash
sudo systemctl start emqx
```

- service 启动

```bash
sudo service emqx start
```

### 使用 zip 包安装 EMQ X

1. 通过 [emqx.com](https://www.emqx.com/zh/downloads?product=enterprise) 选择 Debian
版本，然后下载要安装的 EMQ X 版本的 zip 包。

2. 解压程序包

```bash
unzip emqx-ee-debian9-v4.0.0.zip
```

3. 启动 EMQ X

```bash
./bin/emqx start
emqx v4.0.0 is started successfully!

./bin/emqx_ctl status
Node 'emqx@127.0.0.1' is started
emqx 4.0.0 is running
```

:::

::: tab macOS

## macOS

### 使用 ZIP 包安装 EMQ X

1. 通过 [emqx.com](https://www.emqx.com/zh/downloads?product=enterprise) ，选择 EMQ X
版本，然后下载要安装的 zip 包。

2. 解压压缩包

```bash
unzip emqx-ee-macos-v4.0.0.zip
```

3. 启动 EMQ X

```bash
./bin/emqx start
emqx v4.0.0 is started successfully!

./bin/emqx_ctl status
Node 'emqx@127.0.0.1' is started
emqx 4.0.0 is running
```
:::

::::