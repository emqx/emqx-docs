# EMQX 企业版安装

EMQX 消息服务器可跨平台运行在 Linux、FreeBSD、macOS 或 openSUSE 服务器上。

## EMQX License 文件获取

联系商务或登陆 https://emqx.io 注册账号获取免费的试用 License 文件

## EMQX 程序包下载

EMQX 消息服务器每个版本会发布 CentOS、Ubuntu、Debian、FreeBSD 、openSUSE
平台程序包与 Docker 镜像。

下载地址: <https://www.emqx.com/zh/downloads?product=enterprise>


## CentOS

  - CentOS6.X
  - CentOS7.X
  - CentOS8.X

### 使用储存库安装 EMQX

1.  删除旧的 EMQX
    
```bash
$ sudo yum remove emqx emqx-edge emqx-ee
```

2.  安装所需要的依赖包
    
```bash
$ sudo yum install -y yum-utils device-mapper-persistent-data lvm2
```

3.  使用以下命令设置存储库，以 CentOS7
    为例
    
```bash
$ sudo yum-config-manager --add-repo https://repos.emqx.io/emqx-ee/redhat/centos/7/emqx-ee.repo
```

4.  安装最新版本的 EMQX
    
```bash
$ sudo yum install emqx-ee
```

::: tip
如果提示接受 GPG 密钥，请确认密钥符合 fc84 1ba6 3775 5ca8 487b 1e3c c0b4 0946 3e64
0d53，如果符合，则接受该指纹。
:::

5.  安装特定版本的 EMQX
    
    1.  查询可用版本
        
        ```bash
        $ yum list emqx-ee --showduplicates | sort -r
        
        emqx-ee.x86_64                    4.0.0-1.el7                     emqx-ee-stable
        ```
    
    2.  根据第二列中的版本字符串安装特定版本，例如 4.0.0
        
        ```bash
        $ sudo yum install emqx-ee-4.0.0
        ```

6.  导入License文件:
    
    ```bash
    $ cp /path/to/emqx.lic /etc/emqx/emqx.lic
    ```

7.  启动 EMQX
    
      - 直接启动
        
        ```bash
        $ emqx start
        emqx v4.0.0 is started successfully!
        
        $ emqx_ctl status
        Node 'emqx@127.0.0.1' is started
        emqx 4.0.0 is running
        ```
    
      - systemctl 启动
        
        ```bash
        $ sudo systemctl start emqx
        ```
    
      - service 启动
        
        ```bash
        $ sudo service emqx start
        ```

### 使用 rpm 包安装 EMQX

1.  通过 [emqx.io](https://www.emqx.com/zh/downloads?product=enterprise) 选择 CentOS
    版本，然后下载要安装的 EMQX 版本的 rpm 包。

2.  安装 EMQX
    
    ```bash
    $ sudo rpm -ivh emqx-ee-centos7-v4.0.0.x86_64.rpm
    ```

3.  导入License文件:
    
    ```bash
    $ cp /path/to/emqx.lic /etc/emqx/emqx.lic
    ```

4.  启动 EMQX
    
      - 直接启动
        
        ```bash
        $ emqx start
        emqx  is started successfully!
        
        $ emqx_ctl status
        Node 'emqx@127.0.0.1' is started
        emqx 4.0.0 is running
        ```
    
      - systemctl 启动
        
        ```bash
        $ sudo systemctl start emqx
        ```
    
      - service 启动
        
        ```bash
        $ sudo service emqx start
        ```

### 使用 zip 包安装 EMQX

1.  通过 [emqx.io](https://www.emqx.com/zh/downloads?product=enterprise) 选择 Centos
    版本，然后下载要安装的 EMQX 版本的 zip 包。

2.  解压程序包
    
    ```bash
    $ unzip emqx-ee-centos7-v4.0.0.zip
    ```

3.  导入License文件:
    
    ```bash
    $ cp /path/to/emqx.lic /path/to/emqx/etc/emqx.lic
    ```

4.  启动 EMQX
    
    ```bash
    $ ./bin/emqx start
    emqx v4.0.0 is started successfully!
    
    $ ./bin/emqx_ctl status
    Node 'emqx@127.0.0.1' is started
    emqx 4.0.0 is running
    ```



## Ubuntu

  - Bionic 18.04 (LTS)
  - Xenial 16.04 (LTS)
  - Trusty 14.04 (LTS)
  - Precise 12.04 (LTS)

### 使用储存库安装 EMQX

1.  删除旧的 EMQX

```bash
$ sudo apt remove emqx emqx-edge emqx-ee
```

2.  安装所需要的依赖包
    
```bash
$ sudo apt update && sudo apt install -y \
    apt-transport-https \
    ca-certificates \
    curl \
    gnupg-agent \
    software-properties-common
```

3.  添加 EMQX 的官方 GPG 密钥
    
```bash
$ curl -fsSL https://repos.emqx.io/gpg.pub | sudo apt-key add -
```

验证密钥

```bash
$ sudo apt-key fingerprint 3E640D53

pub   rsa2048 2019-04-10 [SC]
    FC84 1BA6 3775 5CA8 487B  1E3C C0B4 0946 3E64 0D53
uid           [ unknown] emqx team <support@emqx.io>
```

4.  添加 EMQX 存储库。
    
```bash
$ sudo add-apt-repository \
    "deb [arch=amd64] https://repos.emqx.io/emqx-ee/deb/ubuntu/ \
    $(lsb_release -cs) \
    stable"
```

5.  更新 apt 包索引
    
```bash
$ sudo apt update
```

6.  安装最新版本的 EMQX

```bash
$ sudo apt install emqx-ee
```

7.  安装特定版本的 EMQX
    
    1.  查询可用版本
        
    ```bash
    $ sudo apt-cache madison emqx-ee
    
    emqx-ee |      4.0.0 | https://repos.emqx.io/emqx-ee/deb/ubuntu bionic/stable amd64 Packages
    ```

    2.  使用第二列中的版本字符串安装特定版本，例如
    
    ```bash
    $ sudo apt install emqx-ee=4.0.0
    ```

8.  导入License文件:
    
    ```bash
    $ cp /path/to/emqx.lic /etc/emqx/emqx.lic
    ```

9.  启动 EMQX
    
    - 直接启动
    
    ```bash
    $ emqx start
    emqx v4.0.0 is started successfully!
    
    $ emqx_ctl status
    Node 'emqx@127.0.0.1' is started
    emqx 4.0.0 is running
    ```

    - systemctl 启动
    
    ```bash
    $ sudo systemctl start emqx
    ```

    - service 启动
    
    ```bash
    $ sudo service emqx start
    ```

### 使用 deb 包安装 EMQX

1.  通过 [emqx.io](https://www.emqx.com/zh/downloads?product=enterprise) 选择 Ubuntu
    版本，然后下载要安装的 EMQX 版本的 deb 包。

2.  安装 EMQX
    
```bash
# for ubuntu
$ sudo apt install ./emqx-ee-ubuntu18.04-v3.1.0_amd64.deb
# for debian
$ sudo dpkg -i emqx-ee-ubuntu18.04-v3.1.0_amd64.deb
```

3.  导入License文件:
    
```bash
$ cp /path/to/emqx.lic /etc/emqx/emqx.lic
```

4.  启动 EMQX
    
- 直接启动

```bash
$ emqx start
emqx  is started successfully!

$ emqx_ctl status
Node 'emqx@127.0.0.1' is started
emqx 4.0.0 is running
```

- systemctl 启动

```bash
$ sudo systemctl start emqx
```

- service 启动

```bash
$ sudo service emqx start
```

### 使用 zip 包安装 EMQX

1.  通过 [emqx.io](https://www.emqx.com/zh/downloads?product=enterprise) 选择 Ubuntu
    版本，然后下载要安装的 EMQX 版本的 zip 包。

2.  解压程序包

```bash
$ unzip emqx-ee-ubuntu18.04-v4.0.0.zip
```

3.  导入License文件:
    
```bash
$ cp /path/to/emqx.lic /path/to/emqx/etc/emqx.lic
```

4.  启动 EMQX
    
```bash
$ ./bin/emqx start
emqx v4.0.0 is started successfully!

$ ./bin/emqx_ctl status
Node 'emqx@127.0.0.1' is started
emqx 4.0.0 is running
```


## Debian 

  - Stretch (Debian 9)
  - Jessie (Debian 8)

### 使用储存库安装 EMQX

1.  删除旧的 EMQX
    
```bash
$ sudo apt remove emqx emqx-edge emqx-ee
```

2.  安装所需要的依赖包
    
```bash
$ sudo apt update && sudo apt install -y \
    apt-transport-https \
    ca-certificates \
    curl \
    gnupg-agent \
    software-properties-common
```

3.  添加 EMQX 的官方 GPG 密钥
    
```bash
$ curl -fsSL https://repos.emqx.io/gpg.pub | sudo apt-key add -
```
    
验证密钥

```bash
$ sudo apt-key fingerprint 3E640D53

pub   rsa2048 2019-04-10 [SC]
    FC84 1BA6 3775 5CA8 487B  1E3C C0B4 0946 3E64 0D53
uid           [ unknown] emqx team <support@emqx.io>
```

4.  设置 EMQX 存储库。
    
```bash
$ sudo add-apt-repository \
    "deb [arch=amd64] https://repos.emqx.io/emqx-ee-ce/deb/debian/ \
    $(lsb_release -cs) \
    stable"
```

5.  更新 apt 包索引
    
```bash
$ sudo apt update
```

6.  安装最新版本的 EMQX

```bash
$ sudo apt install emqx-ee
```

7.  安装特定版本的 EMQX
    
    1.  查询可用版本
        
        ```bash
        $ sudo apt-cache madison emqx-ee
        
        emqx-ee |      4.0.0 | https://repos.emqx.io/emqx-ee/deb/ubuntu bionic/stable amd64 Packages
        ```
    
    2.  使用第二列中的版本字符串安装特定版本，例如
        
        ```bash
        $ sudo apt install emqx-ee=4.0.0
        ```

8.  导入License文件:
    
    ```bash
    $ cp /path/to/emqx.lic /etc/emqx/emqx.lic
    ```

9.  启动 EMQX
    
    - 直接启动
    
    ```bash
    $ emqx start
    emqx v4.0.0 is started successfully!
    
    $ emqx_ctl status
    Node 'emqx@127.0.0.1' is started
    emqx 4.0.0 is running
    ```

    - systemctl 启动
    
    ```bash
    $ sudo systemctl start emqx
    ```

    - service 启动
    
    ```bash
    $ sudo service emqx start
    ```

### 使用 deb 包安装 EMQX

1.  通过 [emqx.io](https://www.emqx.com/zh/downloads?product=enterprise) 选择 Debian
    版本，然后下载要安装的 EMQX 版本的 deb 包。

2.  安装 EMQX
    
    ```bash
    # for ubuntu
    $ sudo apt install ./emqx-ee-debian9-v3.1.0_amd64.deb
    
    # for debian 
    # 首先确保已安装 libodbc 
    $ sudo dpkg -i emqx-ee-debian9-v3.1.0_amd64.deb
    ```

3.  导入License文件:
    
    ```bash
    $ cp /path/to/emqx.lic /etc/emqx/emqx.lic
    ```

4.  启动 EMQX
    
      - 直接启动
        
        ```bash
        $ emqx start
        emqx v4.0.0 is started successfully!
        
        $ emqx_ctl status
        Node 'emqx@127.0.0.1' is started
        emqx 4.0.0 is running
        ```
    
      - systemctl 启动
        
        ```bash
        $ sudo systemctl start emqx
        ```
    
      - service 启动
        
        ```bash
        $ sudo service emqx start
        ```

### 使用 zip 包安装 EMQX

1.  通过 [emqx.io](https://www.emqx.com/zh/downloads?product=enterprise) 选择 Debian
    版本，然后下载要安装的 EMQX 版本的 zip 包。

2.  解压程序包
    
    ```bash
    $ unzip emqx-ee-debian9-v4.0.0.zip
    ```

3.  导入License文件:
    
    ```bash
    $ cp /path/to/emqx.lic /path/to/emqx/etc/emqx.lic
    ```

4.  启动 EMQX
    
    ```bash
    $ ./bin/emqx start
    emqx v4.0.0 is started successfully!
    
    $ ./bin/emqx_ctl status
    Node 'emqx@127.0.0.1' is started
    emqx 4.0.0 is running
    ```


## macOS

### 使用 ZIP 包安装 EMQX

1.  通过 [emqx.io](https://www.emqx.com/zh/downloads?product=enterprise) ，选择 EMQX
    版本，然后下载要安装的 zip 包。

2.  解压压缩包
    
    ```bash
    $ unzip emqx-ee-macos-v4.0.0.zip
    ```

3.  导入License文件:
    
    ```bash
    $ cp /path/to/emqx.lic /path/to/emqx/etc/emqx.li
    ```

4.  启动 EMQX
    
    ```bash
    $ ./bin/emqx start
    emqx v4.0.0 is started successfully!
    
    $ ./bin/emqx_ctl status
    Node 'emqx@127.0.0.1' is started
    emqx 4.0.0 is running
    ```


## openSUSE

  - openSUSE leap

### 使用储存库安装 EMQX

1.  删除旧的 EMQX
    
    ```bash
    $ sudo zypper remove emqx emqx-edge emqx-ee
    ```

2.  下载 GPG 公钥并导入。
    
    ```bash
    $ curl -L -o /tmp/gpg.pub https://repos.emqx.io/gpg.pub
    $ sudo rpmkeys --import /tmp/gpg.pub
    ```

3.  添加储存库地址
    
    ```bash
    $ sudo zypper ar -f -c https://repos.emqx.io/emqx-ee/redhat/opensuse/leap/stable emqx-ee
    ```

4.  安装最新版本的 EMQX
    
    ```bash
    $ sudo zypper in emqx-ee
    ```

5.  安装特定版本的 EMQX
    
    1.  查询可用版本
        
        ```bash
        $ sudo zypper pa emqx-ee
        
        Loading repository data...
        Reading installed packages...
        S | Repository | Name    | Version | Arch
        --+------------+---------+---------+-------
          | emqx-ee    | emqx-ee | 4.0.0-1 | x86_64
        ```
    
    2.  使用 Version 安装特定版本，例如
        
        ```bash
        $ sudo zypper in emqx-ee-4.0.0
        ```

6.  导入License文件:
    
    ```bash
    $ cp /path/to/emqx.lic /etc/emqx/emqx.lic
    ```

7.  启动 EMQX
    
      - 直接启动
        
        ```bash
        $ emqx start
        emqx v4.0.0 is started successfully!
        
        $ emqx_ctl status
        Node 'emqx@127.0.0.1' is started
        emqx 4.0.0 is running
        ```
    
      - systemctl 启动
        
        ```bash
        $ sudo systemctl start emqx
        ```
    
      - service 启动
        
        ```bash
        $ sudo service emqx start
        ```

### 使用 rpm 包安装 EMQX

1.  通过 [emqx.io](https://www.emqx.com/zh/downloads?product=enterprise) 选择
    openSUSE，然后下载要安装的 EMQX 版本的 rpm 包。

2.  安装 EMQX，将下面的路径更改为您下载 EMQX 软件包的路径。
    
    ```bash
    $ sudo rpm -ivh emqx-ee-opensuse-v4.0.0.x86_64.rpm
    ```

3.  导入License文件:
    
    ```bash
    $ cp /path/to/emqx.lic /etc/emqx/emqx.lic
    ```

4.  启动 EMQX
    
      - 直接启动
        
        ```bash
        $ emqx start
        emqx v4.0.0 is started successfully!
        
        $ emqx_ctl status
        Node 'emqx@127.0.0.1' is started
        emqx 4.0.0 is running
        ```
    
      - systemctl 启动
        
        ```bash
        $ sudo systemctl start emqx
        ```
    
      - service 启动
        
        ```bash
        $ sudo service emqx start
        ```

### 使用 zip 包安装 EMQX

1.  通过 [emqx.io](https://www.emqx.com/zh/downloads?product=enterprise) 选择
    openSUSE，然后下载要安装的 EMQX 版本的 zip 包。

2.  解压压缩包
    
    ```bash
    $ unzip emqx-ee-opensuse-v4.0.0.zip
    ```

3.  导入License文件:
    
    ```bash
    $ cp /path/to/emqx.lic /path/to/emqx/etc/emqx.lic
    ```

4.  启动 EMQX
    
    ```bash
    $ ./bin/emqx start
    emqx v4.0.0 is started successfully!
    
    $ ./bin/emqx_ctl status
    Node 'emqx@127.0.0.1' is started
    emqx 4.0.0 is running
    ```


## FreeBSD

  - FreeBSD 12

### 使用 zip 包安装 EMQX

1.  通过 [emqx.io](https://www.emqx.com/zh/downloads?product=enterprise) 选择
    FreeBSD，然后下载要安装的 EMQX 版本的 zip 包。

2.  解压压缩包
    
    ```bash
    $ unzip emqx-ee-freebsd12-v4.0.0.zip
    ```

3.  导入License文件:
    
    ```bash
    $ cp /path/to/emqx.lic /path/to/emqx/etc/emqx.lic
    ```

4.  启动 EMQX
    
    ```bash
    $ ./bin/emqx start
    emqx v4.0.0 is started successfully!
    
    $ ./bin/emqx_ctl status
    Node 'emqx@127.0.0.1' is started
    emqx 4.0.0 is running
    ```


## Docker

1.  获取 docker 镜像
    
- 通过 [Docker Hub](https://hub.docker.com/r/emqx/emqx-ee) 获取

```bash
$ docker pull emqx/emqx-ee:v4.0.0
```

- 通过 [emqx.io](https://www.emqx.com/zh/downloads?product=enterprise) 手动下载
docker
镜像，并手动加载

```bash
$ wget -O emqx-ee-docker-v4.0.0.zip https://www.emqx.com/en/downloads/enterprise/v4.0.0/emqx-ee-docker-v4.0.0-amd64.zip
$ unzip emqx-ee-docker.zip
$ docker load < emqx-ee-docker-v4.0.0
```

2.  启动 docker 容器
    
```bash
$ docker run -d -\
    -name emqx-ee \
    -p 1883:1883 \
    -p 8083:8083 \
    -p 8883:8883 \
    -p 8084:8084 \
    -p 18083:18083 \
    -v /path/to/emqx.lic:/opt/emqx/etc/emqx.lic
    emqx/emqx-ee:v4.0.0
```

更多关于 EMQX Docker 的信息请查看 [Docker Hub](https://hub.docker.com/r/emqx/emqx-ee)
