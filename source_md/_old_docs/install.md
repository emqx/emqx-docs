# 程序安装 (Installation)

*EMQ X* 消息服务器可跨平台运行在 Linux、FreeBSD、macOS、Windows 或 openSUSE 服务器上。


> 产品部署建议 Linux 服务器，不推荐 Windows 服务器。

## *EMQ X* 程序包下载

*EMQ X* 消息服务器每个版本会发布 CentOS、Ubuntu、Debian、FreeBSD、macOS、Windows
、openSUSE 平台程序包与 Docker 镜像。

下载地址: <https://www.emqx.io/downloads>

## CentOS

  - CentOS6.X
  - CentOS7.X

### 使用储存库安装 EMQ X

1.  安装所需要的依赖包
    
    ``` sourceCode console
    $ sudo yum install -y yum-utils device-mapper-persistent-data lvm2
    ```

2.  使用以下命令设置稳定存储库，以 CentOS7
    为例
    
    ``` sourceCode console
    $ sudo yum-config-manager --add-repo https://repos.emqx.io/emqx-ce/redhat/centos/7/emqx-ce.repo
    ```

3.  安装最新版本的 EMQ X
    
    ``` sourceCode console
    $ sudo yum install emqx
    ```
    
    <div class="note">
    
    <div class="admonition-title">
    
    Note
    
    </div>
    
    如果提示接受 GPG 密钥，请确认密钥符合 fc84 1ba6 3775 5ca8 487b 1e3c c0b4 0946 3e64
    0d53，如果符合，则接受该指纹。
    
    </div>

4.  安装特定版本的 EMQ X
    
    1.  查询可用版本
        
        ``` sourceCode console
        $ yum list emqx --showduplicates | sort -r
        
        emqx.x86_64                     4.0.0-1.el7                        emqx-stable
        emqx.x86_64                     3.0.1-1.el7                        emqx-stable
        emqx.x86_64                     3.0.0-1.el7                        emqx-stable
        ```
    
    2.  根据第二列中的版本字符串安装特定版本，例如 4.0.0
        
        ``` sourceCode console
        $ sudo yum install emqx-4.0.0
        ```

5.  启动 EMQ X
    
      - 直接启动
        
        ``` sourceCode console
        $ emqx start
        emqx 4.0.0 is started successfully!
        
        $ emqx_ctl status
        Node 'emqx@127.0.0.1' is started
        emqx v4.0.0 is running
        ```
    
      - systemctl 启动
        
        ``` sourceCode console
        $ sudo systemctl start emqx
        ```
    
      - service 启动
        
        ``` sourceCode console
        $ sudo service emqx start
        ```

6.  配置文件路径
    
      - 配置文件路径：`/etc/emqx`
      - 日志文件路径：`/var/log/emqx`
      - 数据文件路径：`/var/lib/emqx`

### 使用 rpm 包安装 EMQ X

1.  通过 [emqx.io](https://www.emqx.io/downloads/broker?osType=Linux) 或
    [github](https://github.com/emqx/emqx/releases) 选择 CentOS
    版本，然后下载要安装的 EMQ X 版本的 rpm 包。

2.  安装 EMQ X
    
    ``` sourceCode console
    $ sudo rpm -ivh emqx-centos7-v4.0.0.x86_64.rpm
    ```

3.  启动 EMQ X
    
      - 直接启动
        
        ``` sourceCode console
        $ emqx start
        emqx 4.0.0 is started successfully!
        
        $ emqx_ctl status
        Node 'emqx@127.0.0.1' is started
        emqx v4.0.0 is running
        ```
    
      - systemctl 启动
        
        ``` sourceCode console
        $ sudo systemctl start emqx
        ```
    
      - service 启动
        
        ``` sourceCode console
        $ sudo service emqx start
        ```

4.  配置文件路径
    
      - 配置文件路径：`/etc/emqx`
      - 日志文件路径：`/var/log/emqx`
      - 数据文件路径：`/var/lib/emqx`

### 使用 zip 包安装 EMQ X

1.  通过 [emqx.io](https://www.emqx.io/downloads/broker?osType=Linux) 或
    [github](https://github.com/emqx/emqx/releases) 选择 Centos
    版本，然后下载要安装的 EMQ X 版本的 zip 包。

2.  解压程序包
    
    ``` sourceCode console
    $ unzip emqx-centos7-v4.0.0.zip
    ```

3.  启动 EMQ X
    
    ``` sourceCode console
    $ ./bin/emqx start
    emqx 4.0.0 is started successfully!
    
    $ ./bin/emqx_ctl status
    Node 'emqx@127.0.0.1' is started
    emqx v4.0.0 is running
    ```

## Ubuntu

  - Bionic 18.04 (LTS)
  - Xenial 16.04 (LTS)
  - Trusty 14.04 (LTS)
  - Precise 12.04 (LTS)

### 使用储存库安装 EMQ X

1.  安装所需要的依赖包
    
    ``` sourceCode console
    $ sudo apt update && sudo apt install -y \
        apt-transport-https \
        ca-certificates \
        curl \
        gnupg-agent \
        software-properties-common
    ```

2.  添加 EMQ X 的官方 GPG 密钥
    
    ``` sourceCode console
    $ curl -fsSL https://repos.emqx.io/gpg.pub | sudo apt-key add -
    ```
    
    验证密钥
    
    ``` sourceCode console
    $ sudo apt-key fingerprint 3E640D53
    
    pub   rsa2048 2019-04-10 [SC]
        FC84 1BA6 3775 5CA8 487B  1E3C C0B4 0946 3E64 0D53
    uid           [ unknown] emqx team <support@emqx.io>
    ```

3.  使用以下命令设置 stable 存储库。 如果要添加 unstable 存储库，请在以下命令中的单词 stable 之后添加单词
    unstable。
    
    ``` sourceCode console
    $ sudo add-apt-repository \
        "deb [arch=amd64] https://repos.emqx.io/emqx-ce/deb/ubuntu/ \
        $(lsb_release -cs) \
        stable"
    ```
    
    <div class="note">
    
    <div class="admonition-title">
    
    Note
    
    </div>
    
    lsb\_release -cs 子命令返回 Ubuntu 发行版的名称，例如 xenial。 有时，在像 Linux Mint
    这样的发行版中，您可能需要将 $(lsb\_release -cs) 更改为您的父 Ubuntu 发行版。
    例如，如果您使用的是 Linux Mint Tessa，则可以使用 bionic。 EMQ X
    不对未经测试和不受支持的 Ubuntu 发行版提供任何保证。
    
    </div>

4.  更新 apt 包索引
    
    ``` sourceCode console
    $ sudo apt update
    ```

5.  安装最新版本的 EMQ X
    
    ``` sourceCode console
    $ sudo apt install emqx
    ```
    
    <div class="note">
    
    <div class="admonition-title">
    
    Note
    
    </div>
    
    在启用了多个 EMQ X 仓库的情况下，如果 apt install 和 apt update
    命令没有指定版本号，那么会自动安装最新版的 EMQ
    X。这对于有稳定性需求的用户来说是一个问题。
    
    </div>

6.  安装特定版本的 EMQ X
    
    1.  查询可用版本
        
        ``` sourceCode console
        $ sudo apt-cache madison emqx
        
        emqx |      4.0.0 | https://repos.emqx.io/emqx-ce/deb/ubuntu bionic/stable amd64 Packages
        emqx |      3.0.1 | https://repos.emqx.io/emqx-ce/deb/ubuntu bionic/stable amd64 Packages
        emqx |      3.0.0 | https://repos.emqx.io/emqx-ce/deb/ubuntu bionic/stable amd64 Packages
        ```
    
    2.  使用第二列中的版本字符串安装特定版本，例如 4.0.0
        
        ``` sourceCode console
        $ sudo apt install emqx=4.0.0
        ```

7.  启动 EMQ X
    
      - 直接启动
        
        ``` sourceCode console
        $ emqx start
        emqx 4.0.0 is started successfully!
        
        $ emqx_ctl status
        Node 'emqx@127.0.0.1' is started
        emqx v4.0.0 is running
        ```
    
      - systemctl 启动
        
        ``` sourceCode console
        $ sudo systemctl start emqx
        ```
    
      - service 启动
        
        ``` sourceCode console
        $ sudo service emqx start
        ```

8.  配置文件路径
    
      - 配置文件路径：`/etc/emqx`
      - 日志文件路径：`/var/log/emqx`
      - 数据文件路径：\`/var/lib/emqx

### 使用 deb 包安装 EMQ X

1.  通过 [emqx.io](https://www.emqx.io/downloads/broker?osType=Linux) 或
    [github](https://github.com/emqx/emqx/releases) 选择 Ubuntu
    版本，然后下载要安装的 EMQ X 版本的 deb 包。

2.  安装 EMQ X
    
    ``` sourceCode console
    $ sudo dpkg -i emqx-ubuntu18.04-v4.0.0_amd64.deb
    ```

3.  启动 EMQ X
    
      - 直接启动
        
        ``` sourceCode console
        $ emqx start
        emqx 4.0.0 is started successfully!
        
        $ emqx_ctl status
        Node 'emqx@127.0.0.1' is started
        emqx v4.0.0 is running
        ```
    
      - systemctl 启动
        
        ``` sourceCode console
        $ sudo systemctl start emqx
        ```
    
      - service 启动
        
        ``` sourceCode console
        $ sudo service emqx start
        ```

4.  配置文件路径
    
      - 配置文件路径：`/etc/emqx`
      - 日志文件路径：`/var/log/emqx`
      - 数据文件路径：\`/var/lib/emqx

### 使用 zip 包安装 EMQ X

1.  通过 [emqx.io](https://www.emqx.io/downloads/broker?osType=Linux) 或
    [github](https://github.com/emqx/emqx/releases) 选择 Ubuntu
    版本，然后下载要安装的 EMQ X 版本的 zip 包。

2.  解压程序包
    
    ``` sourceCode console
    $ unzip emqx-ubuntu18.04-v4.0.0.zip
    ```

3.  启动 EMQ X
    
    ``` sourceCode console
    $ ./bin/emqx start
    emqx 4.0.0 is started successfully!
    
    $ ./bin/emqx_ctl status
    Node 'emqx@127.0.0.1' is started
    emqx v4.0.0 is running
    ```

## Debian

  - Stretch (Debian 9)
  - Jessie (Debian 8)

### 使用储存库安装 EMQ X

1.  安装所需要的依赖包
    
    ``` sourceCode console
    $ sudo apt update && sudo apt install -y \
        apt-transport-https \
        ca-certificates \
        curl \
        gnupg-agent \
        software-properties-common
    ```

2.  添加 EMQ X 的官方 GPG 密钥
    
    ``` sourceCode console
    $ curl -fsSL https://repos.emqx.io/gpg.pub | sudo apt-key add -
    ```
    
    验证密钥
    
    ``` sourceCode console
    $ sudo apt-key fingerprint 3E640D53
    
    pub   rsa2048 2019-04-10 [SC]
        FC84 1BA6 3775 5CA8 487B  1E3C C0B4 0946 3E64 0D53
    uid           [ unknown] emqx team <support@emqx.io>
    ```

3.  使用以下命令设置 stable 存储库。 如果要添加 unstable 的存储库，请在以下命令中的单词 stable 之后添加单词
    unstable。
    
    ``` sourceCode console
    $ sudo add-apt-repository \
        "deb [arch=amd64] https://repos.emqx.io/emqx-ce/deb/debian/ \
        $(lsb_release -cs) \
        stable"
    ```
    
    <div class="note">
    
    <div class="admonition-title">
    
    Note
    
    </div>
    
    lsb\_release -cs 子命令返回 Debian 发行版的名称，例如 helium。 有时，在像 BunsenLabs
    Linux 这样的发行版中，您可能需要将 $(lsb\_release -cs) 更改为您的父 Debian 发行版。
    例如，如果您使用的是 BunsenLabs Linux Helium，则可以使用 stretch。
    EMQ X 不对未经测试和不受支持的 Debian 发行版提供任何保证。
    
    </div>

4.  更新 apt 包索引
    
    ``` sourceCode console
    $ sudo apt update
    ```

5.  安装最新版本的 EMQ X
    
    ``` sourceCode console
    $ sudo apt install emqx
    ```
    
    <div class="note">
    
    <div class="admonition-title">
    
    Note
    
    </div>
    
    在启用了多个 EMQ X 仓库的情况下，如果 apt install 和 apt update
    命令没有指定版本号，那么会自动安装最新版的 EMQ
    X。这对于有稳定性需求的用户来说是一个问题。
    
    </div>

6.  安装特定版本的 EMQ X
    
    1.  查询可用版本
        
        ``` sourceCode console
        $ sudo apt-cache madison emqx
        
        emqx |      4.0.0 | https://repos.emqx.io/emqx-ce/deb/debian stretch/stable amd64 Packages
        emqx |      3.0.1 | https://repos.emqx.io/emqx-ce/deb/debian stretch/stable amd64 Packages
        emqx |      3.0.0 | https://repos.emqx.io/emqx-ce/deb/debian stretch/stable amd64 Packages
        ```
    
    2.  使用第二列中的版本字符串安装特定版本，例如 4.0.0
        
        ``` sourceCode console
        $ sudo apt install emqx=4.0.0
        ```

7.  启动 EMQ X
    
      - 直接启动
        
        ``` sourceCode console
        $ emqx start
        emqx 4.0.0 is started successfully!
        
        $ emqx_ctl status
        Node 'emqx@127.0.0.1' is started
        emqx v4.0.0 is running
        ```
    
      - systemctl 启动
        
        ``` sourceCode console
        $ sudo systemctl start emqx
        ```
    
      - service 启动
        
        ``` sourceCode console
        $ sudo service emqx start
        ```

8.  配置文件路径
    
      - 配置文件路径：`/etc/emqx`
      - 日志文件路径：`/var/log/emqx`
      - 数据文件路径：\`/var/lib/emqx

### 使用 deb 包安装 EMQ X

1.  通过 [emqx.io](https://www.emqx.io/downloads/broker?osType=Linux) 或
    [github](https://github.com/emqx/emqx/releases) 选择 Debian
    版本，然后下载要安装的 EMQ X 版本的 deb 包。

2.  安装 EMQ X
    
    ``` sourceCode console
    $ sudo dpkg -i emqx-debian9-v4.0.0_amd64.deb
    ```

3.  启动 EMQ X
    
      - 直接启动
        
        ``` sourceCode console
        $ emqx start
        emqx 4.0.0 is started successfully!
        
        $ emqx_ctl status
        Node 'emqx@127.0.0.1' is started
        emqx v4.0.0 is running
        ```
    
      - systemctl 启动
        
        ``` sourceCode console
        $ sudo systemctl start emqx
        ```
    
      - service 启动
        
        ``` sourceCode console
        $ sudo service emqx start
        ```

4.  配置文件路径
    
      - 配置文件路径：`/etc/emqx`
      - 日志文件路径：`/var/log/emqx`
      - 数据文件路径：\`/var/lib/emqx

### 使用 zip 包安装 EMQ X

1.  通过 [emqx.io](https://www.emqx.io/downloads/broker?osType=Linux) 或
    [github](https://github.com/emqx/emqx/releases) 选择 Debian
    版本，然后下载要安装的 EMQ X 版本的 zip 包。

2.  解压程序包
    
    ``` sourceCode console
    $ unzip emqx-debian9-v4.0.0.zip
    ```

3.  启动 EMQ X
    
    ``` sourceCode console
    $ ./bin/emqx start
    emqx 4.0.0 is started successfully!
    
    $ ./bin/emqx_ctl status
    Node 'emqx@127.0.0.1' is started
    emqx v4.0.0 is running
    ```

## macOS

### 使用 Homebrew 安装

1.  添加 EMQ X 的 tap
    
    ``` sourceCode console
    $ brew tap emqx/emqx
    ```

2.  安装 EMQ X
    
    ``` sourceCode console
    $ brew install emqx
    ```

3.  启动 EMQ X
    
    ``` sourceCode console
    $ emqx start
    emqx 4.0.0 is started successfully!
    
    $ emqx_ctl status
    Node 'emqx@127.0.0.1' is started
    emqx v4.0.0 is running
    ```

### 使用 zip 包安装 EMQ X

1.  通过 [emqx.io](https://www.emqx.io/downloads/broker?osType=Linux) 或
    [github](https://github.com/emqx/emqx/releases)，选择 EMQ X 版本，然后下载要安装的
    zip 包。

2.  解压压缩包
    
    ``` sourceCode console
    $ unzip emqx-macos-v4.0.0.zip
    ```

3.  启动 EMQ X
    
    ``` sourceCode console
    $ ./bin/emqx start
    emqx 4.0.0 is started successfully!
    
    $ ./bin/emqx_ctl status
    Node 'emqx@127.0.0.1' is started
    emqx v4.0.0 is running
    ```

## Windows

1.  通过 [emqx.io](https://www.emqx.io/downloads/broker?osType=Linux) 或
    [github](https://github.com/emqx/emqx/releases) 选择 Windows
    版本，然后下载要安装的 .zip 包。

2.  解压压缩包

3.  打开 Windows 命令行窗口，cd 到程序目录， 启动 EMQ X。
    
    ``` sourceCode console
    cd emqx/bin
    emqx start
    ```

## openSUSE

  - openSUSE leap

### 使用储存库安装 EMQ X

1.  下载 GPG 公钥并导入。
    
    ``` sourceCode console
    $ curl -L -o /tmp/gpg.pub https://repos.emqx.io/gpg.pub
    $ sudo rpmkeys --import /tmp/gpg.pub
    ```

2.  添加储存库地址
    
    ``` sourceCode console
    $ sudo zypper ar -f -c https://repos.emqx.io/emqx-ce/redhat/opensuse/leap/stable emqx
    ```

3.  安装最新版本的 EMQ X
    
    ``` sourceCode console
    $ sudo zypper in emqx
    ```

4.  安装特定版本的 EMQ X
    
    1.  查询可用版本
        
        ``` sourceCode console
        $ sudo zypper pa emqx
        
        Loading repository data...
        Reading installed packages...
        S | Repository | Name | Version  | Arch
        --+------------+------+----------+-------
          | emqx       | emqx | 4.0.0-1  | x86_64
          | emqx       | emqx | 3.0.1-1  | x86_64
          | emqx       | emqx | 3.0.0-1  | x86_64
        ```
    
    2.  使用 Version 安装特定版本，例如 4.0.0
        
        ``` sourceCode console
        $ sudo zypper in emqx-4.0.0
        ```

5.  启动 EMQ X
    
      - 直接启动
        
        ``` sourceCode console
        $ emqx start
        emqx 4.0.0 is started successfully!
        
        $ emqx_ctl status
        Node 'emqx@127.0.0.1' is started
        emqx v4.0.0 is running
        ```
    
      - systemctl 启动
        
        ``` sourceCode console
        $ sudo systemctl start emqx
        ```
    
      - service 启动
        
        ``` sourceCode console
        $ sudo service emqx start
        ```

### 使用 rpm 包安装 EMQ X

1.  通过 [emqx.io](https://www.emqx.io/downloads/broker?osType=Linux) 或
    [github](https://github.com/emqx/emqx/releases) 选择 openSUSE，然后下载要安装的
    EMQ X 版本的 rpm 包。

2.  安装 EMQ X，将下面的路径更改为您下载 EMQ X 软件包的路径。
    
    ``` sourceCode console
    $ sudo rpm -ivh emqx-opensuse-v4.0.0.x86_64.rpm
    ```

3.  启动 EMQ X
    
      - 直接启动
        
        ``` sourceCode console
        $ emqx start
        emqx 4.0.0 is started successfully!
        
        $ emqx_ctl status
        Node 'emqx@127.0.0.1' is started
        emqx v4.0.0 is running
        ```
    
      - systemctl 启动
        
        ``` sourceCode console
        $ sudo systemctl start emqx
        ```
    
      - service 启动
        
        ``` sourceCode console
        $ sudo service emqx start
        ```

4.  配置文件路径
    
      - 配置文件路径：`/etc/emqx`
      - 日志文件路径：`/var/log/emqx`
      - 数据文件路径：\`/var/lib/emqx

### 使用 zip 包安装 EMQ X

1.  通过 [emqx.io](https://www.emqx.io/downloads/broker?osType=Linux) 或
    [github](https://github.com/emqx/emqx/releases) 选择 openSUSE，然后下载要安装的
    EMQ X 版本的 zip 包。

2.  解压压缩包
    
    ``` sourceCode console
    $ unzip emqx-opensuse-v4.0.0.zip
    ```

3.  启动 EMQ X
    
    ``` sourceCode console
    $ ./bin/emqx start
    emqx 4.0.0 is started successfully!
    
    $ ./bin/emqx_ctl status
    Node 'emqx@127.0.0.1' is started
    emqx v4.0.0 is running
    ```

## FreeBSD

  - FreeBSD 12

### 使用 zip 包安装 EMQ X

1.  通过 [emqx.io](https://www.emqx.io/downloads/broker?osType=Linux) 或
    [github](https://github.com/emqx/emqx/releases) 选择 FreeBSD，然后下载要安装的
    EMQ X 版本的 zip 包。

2.  解压压缩包
    
    ``` sourceCode console
    $ unzip emqx-freebsd12-v4.0.0.zip
    ```

3.  启动 EMQ X
    
    ``` sourceCode console
    $ ./bin/emqx start
    emqx 4.0.0 is started successfully!
    
    $ ./bin/emqx_ctl status
    Node 'emqx@127.0.0.1' is started
    emqx v4.0.0 is running
    ```

## Docker

1.  获取 docker 镜像
    
      - 通过 [Docker Hub](https://hub.docker.com/r/emqx/emqx) 获取
        
        ``` sourceCode console
        $ docker pull emqx/emqx:v4.0.0
        ```
    
      - 通过 [emqx.io](https://www.emqx.io/downloads/broker?osType=Linux)
        或 [github](https://github.com/emqx/emqx/releases) 手动下载 docker
        镜像，并手动加载
        
        ``` sourceCode console
        $ wget -O emqx-docker.zip https://www.emqx.io/downloads/v3/latest/emqx-docker.zip
        $ unzip emqx-docker.zip
        $ docker load < emqx-docker-v4.0.0
        ```

2.  启动 docker
    容器
    
    ``` sourceCode console
    $ docker run -d --name emqx31 -p 1883:1883 -p 8083:8083 -p 8883:8883 -p 8084:8084 -p 18083:18083 emqx/emqx:v4.0.0
    ```

更多关于 EMQ X Docker 的信息请查看 [Docker
Hub](https://hub.docker.com/r/emqx/emqx) 或 [EMQ X
Docker](https://github.com/emqx/emqx-docker)

## 源码编译安装

### 环境要求

*EMQ X* 消息服务器基于 Erlang/OTP 平台开发，项目托管的 GitHub 管理维护，源码编译依赖 Erlang 环境和 git
客户端。

<div class="note">

<div class="admonition-title">

Note

</div>

EMQ X 依赖 Erlang R21.2+ 版本

</div>

Erlang 安装: <http://www.erlang.org/>

Git 客户端: <http://www.git-scm.com/>

### 编译安装 EMQ X

1.  获取源码
    
    ``` sourceCode bash
    $ git clone -b v3.2.0 https://github.com/emqx/emqx-rel.git
    ```

2.  设置环境变量
    
    ``` sourceCode bash
    $ export EMQX_DEPS_DEFAULT_VSN=v3.2.0
    ```

3.  编译
    
    ``` sourceCode bash
    $ cd emqx-rel && make
    ```

4.  启动 EMQ X
    
    ``` sourceCode bash
    $ cd emqx-rel/_rel/emqx
    $ ./bin/emqx start
    emqx v3.2.0 is started successfully!
    
    $ ./bin/emqx_ctl status
    Node 'emqx@127.0.0.1' is started
    emqx 3.2.0 is running
    ```

## Windows 源码编译安装

Erlang 安装: <http://www.erlang.org/>

scoop 安装: <https://scoop.sh/>

scoop 安装完成后，使用 scoop 来安装 Git、 Make、curl、erlang

``` sourceCode bash
scoop install git make curl erlang
```

编译环境准备完成

rebar3 安装:

``` sourceCode bash
git clone https://github.com/erlang/rebar3.git

cd rebar3

bootstrap
```

rebar3 编译完成后，在 windows 环境变量 PATH 中添加 rebar3 的路径

构建环境准备之后，clone 代码开始构建 emqx

``` sourceCode bash
git clone  -b v3.2.0 https://github.com/emqx/emqx-rel.git

cd emqx-relx && make

cd _build/emqx/rel/emqx && ./bin/emqx start
```

控制台启动编译的 EMQ 程序包

``` sourceCode bash
cd _build/emqx/rel/emqx
emqx console
```

注册 windows 服务

``` sourceCode bash
cd _build/emqx/rel/emqx
emqx install
```
