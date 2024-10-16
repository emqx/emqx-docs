# Deployment FAQs

## Which operating systems are recommended for EMQX deployment?

EMQX supports running on a variety of operating systems and hardware platforms. Considering the stability and reliability of enterprise-level deployment, we generally recommend deploying it on Linux distributions such as CentOS, Ubuntu, and Debian.

## What is the recommended deployment plan for EMQX?

We recommend deploying EMQX in a cluster and deploying load balancing (Nginx, HAProxy, etc.) on the front-end of the cluster so that connections are balanced to each node in the cluster.

For users with high communication security requirements, we recommend enabling TLS connections for clients and terminating TLS connections on the LB side, i.e., TLS encrypted communication is used between the client and the LB, while TCP communication is still used between the LB and the EMQX nodes.

Since EMQX nodes do not expose their ports to the public network, it does not reduce the overall security, but by offloading TLS, it can effectively save the resource consumption of EMQX.

## I don't have a large number of devices or high message throughput, do I still need to deploy clusters?

Even with a small number of devices and low message throughput, it still makes sense to deploy clustering in a production environment.

Clustering improves system availability and reduces the likelihood of a single point of failure. Even if a node goes down, other healthy nodes within the cluster can continue to provide services, ensuring that business is not affected.

## Deployment of EMQX Open Source Core + Repl Cluster Fails

Starting from EMQX v5.8, only the EMQX Enterprise edition supports clusters with Core + Replicant nodes. Therefore, when using the [EMQX Operator](https://github.com/emqx/emqx-operator) to deploy an EMQX open-source Core + Repl cluster, you may encounter the issue where the EMQX custom resource remains in the `replicantNodesProgressing` state, and the EMQX Repl Pod repeatedly crashes.

It is recommended to either deploy an open-source cluster with only Core nodes or deploy a Core + Repl cluster using the Enterprise Edition.

## How to troubleshoot when EMQX fails to start?

When EMQX fails to start, you can check `emqx.log.N` or `erlang.log.N` under [Log Directory](../deploy/install.md#files-and-directories) to get detailed error prompts. 

Or execute `emqx console` to start EMQX from the console, so that the error log will be output directly to the console. Then, according to the log content, find the corresponding solution in this page, or post on [GitHub](https://github.com/emqx/emqx/issues) for support.

## EMQX failed to start with log message "logger: command not found"

Just install the following dependency:

- `CentOS/Redhat`

```
$ yum install rsyslog
```

- `Ubuntu/Debian`

```
$ apt-get install bsdutils
```

## EMQX failed to start with log message "...{on_load_function_failed,crypto}..."

For better security, starting from version 4.3, EMQX runs on openssl-1.1.
This may cause some troubles for users running EMQX on some old Linux distributions,

For EMQX version earlier than v4.3.10 and EMQX Enterprise version earlier than e4.3.5, you may see the following error messages:

```bash
{application_start_failure,kernel,{{shutdown,{failed_to_start_child,kernel_safe_sup,{on_load_function_failed,crypto}}}, ..}
```

For later versions, you may see the following error messages:

```bash
FATAL: Unable to start Erlang.
Please make sure openssl-1.1.1 (libcrypto) and libncurses are installed.
```

It indicates that the "crypto" application in Erlang/OTP that EMQX depends on failed to start because the required openssl dynamic lib (.so) is not found. The solution is as follows:

:::: tabs

::: tab CentOS

Extra Packages for Enterprise Linux (or EPEL) is a Fedora Special Interest Group that creates, maintains, and manages a high-quality set of additional packages for Enterprise Linux. Using CentOS 7 as an example:

1. To install the RPM repos, execute `yum install epel-release`.
1. If failed to install, follow the instructions here: https://docs.fedoraproject.org/en-US/epel/#_el7 to ensure the yum repos are added, and try step 1 again.
1. Execute `yum install openssl11` to install openssl-1.1.

:::

::: tab Linux

Go to the installation directory of EMQX (If you use the package management tool to install EMQX, you should enter the same level directory as the `lib` of EMQX)

```bash
  ## Package installation
$ cd emqx

  ## Package manager installation, such as yum. Its lib directory should be in /lib/emqx
$ cd /lib/emqx
```

Query the list of `.so` dynamic libraries that `crypto` depends on and its location in memory:

```bash
$ ldd lib/crypto-*/priv/lib/crypto.so

lib/crypto-4.6/priv/lib/crypto.so: /lib64/libcrypto.so.10: version `OPENSSL_1.1.1' not found (required by lib/crypto-4.6/priv/lib/crypto.so)
          linux-vdso.so.1 =>  (0x00007fff67bfc000)
          libcrypto.so.10 => /lib64/libcrypto.so.10 (0x00007fee749ca000)
          libc.so.6 => /lib64/libc.so.6 (0x00007fee74609000)
          libdl.so.2 => /lib64/libdl.so.2 (0x00007fee74404000)
          libz.so.1 => /lib64/libz.so.1 (0x00007fee741ee000)
          /lib64/ld-linux-x86-64.so.2 (0x00007fee74fe5000)

```

Among them, `OPENSSL_1.1.1' not found` indicates that the `.so` library of specified OPENSSL version is not installed correctly.

Compile and install OPENSSL 1.1.1 from source code, and place its so file to a path recognized by the system:

```bash
## Download the latest version 1.1.1
$ wget https://www.openssl.org/source/openssl-1.1.1c.tar.gz

## Upload to ct-test-ha
$ scp openssl-1.1.1c.tar.gz ct-test-ha:~/

## Unzip, compile and install
$ tar zxf   openssl-1.1.1c.tar.gz
$ cd openssl-1.1.1c
$ ./config
$ make test   		# Perform test; continue if PASS is output
$ make install

## Ensure library references
$ ln -s /usr/local/lib64/libssl.so.1.1 /usr/lib64/libssl.so.1.1
$ ln -s /usr/local/lib64/libcrypto.so.1.1 /usr/lib64/libcrypto.so.1.1
```

After the completion, execute `ldd lib/crypto-*/priv/lib/crypto.so` in the lib-level directory of EMQX to check whether it can be correctly identified. If there is no `.so` library in `not found`, you can start EMQX normally.

:::

::: tab macOS

Go to the installation directory of EMQX:

```bash
  ## package installation
$ cd emqx

  ## brew installation
$ cd /usr/local/Cellar/emqx/<version>/
```

Query the list of `.so` dynamic libraries that `crypto` depends on:

```bash
$ otool -L lib/crypto-*/priv/lib/crypto.so

lib/crypto-4.4.2.1/priv/lib/crypto.so:
  /usr/local/opt/openssl@1.1/lib/libcrypto.1.1.dylib (compatibility version 1.1.0, current version 1.1.0)
  /usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1252.200.5)
```

It shows that OPENSSL has been successfully installed to the specified directory by checking:

```bash
$ ls /usr/local/opt/openssl@1.1/lib/libcrypto.1.1.dylib
ls: /usr/local/opt/openssl@1.1/lib/libcrypto.1.1.dylib: No such file or directory
```

If the file does not exist, you need to install the version of OPENSSL corresponding with what printed by `otool`. For example, it shown here as `openssl@1.1`:

```bash
$ brew install openssl@1.1
```

After the installation is complete, you can start EMQX normally.

## EMQX failed to start with log message "libatomic.so.1: cannot open shared object file: No such file or directory"

The reason for this error is that the current system lacks the dependency libatomic. The solution is to install this dependency:

```
# Rocky Linux, CentOS, ...
yum install -y libatomic
# Debian, Ubuntu, ...
apt install -y libatomic
```

If you install the RPM or DEB package manually, you may encounter the following prompt during installation:

```
$ rpm -ivh emqx-5.7.0-el8-amd64.rpm
error: Failed dependencies:
libatomic is needed by emqx-5.7.0-el8-amd64.rpm
```

The solution is still to manually install the dependency libatomic first.

Of course, the most recommended installation method is to use the package manager (yum, apt, etc.), which will automatically install the required dependencies without us having to worry about it.

:::

::::

## Failed to start EMQX with Docker, log prompts "Permission denied"

When you intend to persist EMQX data by mounting the directory:

```
sudo docker run -d --name emqx -p 18083:18083 -p 1883:1883 -v /emqx/data:/opt/emqx/data -v /emqx/log:/opt/emqx/log emqx:latest
```

You may encounter a container startup failure with the following error:

```
mkdir: cannot create directory '/opt/emqx/data/configs': Permission denied
```

This is because EMQX runs as Linux user `emqx` in the container, while the directories in your host may be created using the `root` user, so EMQX cannot create directories or files in these directories.

To solve this problem, you can create an `emqx` user in the host, and then let the user create the directory to be mounted, or directly change the permissions of the created data and log directories to 777.

Of course, the most recommended way to implement EMQX data persistence is to use named data volume, so you don’t have to worry about permission anymore:

```
sudo docker volume create --name emqx-data
sudo docker volume create --name emqx-log
sudo docker run -d --name emqx -p 18083:18083 -p 1883:1883 -v emqx-data:/opt/emqx/data -v emqx-log:/opt/emqx/log emqx:latest
```

## What should I do if EMQX prompts that the port is occupied (eaddrinuse) when starting?

By default, EMQX will occupy 7 ports when it starts. They are:

1. Port 1883, used for MQTT over TCP listener. It can be modified through configuration.
2. Port 8883, used for MQTT over SSL/TLS listener. It can be modified through configuration.
3. Port 8083, used for MQTT over WebSocket listener. It can be modified through configuration.
4. Port 8084, used for MQTT over WSS (WebSocket over SSL) listener. It can be modified through configuration.
5. Port 18083, the default listening port for the HTTP API service. The dashboard also relies on this port, which can be modified through configuration.
6. Port 4370, used for remote function calls in the EMQX distributed cluster and Mnesia data synchronization. This port is occupied by default, even if no cluster is formed. The listening port is determined by `BasePort (4370) + Offset`, where 4370 is fixed and cannot be modified, and Offset is determined by the numeric suffix in the node name (`Name@Host`). If there is no numeric suffix, it defaults to 0. For example, the Offset for `emqx@127.0.0.1` is 0, and the Offset for `emqx1@127.0.0.1` is 1.
7. Port 5370, the cluster RPC port used for load sharing. It is mainly used for forwarding MQTT messages between nodes. Similar to port 4370, this port is occupied by default, even if no cluster is formed. The actual listening port is `BasePort (5370) + Offset`, where 5370 is fixed and cannot be modified, and Offset is determined by the Name part of the node name (`Name@Host`). If there is no numeric suffix, it defaults to 0.

## Why does EMQX output the log "WARNING: Default (insecure) Erlang cookie is in use." during startup?

The complete WARNING log is as follows:

```
WARNING: Default (insecure) Erlang cookie is in use.
WARNING: Configure node.cookie in /usr/lib/emqx/etc/emqx.conf or override from environment variable EMQX_NODE__COOKIE
WARNING: NOTE: Use the same cookie for all nodes in the cluster.
```

Only EMQX nodes using the same cookie can form a cluster. While a cookie does not secure cluster communication, it prevents a node from connecting to a cluster it did not intend to communicate with. By default, EMQX nodes uniformly use the cookie value `emqxsecretcookie`. However, we recommend that users change the cookie value when building a cluster to enhance security.

The second warning log indicates two ways to modify the cookie: by editting `node.cookie` in the `emqx.conf` configuration file or by setting the environment variable `EMQX_NODE__COOKIE`.

## Why does restarting the EMQX Docker container cause data loss, such as configured rules and resources?

The runtime data of EMQX is stored in the `/opt/emqx/data` directory, including configuration rules, resources, retained messages, etc. To ensure data persistence during container restarts, it's important to mount the `/opt/emqx/data` directory to a local host directory or a data volume.

However, even if the `/opt/emqx/data` directory is properly mounted, data loss may still occur after container restarts. This is because the runtime data of EMQX is stored in the `/opt/emqx/data/mnesia/${Node Name}` directory, and when the container is restarted, the node name of EMQX changes, leading to the creation of a new storage directory.

EMQX node name consists of Name and Host, with the Host derived from the container's IP address by default. Under the default network configurations, the container's IP may change upon restarting, so you need to maintain a fixed IP for the container.

To address this issue, EMQX provides an environment variable, `EMQX_HOST`, which allows you to set the Host part of the node name. However, it is crucial that this Host value is reachable by other nodes, so it should be used in conjunction with a network alias. Here is an example command for running the EMQX Docker container with the EMQX_HOST environment variable and a network alias:

```
docker run -d --name emqx -p 18083:18083 -p 1883:1883 -e EMQX_HOST=alias-for-emqx --network example --network-alias alias-for-emqx --mount type=bind,source=/tmp/emqx,target=/opt/emqx/data emqx:5.0.24
```
