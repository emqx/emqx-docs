# Common Errors
## EMQX cannot connect to Mysql8.0

**Tags:** [*MySQL*](tags.md#mysql)  [*Auth*](tags.md#auth)


::: tip
4.3 is now compatible with caching_sha2_password, this issue only occurs in versions below 4.3
:::

Different from previous versions, Mysql8.0 uses the `caching_sha2_password` plugin by default for account password configuration. The password plugin is required to change to `mysql_native_password`.

- Modify the `mysql.user` table

  ```
  ## Switch to the mysql database
  mysql> use mysql;
  
  ## View user table
  
  mysql> select user, host, plugin from user;
  +------------------+-----------+-----------------------+
  | user             | host      | plugin                |
  +------------------+-----------+-----------------------+
  | root             | %         | caching_sha2_password |
  | mysql.infoschema | localhost | caching_sha2_password |
  | mysql.session    | localhost | caching_sha2_password |
  | mysql.sys        | localhost | caching_sha2_password |
  | root             | localhost | caching_sha2_password |
  +------------------+-----------+-----------------------+
  
  ## Change password plugin
  mysql> ALTER USER 'your_username'@'your_host' IDENTIFIED WITH mysql_native_password BY 'your_password';
  Query OK, 0 rows affected (0.01 sec)
  
  ## Refresh
  mysql> FLUSH PRIVILEGES;
  Query OK, 0 rows affected (0.00 sec)
  ```

- Change `my.conf`

  Add a line below the [mysqld] in the `my.cnf` configuration file.

  ```
  default_authentication_plugin=mysql_native_password
  ```

- Restart Mysql

## Incompatible openssl version

**Tags:** [*fail to start*](tags.md#启动失败)

For better security, starting from version 4.3, EMQX runs on openssl-1.1.
This may cause some troulbes for users running EMQX on some old linux distributions,

### Error

If starting EMQX with command `./bin/emqx console` result in below error messages:

```bash
FATAL: Unable to start Erlang.
Please make sure openssl-1.1.1 (libcrypto) and libncurses are installed.
```

Or for emqx version earlier to v4.3.10 and emqx-enterprise version earlier than e4.3.5
```bash
\{application_start_failure,kernel,\{\{shutdown,\{failed_to_start_child,kernel_safe_sup,\{on_load_function_failed,crypto\}\}\}, ..\}
```

It indicates that the "crypto" application in Erlang/OTP that EMQX depends on failed to start because the required openssl dynamic lib (.so) is not found.

### Solution

#### CentOS (install from epel-relese, using centos7 as example)

Extra Packages for Enterprise Linux (or EPEL) is a Fedora Special Interest Group that creates, maintains, and manages a high quality set of additional packages for Enterprise Linux.

1. To install the RPM repos, execute `yum install epel-release`
1. If failed to install, follow the instructions here: https://docs.fedoraproject.org/en-US/epel/#_el7 to ensure the yum repos are added, and try step 1 again
1. Execute `yum install openssl11` to install openssl-1.1

#### Linux (compile openssl-1.1 from source code)

Go to the installation directory of EMQX (If you use the package management tool to install EMQX, you should enter the same level directory as the `lib` of EMQX)

```bash
  ## Package installation
$ cd emqx
  
  ## Package manager installation, such as yum. Its lib directory should be in /lib/emqx
$ cd /lib/emqx
```

Query the list of `.so` dynamic libraries that `crypto` depends on and its location in memory:

``` bash
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


#### macOS

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

## MSVCR120.dll is missing from Windows

**Tags:** [*fail to start*](tags.md#启动失败)

### Phenomenon

When Windows executes `./bin/emqx console`, an error window pops up:

```bash
This program cannot be started because MSVCR120.dll is missing from the computer. Please try to reinstall the program to resolve this issue.
```

### Solution

Install [Microsoft Visual C++ RedistributablePackage](https://www.microsoft.com/en-us/download/search.aspx?q=redistributable+package.)

## SSL Connection Error

### Phenomenon

The client cannot establish an SSL connection with EMQX.

### Solution

You can use the keywords in the EMQX log to perform simple troubleshooting. For the EMQX log related content, please refer to: [Log and Trace](../getting-started/log.md).

1. certificate_expired
   
   The `certificate_expired` keyword appears in the log, indicating that the certificate has expired, please renew it in time.

2. no_suitable_cipher

   The `no_suitable_cipher` keyword appears in the log, indicating that a suitable cipher suite was not found during the handshake process. The possible reasons are that the certificate type does not match the cipher suite, the cipher suite supported by both the server and the client was not found, and so on.

3. handshake_failure

   The `handshake_failure` keyword appears in the log. There are many reasons, which may be analyzed in conjunction with the error reported by the client. For example, the client may find that the connected server address does not match the domain name in the server certificate.

4. unknown_ca

   The `unknown_ca` keyword appears in the log, which means that the certificate verification fails. Common reasons are that the intermediate CA certificate is omitted, the Root CA certificate is not specified, or the wrong Root CA certificate is specified. In the two-way authentication, we can judge whether the certificate configuration of the server or the client is wrong according to other information in the log. If there is a problem with the server certificate, the error log is usually:

   ```
   {ssl_error,{tls_alert,{unknown_ca,"TLS server: In state certify received CLIENT ALERT: Fatal - Unknown CA\n"}}}
   ```

   When you see `CLIENT ALERT`, you can know that this is a warning message from the client, and the server certificate fails the client's check.

   If there is a problem with the client certificate, the error log is usually:

   ```
   {ssl_error,{tls_alert,{unknown_ca,"TLS server: In state certify at ssl_handshake.erl:1887 generated SERVER ALERT: Fatal - Unknown CA\n"}}}
   ```

   When you see `SERVER ALERT`, you can know that the server finds that the certificate cannot pass the authentication when checking the client certificate, and the client will receive a warning message from the server.

5. protocol_version

   The `protocol_version` keyword appears in the log, indicating a mismatch between the TLS protocol versions supported by the client and server.
