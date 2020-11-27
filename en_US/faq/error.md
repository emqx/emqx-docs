# Common Errors
### EMQ X cannot connect to Mysql8.0

**Tags:** [*MySQL*](tags.md#mysql)  [*Auth*](tags.md#auth)


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

  ### Incorrect OPENSSL version

  **Tags:** [*fail to start*](tags.md#启动失败)

  #### Phenomenon

  The output error  from executing `./bin/emqx console` includes:

  ```bash
  \{application_start_failure,kernel,\{\{shutdown,\{failed_to_start_child,kernel_safe_sup,\{on_load_function_failed,crypto\}\}\}, ..\}
  ```

  It indicates that the "crypto" application in Erlang/OTP that EMQ X depends on failed to start.

  #### Solution

  ##### Linux

  Go to the installation directory of EMQ X (If you use the package management tool to install EMQ X, you should enter the same level directory as the `lib` of EMQ X)

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

  After the completion, execute `ldd lib/crypto-*/priv/lib/crypto.so` in the lib-level directory of EMQ X to check whether it can be correctly identified. If there is no `.so` library in `not found`, you can start EMQ X normally.


  ##### macOS

  Go to the installation directory of EMQ X:

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

  After the installation is complete, you can start EMQ X normally.

  ### MSVCR120.dll is missing from Windows

  **Tags:** [*fail to start*](tags.md#启动失败)

  #### Phenomenon

  When Windows executes `./bin/emqx console`, an error window pops up:

  ```bash
  This program cannot be started because MSVCR120.dll is missing from the computer. Please try to reinstall the program to resolve this issue.
  ```

  ![error](D:/emqx/emqx-docs-cn/faq/static/WechatIMG18396.png)

  #### Solution

  Install [Microsoft Visual C++ RedistributablePackage](https://www.microsoft.com/en-us/download/search.aspx?q=redistributable+package.)