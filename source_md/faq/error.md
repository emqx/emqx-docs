# 常见错误
### EMQ X 无法连接 MySQL 8.0

**标签:** [*MySQL*](tags.md#mysql)  [*认证*](tags.md#认证)


不同于以往版本，MySQL 8.0 对账号密码配置默认使用`caching_sha2_password`插件，需要将密码插件改成`mysql_native_password`

  + 修改 `mysql.user` 表

    ```
    ## 切换到 mysql 数据库
    mysql> use mysql;

    ## 查看 user 表

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

    ## 修改密码插件
    mysql> ALTER USER 'your_username'@'your_host' IDENTIFIED WITH mysql_native_password BY 'your_password';
    Query OK, 0 rows affected (0.01 sec)

    ## 刷新
    mysql> FLUSH PRIVILEGES;
    Query OK, 0 rows affected (0.00 sec)
    ```

  + 修改 `my.conf`
    
    在 `my.cnf` 配置文件里面的 [mysqld] 下面加一行
    ```
    default_authentication_plugin=mysql_native_password
    ```

  + 重启 MySQL 即可