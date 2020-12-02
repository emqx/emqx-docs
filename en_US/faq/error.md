---
# 标题
title: 常见错误
# 编写日期
date: 2020-02-20 12:44:32
# 作者 Github 名称
author: wivwiv
# 关键字
keywords:
# 描述
description:
# 分类
category:
# 引用
ref:
---
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