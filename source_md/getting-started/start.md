---
# 标题
title: 启动 EMQ X
# 编写日期
date: 2020-02-07 17:15:26
# 作者 Github 名称
author: wivwiv
# 关键字
keywords:
# 描述
description:
# 分类
category: 
# 引用
ref: undefined
---

# 启动 EMQ X Broker

EMQ X Enterprise 需要 License 文件才能正常启动，EMQ X Broker 可以跳过这一步，关于 License 的详情请查看 [申请与导入 License](getting-started/license.md)。

+ 后台启动 EMQ X Broker

    ```
    $ emqx start
    EMQ X Broker v4.0.0 is started successfully!
    ```

+ systemctl 启动

    ```
    $ sudo systemctl start emqx
    EMQ X Broker v4.0.0 is started successfully!
    ```

+ service 启动

    ```
    $ sudo service emqx start
    EMQ X Broker v4.0.0 is started successfully!
    ```

通过 ZIP 压缩包安装的 EMQ X Broker 不支持通过 systemctl 和 service 启动。

## 使用 `emqx_ctl status` 查看 EMQ X Broker 的状态

EMQ X Broker 正常启动: 
```
$ emqx_ctl status
Node 'emqx@127.0.0.1' is started
emqx 4.0.0 is running
```

EMQ X Broker 未能正常启动:
```
$ emqx_ctl status
Node 'emqx@127.0.0.1' not responding to pings。
```

如果 EMQ X Broker 未能正常启动，查看 [`logs` 目录](using-emqx/directory.md)下的日志文件，并参考我们的 [FAQ](faq/index.md#) 进行排错。