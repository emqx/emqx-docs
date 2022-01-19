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

# Start EMQ X

Start EMQ X broker in the background

```bash
$ emqx start
EMQ X Broker v4.0.0 is started successfully!
```

systemctl start

```bash
$ sudo systemctl start emqx
EMQ X Broker v4.0.0 is started successfully!
```

service start

```bash
$ sudo service emqx start
EMQ X Broker v4.0.0 is started successfully!
```

::: tip
If you are using EMQ X Enterprise, you need to import a license to use it. For the import steps, see **Start EMQ X Enterprise** below.

EMQ X Broker installed via ZIP archive does not support systemctl and service startup.

:::


## Check the status of EMQ X Broker

EMQ X Broker starts normally:

```bash
$ emqx_ctl status
Node 'emqx@127.0.0.1' is started
emqx 4.0.0 is running
```

EMQ X Broker failed to start normally:

```bash
$ emqx_ctl status
Node 'emqx@127.0.0.1' not responding to pings。
```

You can check the log file from [`logs`](getting-started/directory.md) and confirm whether it belongs to [Common Error](faq/error.md#).

## Start EMQ X Enterprise
EMQ X Enterprise needs a license file to start normally. EMQ X can skip this step.


## Request a trial license

EMQ X Enterprise requires a license file to start normally. EMQ X Broker can skip this step.

1. Visit `https: // emqx.io`. On the EMQ X Enterprise download page, click **Get FREE Trial License**.

    ![](./static/WX20200210-153301@2x.png)

2. Register to log in and apply for a trial license file, then download the license file.

    ![](./static/WX20200210-153822@2x.png)

3. Replace the license file (`etc/emqx.lic`) in the default certificate directory. You can also choose to change the read path of the certificate file, modify `license.file` in the `etc/emqx.conf` file, and make sure that the license file is in the updated read path and EMQ X Enterprise has read permission. Then, start EMQ X Enterprise. EMQ X Enterprise is started in the same way as EMQ X Broker, which can be seen below.

2. If the running EMQ X Enterprise needs to update the license file, you can use the `emqx_ctl license reload [path of the license file]` command to directly update the license file without restarting EMQ X Enterprise. It should be noted that the certificate loaded by the `emqx_ctl license reload` command will only take effect during this run of EMQ X Enterprise. If you need to permanently update the license certificate path, you still need to replace the old certificate or modify the configuration file, which can be seen from the previous step.


::: danger
The certificate loaded by the `emqx_ctl license reload` command will only take effect during this runtime of EMQ X Enterprise. If you need to permanently update the path of the License certificate, you still need to replace the old certificate or modify the configuration file.
:::
