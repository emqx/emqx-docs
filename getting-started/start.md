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

后台启动 EMQ X Broker

```bash
$ emqx start
EMQ X v4.0.0 is started successfully!
```

systemctl 启动

```bash
$ sudo systemctl start emqx
EMQ X v4.0.0 is started successfully!
```

service 启动

```bash
$ sudo service emqx start
EMQ X v4.0.0 is started successfully!
```

{% hint style="info" %}
如果你使用的是 EMQ X Enterprise 则需要导入 License 才能使用，导入步骤见下文**启动 EMQ X Enterprise**。

通过 ZIP 压缩包安装的 EMQ X 不支持通过 systemctl 和 service 启动。
{% endhint %}

## 查看 EMQ X 的状态

EMQ X 正常启动: 

```bash
$ emqx_ctl status
Node 'emqx@127.0.0.1' is started
emqx 4.0.0 is running
```

EMQ X 未能正常启动:

```bash
$ emqx_ctl status
Node 'emqx@127.0.0.1' not responding to pings。
```

你可以查看 [`logs`](using-emqx/directory.md) 下的日志文件并确认是否属于 [常见错误](faq/error.md#)。


## 启动 EMQ X Enterprise

EMQ X Enterprise 需要 License 文件才能正常启动，EMQ X 可以略过这一步。

1. 访问 `https://emqx.io`，在 EMQ X Enterprise 下载页面，点击 **Get FREE Trial License**。

    ![](./static/WX20200210-153301@2x.png)

2. 注册登陆并申请 License 文件试用，下载 License 文件。

    ![](./static/WX20200210-153822@2x.png)

3. 替换默认证书目录下的 License 文件（`etc/emqx.lic`），当然你也可以选择变更证书文件的读取路径，修改 `etc/emqx.conf` 文件中的 `license.file`，并确保 License 文件位于更新后的读取路径且 EMQ X Enterprise 拥有读取权限，然后启动 EMQ X Enterprise。EMQ X Enterprise 的启动方式与 EMQ X 相同，见下文。

4. 如果是正在运行的 EMQ X Enterprise 需要更新 License 文件，那么可以使用 `emqx_ctl license reload [license 文件所在路径]` 命令直接更新 License 文件，无需重启 EMQ X Enterprise。需要注意的是，`emqx_ctl license reload` 命令加载的证书仅在 EMQ X Enterprise 本次运行期间生效，如果需要永久更新 License 证书的路径，依然需要替换旧证书或修改配置文件，请参考上一步。