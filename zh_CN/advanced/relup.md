---
# 编写日期
date: 2020-07-25 14:50:09
# 作者 Github 名称
author: terry-xiaoyu
# 关键字
keywords:
# 描述
description:
# 分类
category:
# 引用
ref:
---

## 版本热升级

自 4.2.0 版本之后，EMQX 支持版本热升级。

使用版本热升级功能，用户可以在保持客户端连接不断开的情况下，快速、安全地升级生产环境中正在运行的 EMQX，并避免了因重启服务导致的系统可用性降低。

::: tip
目前 Windows、MacOSX 暂不支持热升级功能。
:::

:::waning 注意
EMQX 仅允许 Patch 版本（版本号的第三位）的热升级。
即允许 x.y.z 热升级到 x.y.(z+N)，但不允许 x.y 热升级到 (x+N).(y+M)。
请谨慎操作。
:::

:::waning 注意
不允许 EMQX (开源版) 与 EMQX Enterprise 之间的热升级，请谨慎操作。
:::

## 下载热升级 Zip 包

{% emqxce %}
访问 [下载 EMQX](https://www.emqx.com/zh/downloads?product=broker) 选择对应的版本和操作系统类型，然后选择 **"zip"** 包类型。

EMQX 4.4 版本的 Zip 包名格式为：

```
emqx-[EMQX Version]-[OTP Version]-[OS Type]-[Arch].zip
```

以 `emqx-4.4.16-otp24.3.4.2-1-ubuntu20.04-amd64.zip` 为例，其中：

- EMQX Version(`4.4.16`): EMQX 版本号。请确保已经安装的 EMQX 是 4.4.* 版本，而不是 4.3 或者 5.0 等版本。
- OTP Version(`otp24.3.4.2-1`): Erlang OTP 版本号。请确保其首位 (例中为 24) 跟已安装的 EMQX 的 OTP 版本号一致。
- OS Type(`ubuntu20.04`): 操作系统类型。请确保跟已安装的 EMQX 操作系统类型一致。
- Arch(`amd64`): 架构类型。请确保跟已安装的 EMQX 架构类型一致。
{% endemqxce %}

{% emqxee %}
访问 [下载 EMQX 企业版](https://www.emqx.com/zh/try?product=enterprise) 选择对应的版本和操作系统类型，然后选择 **"zip"** 包类型。

EMQX 企业版 4.4 的 Zip 包名格式为：

```
emqx-[EMQX Type]-[EMQX Version]-[OTP Version]-[OS Type]-[Arch].zip
```

:::waning 注意
EMQX 仅允许 Patch 版本（版本号的第三位）的热升级。
即允许 x.y.z 热升级到 x.y.(z+N)，但不允许 x.y 热升级到 (x+N).(y+M)。
请谨慎操作。
:::

:::waning 注意
不允许 EMQX (开源版) 与 EMQX Enterprise 之间的热升级，请谨慎操作。
:::

## 热升级步骤

1. 查看当前已安装的 EMQX 的版本列表。

```bash
$ emqx versions

Installed versions:
* 4.4.0	permanent
```

2. 检查所下载的包的类型是否跟当前已安装的 EMQX 包类型是否一致。
   详见上面的 “下载热升级 Zip 包” 章节。

3. 找到 EMQX 的安装目录：

```bash
$ EMQX_ROOT_DIR=$(emqx root_dir)

$ echo ${EMQX_ROOT_DIR}
"/usr/lib/emqx"
```

4. 将下载的 zip 包放到 EMQX 安装目录下的 `releases` 目录下：

```bash
$ cp emqx-*4.4.16-*.zip ${EMQX_ROOT_DIR}/releases/
```

5. 升级到指定版本:

```bash
$ emqx upgrade 4.4.16

Release 4.4.16 not found, attempting to unpack releases/emqx-4.4.16.tar.gz
Unpacked successfully: "4.4.16"
Installed Release: 4.4.16
Made release permanent: "4.4.16"
```

6. 再次查看版本列表，之前的版本的状态将会变成 `old`:

```bash
$ emqx versions

Installed versions:
* 4.4.16 permanent
* 4.4.0	old
```

:::waning 注意
不要手动删除 `${EMQX_ROOT_DIR}/releases/` 目录下的任何文件。
如果要删除指定版本，请使用 `emqx uninstall` 命令
:::

## 升级后手动持久化

上面的 `emqx upgrade 4.4.16` 命令其实执行了三个动作:

- 解压 zip 包 (`unpack`)
- 安装 (`install`)
- 持久化 (`permanent`)

持久化之后，如果 EMQX 发生重启，使用的将是升级之后的新版本。
如果不想在升级的同时持久化，可以使用 `--no-permanent` 参数：

```bash
$ emqx upgrade --no-permanent 4.4.16

Release 4.4.16 not found, attempting to unpack releases/emqx-4.4.16.tar.gz
Unpacked successfully: "4.4.16"
Installed Release: 4.4.16
```

这时版本已经成功升级到了 4.4.16，但如果重启 EMQX，将会还原到旧版本 4.4.0。
现在如果查看版本列表，会发现 4.4.16 的状态为 "当前版本"(`current`)，而不是持久化的版本：

```bash
$ emqx versions

Installed versions:
* 4.4.16	current
* 4.4.0	permanent
```

在系统稳定运行一段时间后，若决定持久化新版本，可以再次执行 `install` 命令：

```bash
$ emqx install 4.4.16

Release 4.4.16 is already installed and current, making permanent.
Made release permanent: "4.4.16"
```

## 版本降级

如果升级后发现问题想要回退，可以执行版本降级命令。
比如下面的例子会将 EMQX 回退到 4.4.0 版本:

```bash
$ emqx downgrade 4.4.0

Release 4.4.0 is marked old, switching to it.
Installed Release: 4.4.0
Made release permanent: "4.4.0"
```

:::warning 注意
由于 EMQX 是向后兼容而不是向前兼容的，只有在升级完成后马上降级才是安全的，以确保在降级版本之前没有修改过任何数据。
如果您在升级之后通过 Dashboard，API 或 CLI 修改了任何数据，降级后的系统的行为是未知的。
我们的建议是只做版本升级，永远不做版本降级。如果您在升级后发现了任何问题，不要降级，请询问社区或者联系我们的技术支持。
:::

## 卸载版本

在系统稳定运行一段时间后，若决定删除一个旧版本，可以执行版本卸载命令。
比如下面的例子将会卸载旧版本的 4.4.0:

```bash
$ emqx uninstall 4.4.0

Release 4.4.0 is marked old, uninstalling it.
Uninstalled Release: 4.4.0
```
