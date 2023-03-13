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

{% emqxce %}

自 4.2.0 版本之后，EMQX 支持版本热升级。

使用版本热升级功能，用户可以快速、安全地升级生产环境的 EMQX，并避免了因重启服务导致的系统可用性降低。

::: tip
目前 Windows、MacOSX 暂不支持热升级功能。
:::

:::
EMQX 仅允许 Patch 版本（版本号的第三位）的热升级。
即允许 x.y.z 热升级到 x.y.(z+N)，但不允许 x.y 热升级到 (x+N).(y+M)。
请谨慎操作。
:::

:::
不允许 EMQX (开源版) 与 EMQX Enterprise 之间的热升级，请谨慎操作。
:::

## 热升级步骤

1. 查看当前已安装的 EMQX 的版本列表。

```bash

$ emqx versions

Installed versions:
* 4.2.0	permanent
```

2. 从 EMQX 官网下载要升级的软件包。

访问 [EMQX](https://www.emqx.com/en/downloads?product=broker) 选择对应的版本和操作系统类型，然后选择 **"zip"** 包类型。

3. 找到 EMQX 的安装目录：

```bash

$ EMQX_ROOT_DIR=$(emqx root_dir)

$ echo ${EMQX_ROOT_DIR}
"/usr/lib/emqx"

```

4. 将下载的 zip 包放到 EMQX 安装目录下的 `releases` 目录下：

```bash

$ cp emqx-4.2.1.zip ${EMQX_ROOT_DIR}/releases/

```

5. 升级到指定版本:

```bash

$ emqx upgrade 4.2.1

Release 4.2.1 not found, attempting to unpack releases/emqx-4.2.1.tar.gz
Unpacked successfully: "4.2.1"
Installed Release: 4.2.1
Made release permanent: "4.2.1"
```

6. 再次查看版本列表，之前的版本的状态将会变成 `old`:

```bash

$ emqx versions

Installed versions:
* 4.2.1	permanent
* 4.2.0	old
```

## 升级后手动持久化

上面的 `emqx upgrade 4.2.1` 命令其实执行了三个动作:

- 解压 zip 包 (`unpack`)
- 安装 (`install`)
- 持久化 (`permanent`)

持久化之后，这次版本升级将会被固定下来，这意味着热升级后，如果 EMQX 发生重启，使用的将是升级之后的新版本。
如果不想在升级的同时持久化，可以使用 `--no-permanent` 参数：

```bash

$ emqx upgrade --no-permanent 4.2.1

Release 4.2.1 not found, attempting to unpack releases/emqx-4.2.1.tar.gz
Unpacked successfully: "4.2.1"
Installed Release: 4.2.1

```

这时版本已经成功升级到了 4.2.1，但如果重启 EMQX，将会还原到旧版本 4.2.0。
现在如果查看版本列表，会发现 4.2.1 的状态为 "当前版本"(`current`)，而不是持久化版本：

```bash

$ emqx versions

Installed versions:
* 4.2.1	current
* 4.2.0	permanent

```

在系统稳定运行一段时间后，若决定持久化新版本，可以再次执行 `install` 命令：

```bash

$ emqx install 4.2.1

Release 4.2.1 is already installed and current, making permanent.
Made release permanent: "4.2.1"

```

## 版本降级

如果升级后发现问题想要回退，可以执行版本降级命令。
比如下面的例子会将 EMQX 回退到 4.2.0 版本:

```bash

$ emqx downgrade 4.2.0

Release 4.2.0 is marked old, switching to it.
Installed Release: 4.2.0
Made release permanent: "4.2.0"

```

## 删除版本

在系统稳定运行一段时间后，若决定删除一个旧版本，可以执行版本卸载命令。
比如下面的例子将会卸载旧版本的 4.2.0:

```bash

$ emqx uninstall 4.2.0

Release 4.2.0 is marked old, uninstalling it.
Uninstalled Release: 4.2.0

```

{% endemqxce %}

{% emqxee %}

自 4.2.0 版本之后，EMQX Enterprise 支持版本热升级。

使用版本热升级功能，用户可以快速、安全地升级生产环境的 EMQX Enterprise，并避免了因重启服务导致的系统可用性降低。

::: tip
目前 Windows、MacOSX 暂不支持热升级功能。
:::

:::
EMQX 仅允许 Patch 版本（版本号的第三位）的热升级。
即允许 x.y.z 热升级到 x.y.(z+N)，但不允许 x.y 热升级到 (x+N).(y+M)。
请谨慎操作。
:::

:::
不允许 EMQX (开源版) 与 EMQX Enterprise 之间的热升级，请谨慎操作。
:::

## 热升级步骤

1. 查看当前已安装的 EMQX Enterprise 的版本列表。

```bash

$ emqx versions

Installed versions:
* 4.2.0	permanent
```

2. 从 EMQX Enterprise 官网下载要升级的软件包。

访问 [EMQX Enterprise](https://www.emqx.com/en/try?product=enterprise) 选择对应的版本和操作系统类型，然后选择 **"zip"** 包类型。

3. 找到 EMQX Enterprise 的安装目录：

```bash

$ EMQX_ROOT_DIR=$(emqx root_dir)

$ echo ${EMQX_ROOT_DIR}
"/usr/lib/emqx"

```

4. 将下载的 zip 包放到 EMQX Enterprise 安装目录下的 `releases` 目录下：

```bash

$ cp emqx-4.2.1.zip ${EMQX_ROOT_DIR}/releases/

```

5. 升级到指定版本:

```bash

$ emqx upgrade 4.2.1

Release 4.2.1 not found, attempting to unpack releases/emqx-4.2.1.tar.gz
Unpacked successfully: "4.2.1"
Installed Release: 4.2.1
Made release permanent: "4.2.1"
```

6. 再次查看版本列表，之前的版本的状态将会变成 `old`:

```bash

$ emqx versions

Installed versions:
* 4.2.1	permanent
* 4.2.0	old
```

## 升级后手动持久化

上面的 `emqx upgrade 4.2.1` 命令其实执行了三个动作:

- 解压 zip 包 (`unpack`)
- 安装 (`install`)
- 持久化 (`permanent`)

持久化之后，这次版本升级将会被固定下来，这意味着热升级后，如果 EMQX 发生重启，使用的将是升级之后的新版本。
如果不想在升级的同时持久化，可以使用 `--no-permanent` 参数：

```bash

$ emqx upgrade --no-permanent 4.2.1

Release 4.2.1 not found, attempting to unpack releases/emqx-4.2.1.tar.gz
Unpacked successfully: "4.2.1"
Installed Release: 4.2.1

```

这时版本已经成功升级到了 4.2.1，但如果重启 EMQX，将会还原到旧版本 4.2.0。
现在如果查看版本列表，会发现 4.2.1 的状态为 "当前版本"(`current`)，而不是持久化版本：

```bash

$ emqx versions

Installed versions:
* 4.2.1	current
* 4.2.0	permanent

```

在系统稳定运行一段时间后，若决定持久化新版本，可以再次执行 `install` 命令：

```bash

$ emqx install 4.2.1

Release 4.2.1 is already installed and current, making permanent.
Made release permanent: "4.2.1"

```

## 版本降级

如果升级后发现问题想要回退，可以执行版本降级命令。
比如下面的例子会将 EMQX Enterprise 回退到 4.2.0 版本:

```bash

$ emqx downgrade 4.2.0

Release 4.2.0 is marked old, switching to it.
Installed Release: 4.2.0
Made release permanent: "4.2.0"

```

## 删除版本

在系统稳定运行一段时间后，若决定删除一个旧版本，可以执行版本卸载命令。
比如下面的例子将会卸载旧版本的 4.2.0:

```bash

$ emqx uninstall 4.2.0

Release 4.2.0 is marked old, uninstalling it.
Uninstalled Release: 4.2.0

```

{% endemqxee %}
