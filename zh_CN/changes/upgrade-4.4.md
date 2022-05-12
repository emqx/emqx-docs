---
# 编写日期
date: 2020-12-23 10:48:26
# 作者 Github 名称
author: zhongwencool
# 关键字
keywords:
# 描述
description:
# 分类
category:
# 引用
ref:
---

# 从 4.3 升级到 4.4 版本

4.3 和 4.4 的节点可以运行在同一集群中，可以采用滚动升级从 4.3 升级到 4.4 的最新版本。**注意是4.4的最新版本**
比如：connection_low_watermark_alarm 配置为 e4.3.7 e4.4.1 新增加了字段。 如果从 e4.3.7 升级到 e4.4.0时，就会无法识别，需要升级到 e4.4.1，
所以推荐升级到 4.4 的最新版本。

推荐按以下步骤完成滚动升级：

1. 可选步骤：把准备升级的节点从负载均衡器中移除
1. 停止节点 (e.g. `emqx stop`，或者 `systemctl stop emqx`)
1. 在 4.3 节点上备份data及etc目录（见下文）。
1. 卸载 4.3 版本。
1. 安装 4.4 版本，并把备份数据data及配置etc覆盖到相应位置。
1. 启动 4.4 版本，检查系统是否正常
1. 可选步骤：把流量导入到此节点。

## 数据及配置备份

在升级前，应备在每个4.3版本的节点上对 `data` `etc`目录中的数据和配置进行备份。
根据不同的安装方式和配置，`data` `etc` 目录可能在一下这些位置

* 环境变量 `EMQX_NODE__DATA_DIR` 指向的位置
* 配置文件中 `node.data_dir` 指向的位置
* 在docker中运行的默认位置: `/opt/emqx/data` `opt/emqx/etc` （通常是一个挂在的外部volume）
* 直接使用 zip 安装包的默认位置: `<install-path>/data` `<install-path>/etc`
* 使用RPM或DEB安装包安装的默认位置：`/var/lib/emqx/` `/etc/emqx`

以 RPM 默认安装为例，你需要

```bash
## 创建备份文件夹
mkdir -p ~/emqx-backup/etc/
mkdir -p ~/emqx-backup/data/

## 确认本节点emqx已经停止运行
systemctl stop emqx
systemctl status emqx

## 复制备份文件
cp -r /etc/emqx ~/emqx-backup/etc/
cp -r /var/lib/emqx/ ~/emqx-backup/data/
```

## 卸载 4.3 版本

以RPM默认安装为例：

```bash
## 查看确认安装的版本是否为需要卸载的
rpm -qa | grep emqx

## 确认无误后卸载旧版本
rpm -e emqx-4.3.x-x.x86_64
```

## 安装 4.4 版本， 并导入4.3版本数据。

- 以RPM默认安装为例：

```bash
rpm -ivh emqx-ee-4.4.0-otp24.1.5-3-centos7-amd64.rpm
```

- 导入上步已备份的4.3数据及配置。

```bash
cp -r ~/emqx-backup/etc/ /etc/emqx/
cp -r ~/emqx-backup/data/ /var/lib/emqx/
```

## 启动 4.4 版本

如果你使用的是systemctl启动：

```bash
systemctl start emqx
systemctl status emqx
```

- 通过emqx_ctl 查看集群状态

```bash
/usr/bin/emqx_ctl cluster status

```

- 查看日志是否有异常

```bash
## 找到最新的写入日志文件
ls -htl /var/log/emqx/emqx.log.*[0-9] |head -n 1
## 查看此日志：
tail -f -n 100 /var/log/emqx/emqx.log.x
```

- 通过dashboard查看是否有报警，本节点的状态是否正常
