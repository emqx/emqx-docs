## 运行时安装热补丁

从 4.3 开始，EMQX 支持更简单的热补丁安装。
相比[热升级](./relup.md)，热补丁的安装是一个临时的问题修复方案。

::: tip 注意
当修复发布到正式版之后，建议升级到正式版。
热补丁用到的文件在热升级之前应该删除。
:::

## 安装 EMQX 热补丁步骤

1. 从 EMQX 开发者那里获取本次需要更新的 modules 列表。比如：

```
emqx.beam
emqx_rule_engine.beam
```

2. 拷贝补丁文件到 `data/patches` 目录

```bash
cp /path/to/patch/emqx.beam data/patches
cp /path/to/patch/emqx_rule_engine.beam data/patches
```

`data/patches` 目录的位置因配置和安装方式而异。
通常可以在这些地方找到:

* 环境变量 `EMQX_NODE__DATA_DIR` 指向的位置
* 配置文件中 `node.data_dir` 指向的位置
* 在docker中运行的默认位置: `/opt/emqx/data` （通常是一个挂在的外部volume）
* 直接使用 zip 安装包的默认位置: `<install-path>/data`
* 使用RPM或DEB安装包安装的默认位置：`/var/lib/emqx/`

3. 加载新的 beam 文件:

```bash
$ emqx eval 'c:l(emqx).'
{module,emqx}

$ emqx eval 'c:l(emqx_rule_engine).'
{module,emqx_rule_engine}
```

## 回滚补丁包

1. 把安装过的补丁文件删除

```bash
$ mv data/patches/emqx.beam /tmp/
$ mv data/patches/emqx_rule_engine.beam /tmp/
```

2. 重新加载 beam 文件:

```bash
$ emqx eval 'c:l(emqx).'
{module,emqx}

$ emqx eval 'c:l(emqx_rule_engine).'
{module,emqx_rule_engine}
```