# 热升级（Relup）

该插件向运行中的 EMQX 节点应用一组 `.relup` 代码变更指令，使运维人员能够在不重启 VM 的情况下完成补丁发布。

运维人员通过每个节点上的 `emqx ctl relup ...` CLI 触发升级；集群级滚动由运维自行编排（插件内不包含编排逻辑）。

## 何时使用

热升级适用于以下场景：

- 目标升级路径已被 `emqx ctl relup list-supported-paths` 列出（仅支持已声明的 `{from, target}` 跳转）。
- 升级完一个节点后可立即验证再继续下一个。
- 已备份 `data/`：已应用的升级跳转没有原地回滚能力（详见 [回滚](#回滚)）。

如果上述条件无法满足，建议改用常规的滚动重启升级方式。

## 运维操作流程

### 1. 安装插件

从下方 [下载](#下载) 区获取与当前 EMQX 版本匹配的插件包，然后通过 Dashboard（或 REST API / CLI）按常规方式安装。

### 2. 确认升级路径已支持

```bash
emqx ctl relup list-supported-paths
```

输出会列出当前插件版本 `priv/relup/` 中包含的 `{from, target}` 跳转。如果未列出目标路径，则该路径下不支持热升级，需要使用常规的重启升级。

### 3. 在每个节点上准备目标发布包

在每个节点上将以下两个文件放到 EMQX 进程可访问的路径：

- `emqx-enterprise-<TargetVsn>-<os>-<arch>.tar.gz`：目标版本发布包。
- `<tarball>.sha256`：发布包的 sha256 摘要。支持标准 `sha256sum` 格式（`<digest>  <filename>`）。

### 4. 触发升级

在每个节点上执行：

```bash
emqx ctl relup upgrade <TarballPath>
```

升级流程：

- 用 `<TarballPath>.sha256` 校验 tarball 实际摘要，不匹配则拒绝解压。
- 解压 tarball 并读取 `releases/emqx_vars` 中的 `REL_VSN`。
- 在 `priv/relup/*.relup` 中查找匹配的 `{from, target}` 跳转，执行代码变更指令与升级后回调。

### 5. 验证节点

继续之前确认两个关键信号：

- `emqx ctl status` 显示节点正常运行。
- `cat <RootDir>/relup/current` 输出目标版本，且 `<RootDir>/relup/<TargetVsn>/` 下存在 `bin/`、`erts-*/`、`lib/`、`releases/`。

下次执行 `emqx start` / `restart` 时，`bin/emqx` 包装脚本会检测 `relup/current` 并切换到已部署的目录树（新的 ERTS、bin 脚本、lib）。原始 `<RootDir>` 仍是 `data/`、`etc/`、`log/`、`plugins/` 的权威目录。

### 6. 升级成功后清理

集群全部升级到目标版本后，手动删除暂存的发布包及其 `.sha256` 文件。插件不会跟踪源路径，无需做插件侧清理。

## 升级历史

每个节点会在本地的 `emqx_relup_log` 表中保存升级审计记录（基于磁盘存储，仅本节点可见）。卸载插件不会清除该表，重新安装后历史记录依旧可用。

通过 CLI 查看或清理：

```bash
emqx ctl relup logs           # 查看最近的升级记录
emqx ctl relup logs-clear     # 清除本节点上的所有日志
```

## 回滚

已应用的升级跳转无法原地回滚。热升级会在运行中的 VM 上执行 `code_changes`，并可能在 `post_upgrade_callbacks` 中修改磁盘上的数据，这些操作无法通过插件本身回退。

可行的应急路径：

- **在下次重启前**，如升级已完成但新代码出现异常，且磁盘上的数据仍兼容旧版本：

  ```bash
  rm <RootDir>/relup/current
  # 可选：rm -rf <RootDir>/relup/<TargetVsn>/
  emqx restart
  ```

  包装脚本会回退到原始的 `<RootDir>/bin/emqx`。此方式只恢复启动路径，升级失败时 VM 中的运行时状态已无法找回。

- **其他情况下**，使用升级前 `data/`（mnesia、配置等）的备份恢复，并重新安装旧版本 EMQX。规划升级窗口时请考虑这一点。

## 编写升级跳转（开发者参考）

为新版本添加跳转的步骤：

1. 添加 `priv/relup/<from>-to-<to>.relup`，声明该跳转的 `code_changes` 与 `post_upgrade_callbacks`。schema、支持的指令以及升级后回调约定参见插件源码中的 `priv/relup/README.md`。特别注意：当在新版本 EMQX 的 `emqx_post_upgrade` 中新增 `pr_NNNNN_*` 回调时，relup 跳转必须先 reload 该模块再调用回调，或将回调以 `emqx_post_upgrade_<TargetVsn>.erl` 的形式直接放入本插件。
2. 升级本插件的 `VERSION` 并重新发布。

插件启动时会校验 `priv/relup/*.relup` 中的每个条目，对格式错误的文件记录 warning 并跳过，不会因此启动失败。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## 下载

各 EMQX 版本对应的插件安装包：

| EMQX 版本 | 插件版本 | 安装包 |
|---|---|---|
| 5.10.4 | 1.0.0 | [emqx_relup-1.0.0.tar.gz](https://packages.emqx.io/emqx-plugins/e5.10.4/emqx_relup-1.0.0.tar.gz) |

<!-- PLUGIN-DOWNLOADS:END -->
