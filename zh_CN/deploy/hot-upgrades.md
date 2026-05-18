# 版本热升级

热升级允许在不停止 EMQX Enterprise 节点的情况下应用补丁版本更新。升级过程中连接保持不中断，节点在下次重启时切换到新的 Release 目录。

热升级仅支持补丁版本（版本号第三位）跨度。例如，支持从 5.10.3 升级到 5.10.4，但不支持从 5.10.x 升级到 5.11.0。

热升级由 `emqx_relup` 插件驱动，通过 `emqx ctl relup` CLI 操作，不提供 Dashboard 或 REST API 界面。

::: warning 重要提示
执行热升级前，请备份 `data/`、`etc/` 和 `log/` 目录。升级完成后无法原地回滚。
:::

## 前提条件

- 每个节点上的 EMQX Enterprise 均处于运行状态。
- 具有 Shell 访问权限，可在每个节点上执行 `emqx ctl` 命令。
- `emqx_relup` 插件版本与当前 EMQX Enterprise 版本匹配。
- 目标版本已作为受支持的升级路径包含在插件中（在步骤 2 中验证）。

## 步骤 1：安装 emqx_relup 插件

在每个节点上下载并安装插件。插件包发布地址为 `https://packages.emqx.io/emqx-plugins/e<EMQX-VSN>/`。

```bash
# 下载插件包
curl -fLO https://packages.emqx.io/emqx-plugins/e<EMQX-VSN>/emqx_relup-<PLUGIN-VSN>.tar.gz

# 复制到插件安装目录
#   deb/rpm 安装方式：/var/lib/emqx/plugins/
#   压缩包安装方式：<RootDir>/plugins/
cp emqx_relup-<PLUGIN-VSN>.tar.gz <plugin-install-dir>/

# 安装、启用并启动插件
emqx ctl plugins install emqx_relup-<PLUGIN-VSN>
emqx ctl plugins enable  emqx_relup-<PLUGIN-VSN>
emqx ctl plugins start   emqx_relup-<PLUGIN-VSN>
```

验证插件是否正在运行：

```bash
emqx ctl plugins list
```

插件的状态应显示为 `running`。

## 步骤 2：确认升级路径受支持

列出已安装插件支持的版本跨度：

```bash
emqx ctl relup list-supported-paths
```

在继续操作之前，确认当前 `{from, target}` 版本对出现在输出中。

## 步骤 3：准备压缩包和 SHA256 校验文件

将以下两个文件复制到每个节点，可放置于 EMQX 进程可读的任意位置：

- `emqx-enterprise-<TargetVsn>-<os>-<arch>.tar.gz`：EMQX Enterprise 目标版本压缩包。
- `emqx-enterprise-<TargetVsn>-<os>-<arch>.tar.gz.sha256`：压缩包的 SHA256 摘要文件。支持纯摘要格式和 `sha256sum` 输出格式（`<digest>  <filename>`）。

```bash
scp emqx-enterprise-<TargetVsn>-<os>-<arch>.tar.gz       node1:/opt/upgrade/
scp emqx-enterprise-<TargetVsn>-<os>-<arch>.tar.gz.sha256 node1:/opt/upgrade/
```

`.sha256` 文件必须与压缩包放在同一目录下，且文件名为压缩包名加 `.sha256` 后缀。

## 步骤 4：执行升级

在每个节点上，指定压缩包路径触发升级：

```bash
emqx ctl relup upgrade /opt/upgrade/emqx-enterprise-<TargetVsn>-<os>-<arch>.tar.gz
```

升级程序将执行以下步骤：

1. 验证 SHA256 摘要，摘要不匹配时拒绝继续。
2. 解压压缩包并从 `releases/emqx_vars` 中读取目标版本号。
3. 仅将运行时子目录（`bin/`、`erts-*/`、`lib/`、`releases/`）部署到 `<RootDir>/relup/<TargetVsn>/`。配置、数据、日志和插件仍保留在原始安装根目录下。
4. 应用插件 `.relup` 目录中匹配的代码变更指令。
5. 运行升级后回调。
6. 将目标版本号写入 `<RootDir>/relup/version`。

集群范围的升级推进由操作者负责。建议逐节点升级，验证无误后再继续下一个节点。

## 步骤 5：验证节点

升级命令返回后，确认节点状态正常：

```bash
# 检查节点是否正在运行
emqx ctl status

# 确认版本标记文件已写入
cat <RootDir>/relup/version

# 查看升级状态和历史记录
emqx ctl relup status
emqx ctl relup logs
```

`relup/version` 文件应包含目标版本号字符串，`emqx ctl status` 应显示节点正在运行。

## 步骤 6：重启切换到新版本

代码变更升级在当前运行的虚拟机中立即生效。若要完全切换到新的 ERTS 运行时和二进制文件，需重启节点：

```bash
emqx restart
```

重启时，`bin/emqx` 脚本检测到 `<RootDir>/relup/version` 后，会切换执行 `<RootDir>/relup/<TargetVsn>/` 中的程序。原始 `<RootDir>` 仍作为 `data/`、`etc/`、`log/` 和 `plugins/` 的权威目录。

## 步骤 7：清理临时文件

整个集群切换到目标版本后，删除临时升级文件：

```bash
rm /opt/upgrade/emqx-enterprise-<TargetVsn>-<os>-<arch>.tar.gz
rm /opt/upgrade/emqx-enterprise-<TargetVsn>-<os>-<arch>.tar.gz.sha256
```

插件不追踪源文件路径，插件内部无需额外清理。

## 回滚

热升级完成后无法原地回滚。代码变更已对运行中的虚拟机生效，升级后回调可能已修改磁盘数据，本插件不支持撤销这些操作。

以下两种有限的恢复方式可供参考：

- **重启前：** 如果升级后代码出现异常，但磁盘状态仍与旧版本兼容，可删除版本标记文件并重启，恢复到旧版本目录：

  ```bash
  rm <RootDir>/relup/version
  # 可选：rm -rf <RootDir>/relup/<TargetVsn>/
  emqx restart
  ```

  此操作仅恢复启动路径，升级过程中对运行时状态的修改已生效，无法撤销。

- **完整恢复：** 从升级前的备份中恢复 `data/` 目录，并重新安装旧版本的 EMQX。

请在规划升级窗口时充分考虑上述限制。

## CLI 参考

| 命令 | 说明 |
|---|---|
| `emqx ctl relup upgrade <TarballPath>` | 使用指定压缩包应用升级跨度。 |
| `emqx ctl relup list-supported-paths` | 列出插件目录中支持的 `{当前版本, 目标版本}` 升级路径。 |
| `emqx ctl relup status` | 显示升级状态：`in-progress`（进行中）或 `idle`（空闲）。 |
| `emqx ctl relup logs` | 打印本节点持久化日志表中的升级历史记录。 |
| `emqx ctl relup logs-clear` | 清空本节点的升级日志表。 |
