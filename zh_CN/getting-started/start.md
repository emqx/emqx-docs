# 启动 EMQX

后台启动 EMQX

```bash
$ emqx start
EMQX v4.0.0 is started successfully!
```

systemctl 启动

```bash
$ sudo systemctl start emqx
EMQX v4.0.0 is started successfully!
```

service 启动

```bash
$ sudo service emqx start
EMQX v4.0.0 is started successfully!
```

{% emqxce %}

::: tip
如果你使用的是 EMQX Enterprise 则需要导入 License 才能使用，导入步骤见下文**启动 EMQX Enterprise**。

通过 ZIP 压缩包安装的 EMQX 不支持通过 systemctl 和 service 启动。

4.2-rc.1 版本后，EMQX 开源版新加入[遥测](../advanced/telemetry.md)功能，启动前请[详细了解](../advanced/telemetry.md)。

:::

{% endemqxce %}

## 查看 EMQX 的状态

EMQX 正常启动: 

```bash
$ emqx_ctl status
Node 'emqx@127.0.0.1' is started
emqx 4.0.0 is running
```

EMQX 未能正常启动:

```bash
$ emqx_ctl status
Node 'emqx@127.0.0.1' not responding to pings。
```

你可以查看 [`logs`](../getting-started/directory.md) 下的日志文件并确认是否属于 [常见错误](../faq/error.md#)。

{% emqxce %}

## 启动 EMQX Enterprise
EMQX Enterprise 需要 License 文件才能正常启动，EMQX 可以略过这一步。

{% endemqxce %}

{% emqxee %}

## License

EMQX Enterprise 需要 License 文件才能正常启动，请联系销售人员或在线自助购买/申请试用以获取 License。

- 试用版 License：到期后将停止正在运行的 EMQX；
- 正式版 License：到期后不会停止正在运行的 EMQX，但是新节点或手动停止之后的节点将无法启动。

{% endemqxee %}


### 申请试用 License

- 访问 [EMQX Enterprise 下载页面](https://www.emqx.com/zh/downloads?product=enterprise)，点击 **[免费获取 License](https://www.emqx.com/zh/apply-licenses/emqx)**。

    ![](./static/download_enterprise_page.png)

- 申请 License 文件试用，下载 License 文件。

    ![](./static/apply_license.png)

### 放置 License

- 替换默认证书目录下的 License 文件（`etc/emqx.lic`），当然你也可以选择变更证书文件的读取路径，修改 `etc/emqx.conf` 文件中的 `license.file`，并确保 License 文件位于更新后的读取路径且 EMQX Enterprise 拥有读取权限，然后启动 EMQX Enterprise。EMQX Enterprise 的启动方式与 EMQX 相同，见下文。

- 如果是正在运行的 EMQX Enterprise 需要更新 License 文件，那么可以使用 `emqx_ctl license reload [license 文件所在路径]` 命令直接更新 License 文件，无需重启 EMQX Enterprise。

::: tip
`emqx_ctl license reload` 命令加载的证书仅在 EMQX Enterprise 本次运行期间生效，如果需要永久更新 License 证书的路径，依然需要替换旧证书或修改配置文件。
:::
