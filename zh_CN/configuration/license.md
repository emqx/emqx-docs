# License 配置

::: tip

License 的配置仅适用于 EMQX 企业版。

:::

您可以通过配置文件 `emqx.conf` 更新您的 License 文件并配置 License 连接配额使用的设置。配置完成后，您可以在 [EMQX 命令行工具](../admin/cli.md) 中运行 `emqx ctl license reload` 来重新加载 License。

下面是配置 EMQX 企业版 License 设置的示例：

```bash
license {
  key  =  "MjIwMTExCjAKMTAKRXZhbHVhdGlvbgpjb250YWN0QGVtcXguaW8KZGVmYXVsdAoyMDIzMDEwOQoxODI1CjEwMAo=.MEUCIG62t8W15g05f1cKx3tA3YgJoR0dmyHOPCdbUxBGxgKKAiEAhHKh8dUwhU+OxNEaOn8mgRDtiT3R8RZooqy6dEsOmDI="
  connection_low_watermark  =  "60%"
  connection_high_watermark  =  "80%"
}
```

其中，

- `key` 字段包含以 base64 格式编码的 License 密钥。
- `connection_low_watermark` 用于设置 License 连接配额使用率报警解除的阈值；默认值：`"75%"`。
- `connection_high_watermark` 用于设置 License 连接配额使用率报警激活的阈值；默认值：`"80%"`。

执行后，您可以运行 `emqx ctl license info` 来确认新的 License 文件已生效。

::: tip

EMQX 提供了更多配置项以更好地满足定制化需求。详情请参见 [EMQX 开源版配置手册](https://docs.emqx.com/zh/emqx/v@CE_VERSION@/hocon/)和 [EMQX 企业版配置手册](https://docs.emqx.com/zh/enterprise/v@EE_VERSION@/hocon/)。

:::

::: tip

您也可以通过 EMQX Dashboard 或关于如何通过 Dashboard 配置 License，请参见[配置 License](../deploy/license.md)。

一旦您通过 Dashboard 配置了这些配置项，您的设置将覆盖 `emqx.conf` 中相同的配置项。

:::

