# License Configuration

::: tip

The license configuration applies to the EMQX Enterprise edition only.

:::

You can update your license by using `emqx ctl license update` command. The command will update the license file and reload the license. 

```bash
emqx ctl license update <license_key>
```

or

```bash
# Make sure the license file is uploaded to all nodes in the cluster at the same file path.
emqx ctl license update 'file://<license_file_path>'
```

Where,

- `<license>` is the license key string without new lines.
- `<license_file_path>` is the path to the file where the license key is stored.

If the license is configured via `'file://<license_file_path>'` notation, EMQX will be automatically reloading it every 2 minutes.

Additionally, you can configure the settings for the license connection quota usage in the configuration file `emqx.conf`.

```bash
license {
  key  =  "MjIwMTExCjAKMTAKRXZhbHVhdGlvbgpjb250YWN0QGVtcXguaW8KZGVmYXVsdAoyMDIzMDEwOQoxODI1CjEwMAo=.MEUCIG62t8W15g05f1cKx3tA3YgJoR0dmyHOPCdbUxBGxgKKAiEAhHKh8dUwhU+OxNEaOn8mgRDtiT3R8RZooqy6dEsOmDI="
  connection_low_watermark  =  "60%"
  connection_high_watermark  =  "80%"
}
```

Where,

- `key` is the field contains a license key encoded in base64 format.
- `connection_low_watermark` is to set a threshold below which license connection quota usage alarms are deactivated; default: `"75%"`
- `connection_high_watermark` is to set a threshold above which license connection quota usage alarms are activated; default: `"80%"`

After execution, you can run `emqx ctl license info` to confirm that the new license file has taken effect.

::: tip

You can also configure the license file through the EMQX Dashboard. For how to configure the license via Dashboard, see [Work with License](../deploy/license.md). Once you configured these items with the Dashboard, your settings will override the same configuration items in `emqx.conf`.

:::

::: tip

EMQX offers more configuration items to serve customized needs better. For details, see the [EMQX Open Source Configuration Manual](https://docs.emqx.com/en/emqx/v@CE_VERSION@/hocon/) and [EMQX Enterprise Configuration Manual for Enterprise](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/).

:::
