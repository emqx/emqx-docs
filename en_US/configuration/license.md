# License Configuration

You can update your license file and configure the settings for the license connection quota usage the configuration file `emqx.conf`. After the configuration, you can run `emqx ctl license reload` in [EMQX command line tool](../admin/cli.md) to reload the license.  

Below is an example of configuring the EMQX Enterprise license settings: 

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

{% emqxce %}

EMQX has offered more configuration items to serve customized needs better. For details, see [Configuration Manual](https://www.emqx.io/docs/en/v@CE_VERSION@/hocon/).

{% endemqxce %}

{% emqxee %}

EMQX has offered more configuration items to serve customized needs better. For details, see [Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/).

::: tip

You can also configure the license file through the EMQX Dashboard. For how to configure the license via Dashboard, see [Work with License](../deploy/license.md). Once you configured these items with the Dashboard, your settings will override the same configuration items in `emqx.conf`.

:::

{% endemqxee %}