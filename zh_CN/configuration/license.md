# License

This is to configure the license for EMQX. 

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

:::tip

To configure the license via Dashboard, click **Update License** in the lower left corner of the Dashboard homepage. Once you configured these items with the Dashboard, your settings will override the same configuration items in `emqx.conf`.

EMQX has offered more configuration items to better serve customized needs, you can continue to read [Configuration Manual](./configuration-manual.md).

:::