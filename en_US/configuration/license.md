# License

This is configure the license for EMQX. 

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