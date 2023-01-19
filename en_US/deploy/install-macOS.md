# macOS

This section will guide you on how to install and start EMQX on macOS with a zip file.

{%emqxee%}
:::tip
This is the EMQX Open source edition installation document. the EMQX Enterprise installation document is coming soon, until then please follow the guidelines on the [EMQX Enterprise download page](https://www.emqx.com/en/try?product=enterprise) to install it.
:::
{%endemqxee%}

:::: tabs type:card
:::tab Intel Chip

1. To download [emqx-5.0.14-macos11-amd64.zip](https://www.emqx.com/downloads/broker/v5.0.14/emqx-5.0.14-macos11-amd64.zip), run:

```bash
wget https://www.emqx.com/downloads/broker/v5.0.14/emqx-5.0.14-macos11-amd64.zip
```

2. To unzip the package, run:

```bash
unzip emqx-5.0.14-macos11-amd64.zip
```

3. To start EMQX, run:

```bash
cd emqx && ./bin/emqx start
```

:::

:::tab Apple Silicon

1. To download [emqx-5.0.14-macos12-arm64.zip](https://www.emqx.com/en/downloads/broker/v5.0.14/emqx-5.0.14-macos12-arm64.zip), run:

```bash
https://www.emqx.com/en/downloads/broker/v5.0.14/emqx-5.0.14-macos12-arm64.zip
```

2. To unzip the package, run:

```bash
unzip emqx-5.0.14-macos12-arm64.zip
```

3. To start EMQX, run:

```bash
cd emqx && ./bin/emqx start
```

:::
::::
