# macOS

This section will guide you on how to install and start EMQX on macOS with a zip file.

{% emqxce %}

:::: tabs type:card
:::tab Intel_Chip

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

:::tab Apple_Silicon

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



{% endemqxce %}

{% emqxee %}

:::: tabs type:card
:::tab Intel_Chip

1. To download [emqx-ee-5.0.0-macos11-amd64.zip](https://www.emqx.com/downloads/enterprise/5.0.0/emqx-ee-5.0.0-macos11-amd64.zip), run:

```bash
wget https://www.emqx.com/downloads/enterprise/5.0.0/emqx-ee-5.0.0-macos11-amd64.zip
```

2. To unzip the package, run:

```bash
unzip emqx-ee-5.0.0-macos11-amd64.zip
```

3. To start EMQX, run:

```bash
cd emqx && ./bin/emqx start
```

:::

:::tab Apple_Silicon

1. To download [emqx-ee-5.0.0-macos12-arm64.zip](https://www.emqx.com/en/downloads/enterprise/5.0.0/emqx-ee-5.0.0-macos12-arm64.zip), run:

```bash
https://www.emqx.com/en/downloads/enterprise/5.0.0/emqx-ee-5.0.0-macos12-arm64.zip
```

2. To unzip the package, run:

```bash
unzip emqx-ee-5.0.0-macos12-arm64.zip
```

3. To start EMQX, run:

```bash
cd emqx && ./bin/emqx start
```

:::
::::


{% endemqxee %}