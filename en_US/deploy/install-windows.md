# Windows

This page guides you on installing and starting EMQX on Windows with a zip file.

:::tip
It is recommended only to use EMQX for development and testing on Windows. And it is recommended to [run EMQX with Docker](./install-docker.md).
:::

1. Download [emqx-@CE_VERSION@-windows-amd64.zip](https://www.emqx.com/en/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-windows-amd64.zip) to a directory, e.g. `Downloads/emqx-@CE_VERSION@`
2. Enter the download directory, and unzip the .zip package.
3. To start EMQX, run:

```shell
./bin/emqx.cmd start
```
