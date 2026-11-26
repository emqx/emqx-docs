# Install EMQX on macOS

::: warning Homebrew Core Formula

Homebrew core disables the `emqx` formula (EMQX 5.8.8) on **2026-11-30**. For EMQX 6.3.3, use the [macOS ZIP packages](https://github.com/emqx/emqx/releases/tag/6.3.3#macos) or the [Docker installation guide](https://docs.emqx.com/en/emqx/latest/get-started/deploy/install-docker.html).

The separate [EMQ Homebrew tap](https://github.com/emqx/homebrew-emqx) supports `brew install emqx/emqx/emqx@6`. This installs **EMQX Enterprise 6.2.2**, only on **Apple Silicon (arm64) with macOS 14 or later**. It does not install EMQX 6.3.3.

:::

The instructions below install EMQX Open Source `@CE_VERSION@` on macOS from a ZIP package.

The supported versions are:

- macOS 14
- macOS 13

For installation on other systems or to install other versions, visit the [EMQX Open Source download site](https://www.emqx.com/en/downloads-and-install/broker).

## Install EMQX from Zip Package

1. Go to the download site and select the [**macOS** tab](https://www.emqx.com/en/downloads-and-install/broker?os=macOS).
2. Select version `@CE_VERSION@`. From the **Package Type** dropdown, select the `zip` package according to the macOS version and CPU architecture as you need.
3. Click the link below for downloading. You can also follow the command instructions on the page.

## Start and Stop EMQX

You can start EMQX in daemon mode, foreground mode, or interactive mode. Note that only one instance of EMQX can be running at any time with default configuration.

Run the following commands from the directory where you extracted the ZIP package.

   ```bash
   # start as daemon
   ./bin/emqx start

   # start in foreground
   ./bin/emqx foreground

   # start in interactive mode, with Erlang shell
   ./bin/emqx console
   ```

After a successful start, EMQX will output this message (if it is started in the foreground or interactive mode):

```bash
EMQX @CE_VERSION@ is running now!
```

You may also see some warning messages which are intended for operators of the production environment and can be ignored if EMQX is used in the local environment for tests, experiments, or client development:

```bash
ERROR: DB Backend is RLOG, but an incompatible OTP version has been detected. Falling back to using Mnesia DB backend.
WARNING: ulimit -n is 256; 1024 is the recommended minimum.
WARNING: Default (insecure) Erlang cookie is in use.
WARNING: Configure node.cookie in /path/to/emqx/etc/emqx.conf or override from environment variable EMQX_NODE__COOKIE
WARNING: NOTE: Use the same cookie for all nodes in the cluster.
```

You can check the status of EMQX with this command:

```bash
./bin/emqx ctl status
```

Start your web browser and enter `http://localhost:18083/` (`localhost` can be substituted with your IP address) in the address bar to access the  [EMQX Dashboard](../../guides/dashboard/introduction.md), from where you can connect to your clients or check the running status.

The default user name and password are `admin` and `public`. You will be prompted to change the default password once logged in.

To stop EMQX:

* Use `./bin/emqx stop` if it is started in daemon mode.
* Press Ctrl+C if it is started in foreground mode.
* Press Ctrl+C twice if it is started in interactive mode.
