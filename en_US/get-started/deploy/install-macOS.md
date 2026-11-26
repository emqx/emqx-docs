# Install EMQX on macOS

::: warning Homebrew Core Formula

Homebrew core disables the `emqx` formula (EMQX 5.8.8) on **2026-11-30**. For EMQX 6.3.3, use the [macOS ZIP packages](https://github.com/emqx/emqx/releases/tag/6.3.3#macos) or the [Docker installation guide](https://docs.emqx.com/en/emqx/latest/get-started/deploy/install-docker.html).

The separate [EMQ Homebrew tap](https://github.com/emqx/homebrew-emqx) supports `brew install emqx/emqx/emqx@6`. This installs **EMQX Enterprise 6.2.2**, only on **Apple Silicon (arm64) with macOS 14 or later**. It does not install EMQX 6.3.3.

:::

This page guides you on installing and starting EMQX on macOS with a zip file.

Supported versions on Apple Silicon (arm64):

- macOS 26
- macOS 15

EMQX 6.3.3 provides separate ZIP packages for macOS 15 and macOS 26. The macOS 15 package requires macOS 15.0 or later and cannot run on macOS 14 (Sonoma). For a container deployment, use the [Docker installation guide](./install-docker.md).

For installation on other systems or to install other versions, visit the [EMQX Enterprise download site](https://www.emqx.com/en/downloads-and-install/enterprise).

## Install EMQX Enterprise

1. Go to the download site and select the [**macOS** tab](https://www.emqx.com/en/downloads-and-install/enterprise?os=macOS).
2. Select version `@EE_VERSION@`. From the **Package Type** dropdown, select the macOS 15 arm64 or macOS 26 arm64 ZIP package for your system.
3. Click the link below to download. You can also follow the command instructions on the page.

## Start and Stop EMQX

EMQX can be started in daemon mode, foreground mode, or interactive mode. Note that only one instance of EMQX can be running at any time with the default configuration.

   ```bash
   # start as daemon
   ./bin/emqx start

   # start in foreground
   ./bin/emqx foreground

   # start in interactive mode, with Erlang shell
   ./bin/emqx console
   ```

After a successful start, EMQX will output this message (if it was started in the foreground or interactive mode):

```bash
EMQX Enterprise @EE_VERSION@ is running now!
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

The default user name and password are `admin` & `public`. You will be prompted to change the default password once logged in.

To stop EMQX:

* Use `emqx stop` or `bin/emqx stop` if it is started in daemon mode.
* Press Ctrl+C if it is started in foreground mode.
* Press Ctrl+C twice if it is started in interactive mode.
