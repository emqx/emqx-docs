# Install EMQX Enterprise on macOS
This page guides you on installing and starting EMQX on macOS with a zip file.

Supported versions:

- macOS 14
- macOS 13

For installation on other systems or to install other versions, visit the [EMQX Enterprise download site](https://www.emqx.com/en/downloads-and-install/enterprise).

## Install EMQX Enterprise

1. Go to the download site and select the [**macOS** tab](https://www.emqx.com/en/downloads-and-install/enterprise?os=macOS).
2. Select the latest version `@EE_VERSION@`. From the **Package Type** dropdown, select the `zip` package according to the macOS version and CPU architecture as you need.
3. Click the link below for downloading. You can also follow the command instructions on the page.

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

Start your web browser and enter `http://localhost:18083/` (`localhost` can be substituted with your IP address) in the address bar to access the  [EMQX Dashboard](../dashboard/introduction.md), from where you can connect to your clients or check the running status.

The default user name and password are `admin` & `public`. You will be prompted to change the default password once logged in.

To stop EMQX:

* Use `emqx stop` or `bin/emqx stop` if it is started in daemon mode.
* Press Ctrl+C if it is started in foreground mode.
* Press Ctrl+C twice if it is started in interactive mode.
