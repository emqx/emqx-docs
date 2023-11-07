{% emqxee %}
# Install EMQX Enterprise on macOS
{% endemqxee %}

{% emqxce %}
# Install EMQX on macOS
{% endemqxce %}


{% emqxce %}

This page guides you on installing and starting EMQX on macOS with a zip file.

Supported versions:

- macOS 13 (Homebrew package only)
- macOS 12
- macOS 11

## Install EMQX with Homebrew

[Homebrew](https://brew.sh/) is a free and open-source software package management system that simplifies the installation of software on macOS.

1. If you don't already have Homebrew installed on your Mac, you can install it by running the following command in the Terminal:

    ```bash
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    ```

2. Install EMQX:

   ```bash
   brew install emqx
   ```

## Install EMQX from Zip Package

1. Download [emqx package for your OS and architecture](https://www.emqx.io/downloads?os=macOS). Here is the direct link to the zip package for a Mac computer with macOS 12 (Monterey) and Apple Silicon:

   ```bash
   wget https://www.emqx.com/en/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-macos12-arm64.zip
   ```
   
2. Extract files from the package:

   ```bash
   mkdir -p emqx && unzip emqx-@CE_VERSION@-macos12-arm64.zip -d emqx && cd emqx
   ```

## Start and Stop EMQX

EMQX can be started in daemon mode, foreground mode, or interactive mode. Note that only one instance of EMQX can be running at any time with default configuration.

If you install EMQX with Homebrew, use `emqx` command as specified below. If you install EMQX from a zip package, use `bin/emqx` instead (assuming you are in the directory where you extract emqx files).

   ```bash
   # start as daemon
   emqx start

   # start in foreground
   emqx foreground

   # start in interactive mode, with Erlang shell
   emqx console
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
WARNING: Configure node.cookie in /opt/homebrew/Cellar/emqx/@CE_VERSION@/etc/emqx.conf or override from environment variable EMQX_NODE__COOKIE
WARNING: NOTE: Use the same cookie for all nodes in the cluster.
```

You can check the status of EMQX with this command:

```bash
emqx ctl status
```

Start your web browser and enter `http://localhost:18083/` (`localhost` can be substituted with your IP address) in the address bar to access the  [EMQX Dashboard](../dashboard/introduction.md), from where you can connect to your clients or check the running status.

The default user name and password are `admin` & `public`. You will be prompted to change the default password once logged in.

To stop EMQX:

* Use `emqx stop` or `bin/emqx stop` if it is started in daemon mode.
* Press Ctrl+C if it is started in foreground mode.
* Press Ctrl+C twice if it is started in interactive mode.

{% endemqxce %}

{% emqxee %}

This page guides you on installing and starting EMQX on macOS with a zip file.

Supported versions:

- macOS 12
- macOS 11

The instructions below will take macOS 12 as an example to illustrate how to download the latest version of EMQX. If you want to install a different version or in a different system, please visit the [EMQX Deployment page](https://www.emqx.com/en/try?product=enterprise). 

## Install EMQX

1. Go to [EMQ Official Site](https://www.emqx.com/en/try?product=enterprise&currentVersion=@EE_VERSION@&currentOS=macOS=currentOS=macOS12&utm_source=docs.emqx.com&utm_medium=referral&utm_campaign=enterprise-docs-install-to-try-enterprise).
2. Select `@EE_VERSION@` for **Version** and `macOS` for **OS**, and click the **Download** button.
3. On the Downloads and Install page, select `zip` as the **Install Method** and select the proper **CPU Architecture** that matches your system. Download and install the package according to the instruction.

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

{% endemqxee %}
