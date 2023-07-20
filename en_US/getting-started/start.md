# Start EMQX

Start EMQX broker in the background

```bash
$ emqx start
EMQX Broker v4.0.0 is started successfully!
```

systemctl start

```bash
$ sudo systemctl start emqx
EMQX Broker v4.0.0 is started successfully!
```

service start

```bash
$ sudo service emqx start
EMQX Broker v4.0.0 is started successfully!
```

::: tip
If you are using EMQX Enterprise, you need to import a license to use it. For the import steps, see **Start EMQX Enterprise** below.

EMQX Broker installed via ZIP archive does not support systemctl and service startup.

:::


## Check the status of EMQX Broker

EMQX Broker starts normally:

```bash
$ emqx_ctl status
Node 'emqx@127.0.0.1' is started
emqx 4.0.0 is running
```

EMQX Broker failed to start normally:

```bash
$ emqx_ctl status
Node 'emqx@127.0.0.1' not responding to pings。
```

You can check the log file from [`logs`](./directory.md) and confirm whether it belongs to [Common Error](../faq/error.md#).

{% emqxce %}

After 4.2-rc.1, EMQX has added [telemetry](../advanced/telemetry.md), Please learn more before starting.

## Start EMQX Enterprise
EMQX Enterprise needs a license file to start normally. EMQX can skip this step.

{% endemqxce %}



## Request a trial license

EMQX Enterprise requires a license file to start normally. EMQX Broker can skip this step.

1. Visit the [EMQX Enterprise download page](https://www.emqx.com/en/downloads?product=enterprise), click **[Get FREE Trial License](https://www.emqx.com/en/apply-licenses/emqx)**.

    ![](./static/download_enterprise_page.png)

2. Apply for a trial license file, then download the license file.

    ![](./static/apply_license.png)

3. Change the path to the license file in the config: modify `license.file` in the `etc/license.conf` file, and make sure that the license file is indeed in the updated path and EMQX Enterprise has read permission to it. Then, start EMQX Enterprise. EMQX Enterprise is started in the same way as EMQX Broker.

4. Alternatively, if the running EMQX Enterprise cluster needs to update the license file, you can use the `emqx_ctl license reload [path of the license file]` command to directly update the license file without restarting any nodes.  It should be noted that the certificate loaded by the `emqx_ctl license reload` command will be applied to the whole EMQX cluster, and it will be saved in EMQX's data directory under the `licenses` subdirectory (i.e.: `data/licenses/emqx.lic`) in each node.  Even if the broker is restarted, this new license file will be loaded and applied.
