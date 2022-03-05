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

3. Replace the license file (`etc/emqx.lic`) in the default certificate directory. You can also choose to change the read path of the certificate file, modify `license.file` in the `etc/emqx.conf` file, and make sure that the license file is in the updated read path and EMQX Enterprise has read permission. Then, start EMQX Enterprise. EMQX Enterprise is started in the same way as EMQX Broker, which can be seen below.

4. If the running EMQX Enterprise needs to update the license file, you can use the `emqx_ctl license reload [path of the license file]` command to directly update the license file without restarting EMQX Enterprise. It should be noted that the certificate loaded by the `emqx_ctl license reload` command will only take effect during this run of EMQX Enterprise. If you need to permanently update the license certificate path, you still need to replace the old certificate or modify the configuration file, which can be seen from the previous step.


::: danger
The certificate loaded by the `emqx_ctl license reload` command will only take effect during this runtime of EMQX Enterprise. If you need to permanently update the path of the License certificate, you still need to replace the old certificate or modify the configuration file.
:::
