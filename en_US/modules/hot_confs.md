# Hot configuration

With the hot configuration feature, most of EMQX's configuration items can be modified at runtime through the Dashboard.
The configuration items modified through the Dashboard will be persistently stored in the `mnesia` database file (typically under the `data/mnesia/<node-name>` directory).

::: warning
Upon activation of hot configuration in EMQX, the only means of modifying the items is through the dashboard.
However, it is still possible to make changes to configuration items that are not present on the dashboard by editing the configuration file and restart the EMQX.
:::

## Create module

Open [EMQX Dashboard](http://127.0.0.1:18083/#/modules) and click on the "Modules" tab on the left:

![image-20200927213049265](./assets/modules.png)

Select the hot configuration module:

![](./assets/hot_confs1.png)


After clicking the selection, the module is added:

![](./assets/hot_confs2.png)

emqx provides more configuration modifications in dashboard, including basic configuration, zones, listeners, monitoring alarms, etc.

![](./assets/hot_confs3.png)