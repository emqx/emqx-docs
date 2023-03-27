# Hot configuration

With the hot configuration feature, most of EMQX's configuration items can be modified at runtime through the Dashboard.
The configuration items modified through the Dashboard will be persistently stored in the `mnesia` database file (typically under the `data/mnesia/<node-name>` directory).

::: warning
After the hot configuration in EMQX is activated, for the configuration items included in the EMQX Dashboard, you can only modify the settings with Dashboard. 
For the other configuration items, you can still modify with the configuration file and these changes will take effect after EMQX restarts.
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