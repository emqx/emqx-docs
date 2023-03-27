# 热配置

使用热配置功能，可以在运行时通过 Dashboard 修改多数 EMQX 的配置项。
通过 Dashboard 修改的配置项会被持久化存储到 `mnesia` 数据库文件里（一般是 `data/mneisa/<节点名>` 目录下）。

::: warning
在 EMQX 中激活热配置后，只能通过 Dashboard 来修改配置项。但是，那些不在 Dashboard 上的配置项，仍然可以通过编辑配置文件并重启 EMQX 来更改。
:::

## 创建模块

打开 [EMQX Dashboard](http://127.0.0.1:18083/#/modules)，点击左侧的 “模块” 选项卡：

![image-20200927213049265](./assets/modules.png)

选择 热配置模块:

![](./assets/hot_confs1.png)


点击选择后，模块添加完成:

![](./assets/hot_confs2.png)

emqx提供了较多的配置在dashboard修改，包括基础配置、zones、监听器、监控告警等

![](./assets/hot_confs3.png)
