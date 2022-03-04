# 在运行时安装 EMQX 补丁包

如果一个 Bug 修复只更新了少数几个 module，在已经知道需要更新的 module 列表的情况下，可以使用补丁包方式升级 EMQX。

注意：如果可以使用版本热升级的方式，则首选版本热升级。只有版本热升级不可用，并且你了解在生产环境中
安装补丁包的后果的情况下，才可以使用此方式解决问题。

## 安装 EMQX 补丁包的步骤

1. 从 EMQX 开发者那里获取本次需要更新的 modules 列表。比如：

    ```
    emqx.beam
    emqx_rule_engine.beam
    ```

2. 从 EMQX 官网，或者 EMQX 开发者那里获取本次更新对应的软件包。

访问 [开源版下载地址](https://www.emqx.com/en/try?product=broker) 或者 [企业版下载地址](https://www.emqx.com/en/try?product=enterprise)，下载对应版本的 zip 软件包。

注意选择正确的软件版本号、OTP 版本号、以及操作系统类型，并且选择 **"zip"** 包类型。

3. 解压下载的 zip 包，并找到要更新的 modules：

    ```bash
    $ unzip -q emqx-ee-4.4.1-otp24.1.5-3-ubuntu20.04-amd64.zip
    ```

    假设我们要更新 emqx.beam 和 emqx_rule_engine.beam，则在解压目录下寻找他们：

    ```bash
    $ find ./emqx -name "emqx.beam"
    ./emqx/lib/emqx-4.4.1/ebin/emqx.beam

    $ find ./emqx -name "emqx_rule_engine.beam"
    ./emqx/lib/emqx_rule_engine-4.4.1/ebin/emqx_rule_engine.beam
    ```

4. 确保 EMQX 是已经启动的状态：

    ```bash
    $ emqx_ctl status
    Node 'emqx@127.0.0.1' 4.4.1 is started
    ```

5. 找到 beam 文件的对应位置，备份并替换原 beam 文件:

    找到 EMQX 的安装目录：

    ```bash
    $ emqx root_dir
    "/usr/lib/emqx"
    ```

    在安装目录下的 lib 目录下查找原 beam 文件的路径:

    ```bash
    $ find /usr/lib/emqx/lib -name "emqx.beam"
    /usr/lib/emqx/lib/emqx-4.4.0/ebin/emqx.beam

    $ find /usr/lib/emqx/lib -name "emqx_rule_engine.beam"
    /usr/lib/emqx/lib/emqx_rule_engine-4.4.0/ebin/emqx_rule_engine.beam
    ```

    备份原 beam 文件到 `/tmp` 目录:

    ```bash
    $ cp /usr/lib/emqx/lib/emqx-4.4.0/ebin/emqx.beam \
        /usr/lib/emqx/lib/emqx_rule_engine-4.4.0/ebin/emqx_rule_engine.beam /tmp
    ```

    使用新 beam 文件覆盖到对应位置:

    ```bash
    $ cp -f ./emqx/lib/emqx-4.4.1/ebin/emqx.beam /usr/lib/emqx/lib/emqx-4.4.0/ebin/
    $ cp -f ./emqx/lib/emqx_rule_engine-4.4.1/ebin/emqx_rule_engine.beam /usr/lib/emqx/lib/emqx_rule_engine-4.4.0/ebin/
    ```

6. 加载新的 beam 文件:

    ```bash
    $ emqx eval 'c:lm().'
    [{module, emqx},
     {module, emqx_rule_engine}]
    ```

## 回滚补丁包

1. 把安装补丁包时备份好的文件复制回原来的目录：

    ```bash
    $ cp -f /tmp/emqx.beam /usr/lib/emqx/lib/emqx-4.4.0/ebin/
    $ cp -f /tmp/emqx_rule_engine.beam /usr/lib/emqx/lib/emqx_rule_engine-4.4.0/ebin/
    ```
2. 重新加载 beam 文件:

    ```bash
    $ emqx eval 'c:lm().'
    [{module, emqx},
     {module, emqx_rule_engine}]
    ```
