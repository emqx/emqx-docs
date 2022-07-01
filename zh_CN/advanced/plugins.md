# 插件

  插件就是根据 EMQX 插件规范编写的任意 Erlang 应用。开发者可以通过插件在EMQX内部扩展任意功能。

  开发/运行插件基本流程如下：

  - 通过 EMQX 提供的插件模板生成对应的插件tar.gz包。
  - 通过 Dashboard 安装插件包，
  - 通过 Dashboard 启动/停止/卸载插件。

## 插件开发

  [emqx-plugin-template](https://github.com/emqx/emqx-plugin-template) 是一个基础的EMQX 插件模板，你可以直接下载修改它来生成一个标准的插件。比如：实现一种新的认证/访问控制插件。

### 下载插件模板 emqx-plugin-template

  ```sh
  $ git clone https://github.com/emqx/emqx-plugin-template
  ```

  这是一个标准的 Erlang Application 应用。

  ```sh
  $ ls
  LICENSE                  _build                   get-rebar3
  Makefile                 check-vsn.sh             priv
  README.md                rebar.config             src
  ```

#### 编译/打包模板

  不改任何代码，直接运行 `make rel` 编译/打包，第一次编译过程较长，因为它依赖于emqx主项目（方便插件直接使用主项目的各种函数），需要下载依赖，并编译主项目。

#### 挂载认证/访问控制的钩子函数

  ```sh
  > tree src/
  src/
  ├── emqx_cli_demo.erl
  ├── emqx_plugin_template.app.src
  ├── emqx_plugin_template.erl
  ├── emqx_plugin_template_app.erl
  └── emqx_plugin_template_sup.erl
  ```

  `emqx_plugins_template_app.erl`  是应用启动入口。你可以在这里定义需要启动的监控树，及挂载 EMQX 的钩子函数。此模板插件中注册了所有的钩子函数，**使用前请删除不需要的钩子**。

  我们只需要使用到认证和访问控制的2个钩子，所以可以修改`emqx_plugins_template:load/1`为：

  ```erlang
  load(Env) ->
    emqx_hooks:add('client.authenticate', {?MODULE, on_client_authenticate, [Env]}, ?HP_HIGHEST),
    emqx_hooks:add('client.authorize',    {?MODULE, on_client_authorize, [Env]}, ?HP_HIGHEST),
    ok.
  ```

  即：当客户端认证时执行我们自定义的`on_client_authenticate/3` 函数，当客户端做 ACL 访问控制检查时执行`on_client_authorize/5`函数。由于EMQX内部也会挂载钩子函数，或者开发者可能会开发多个插件挂载了相同的钩子函数，我们必须在挂载钩子时指定它的执行的顺序。`?HP_HIGHEST`即指定当前的钩子函数最高优先级，最先被执行。

#### 实现认证/访问控制的钩子函数

  ```erlang
  %%  只允许clientid为[A-Za-z0-9_]的字符连接。
  on_client_authenticate(_ClientInfo = #{clientid := ClientId}, Result, _Env) ->
    case re:run(ClientId, "^[A-Za-z0-9_]+$", [{capture, none}]) of
      match -> {ok, Result};
      nomatch -> {stop, {error, banned}}
    end.
  %% 只允许订阅/room/{clientid}的主题，但是可以发送消息给任意主题。
  on_client_authorize(_ClientInfo = #{clientid := ClientId}, subscribe, Topic, Result, _Env) ->
    case emqx_topic:match(Topic, <<"/room/", ClientId/binary>>) of
      true -> {ok, Result};
      false -> stop
    end;
  on_client_authorize(_ClientInfo, _Pub, _Topic, Result, _Env) -> {ok, Result}.
  ```

  通过以上2个钩子函数，我们只让clientid格式符合规范的客户端登录。并且只能订阅/room/{clientid}，这样就实现了一个简单的聊天室功能。
  客户端可以发消息给任意客户端，但每个客户端只能订阅与自己相关的主题。

  **TIPS**：一定要先把配置中的`authorization.no_match` 设置为`deny`，它默认是`allow`.

  ```
  authorization {
    no_match: deny
  }
  ```

  PS： 当然同样的订阅规则，更常用的是可以通过 [ACL File 访问控制](../security/authz/file.md) 来实现。

#### 打包插件

  通过rebar.config中修改插件的版本信息：
  ```erlang
  {relx, [ {release, {emqx_plugin_template, "5.0.0-rc.3"}, [emqx_plugin_template, map_sets]}
         , {dev_mode, false}
         , {include_erts, false}
         ]}.
  %% Additional info of the plugin
  {emqx_plugrel,
      [ {authors, ["EMQX Team"]}
      , {builder,
          [ {name, "EMQX Team"}
          , {contact, "emqx-support@emqx.io"}
          , {website, "www.emqx.com"}
          ]}
      , {repo, "https://github.com/emqx/emqx-plugin-template"}
      , {functionality, ["Demo"]}
      , {compatibility,
          [ {emqx, "~> 5.0"}
          ]}
      , {description, "This is a demo plugin"}
      ]
  }.
  ```

  再次运行打包命令：

  ```sh
  make rel
  ...
  ===> Release successfully assembled: _build/default/rel/emqx_plugin_template
  ===> [emqx_plugrel] creating _build/default/emqx_plugrel/emqx_plugin_template-5.0.0-rc.3.tar.gz
  ```

  命令会提示生成`插件名-版本.tar.gz`版本的包。

#### 安装/启动插件

  <img src="./assets/plugins_upload.png" alt="plugins_upload" style="zoom:80%;" />

  

  <img src="./assets/plugins_start.png" alt="plugins_start" style="zoom:80%;" />

  确认插件各项信息无误后，直接点击启动按钮。


  <img src="./assets/plugins_start_ok.png" alt="plugins_start_ok" style="zoom:80%;" />


使用MQTTX客户端验证功能是否正常：当 clientId 为`1$`时拒绝连接。

<img src="./assets/connect_failed.png" alt="connect_failed" style="zoom:80%;" />

接下来，你可以尝试使用多个MQTTX客户端连接后，互相给对方发消息。

#### 卸载插件

当你不需要插件，可以在 Dashboard 上轻松卸载插件。
如果是升级插件，目前还未支持热升级插件，需要在 Dashboard 上重新安装。

<img src="./assets/plugins_uninstall.png" alt="plugins_uninstall" style="zoom:80%;" />

