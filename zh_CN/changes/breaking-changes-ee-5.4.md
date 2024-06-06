# EMQX 5.4 中的不兼容变更

## e5.4.0

- [#11998](https://github.com/emqx/emqx/pull/11998) 停止发布 MacOS 11（BigSur）版本的软件包。
- [#12129](https://github.com/emqx/emqx/pull/12129) 将默认试用 License 规格从 100 并发连接调整至 25 并发连接。 
- [#12112](https://github.com/emqx/emqx/pull/12112) 停止支持基于 UDP 多播的集群策略。
- [#10976](https://github.com/emqx/emqx/pull/10976) 修复了共享订阅中的主题过滤重叠处理。
  - 钩子回调 `session.subscribed` 和 `client.subscribe` 现在将以其完整表示形式接收共享订阅，例如 `$share/group1/topic1/#`，并从 `subopts` 中删除 `share` 属性。
  - 钩子回调 `session.unsubscribed` 和 `client.unsubscribe` 现在将以其完整表示形式接收共享订阅，例如 `$share/group1/topic1/#`，而不仅仅是 `topic1/#`。
  - ExHook Proto 发生了变化。消息 `SubOpts` 中的 `share` 字段已被弃用。 ExHook 服务器现在将以其完整表示形式接收共享订阅，例如 `$share/group1/topic1/#`，并从消息 `SubOpts` 中删除 `share` 属性。
  - `session.subscribed` 和 `session.unsubscribed` 规则引擎事件的主题将以其完整表示形式包括共享订阅，例如 `$share/group1/topic1/#`，而不仅仅是 `topic1/#`。
