# EMQX 5.4 における互換性のない変更点

## e5.4.0

- [#11998](https://github.com/emqx/emqx/pull/11998) MacOS 11（BigSur）向けパッケージのリリースを終了しました。

- [#12112](https://github.com/emqx/emqx/pull/12112) UDPマルチキャストを用いたクラスタリング戦略のサポートを終了しました。

- [#10976](https://github.com/emqx/emqx/pull/10976) 共有サブスクリプションにおけるトピックフィルターの重複処理を修正しました。  
  * フックコールバック `session.subscribed` および `client.subscribe` は、共有サブスクリプションを完全な表現（例：`$share/group1/topic1/#`）で受け取り、`subopts` から `share` プロパティは削除されます。  
  * フックコールバック `session.unsubscribed` および `client.unsubscribe` は、共有サブスクリプションを完全な表現（例：`$share/group1/topic1/#`）で受け取り、単なる `topic1/#` ではなくなります。  
  * ExHook プロトコルが変更されました。メッセージ `SubOpts` 内の `share` フィールドは非推奨となりました。ExHook サーバーは共有サブスクリプションを完全な表現（例：`$share/group1/topic1/#`）で受け取り、`SubOpts` メッセージから `share` プロパティが削除されます。  
  * ルールエンジンのイベント `session.subscribed` および `session.unsubscribed` は、共有サブスクリプションの `topic` を完全な表現（例：`$share/group1/topic1/#`）で受け取るようになり、単なる `topic1/#` ではなくなります。

- [#12129](https://github.com/emqx/emqx/pull/12129) デフォルトのライセンス容量を、同時接続100から同時接続25に調整しました。

- [#12114](https://github.com/emqx/emqx/pull/12114) ExHook プロトコルが変更されました。メッセージ `TopicFilter` 内の `qos` フィールドは非推奨となりました。ExHook サーバーはメッセージ `SubOpts` で、`qos`、`rh`、`rap`、`nl` を含む完全なサブスクリプションオプションを受け取るようになります。
