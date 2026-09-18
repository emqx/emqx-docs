# EMQX 5.4 の互換性のない変更点

## 5.4.0

- [#11998](https://github.com/emqx/emqx/pull/11998) MacOS 11（Big Sur）向けパッケージのリリースを停止しました。

- [#12112](https://github.com/emqx/emqx/pull/12112) UDPマルチキャストベースのクラスタリング戦略のサポートを終了しました。

- [#10976](https://github.com/emqx/emqx/pull/10976) 共有サブスクライブにおけるトピックフィルターの重複処理を修正しました。
  * フックコールバック `session.subscribed` および `client.subscribe` は、共有サブスクライブを完全な表現（例：`$share/group1/topic1/#`）で受け取り、`subopts` から `share` プロパティが削除されます。
  * フックコールバック `session.unsubscribed` および `client.unsubscribe` は、共有サブスクライブを完全な表現（例：`$share/group1/topic1/#`）で受け取り、単に `topic1/#` ではなくなります。
  * ExHook Proto が変更されました。メッセージ `SubOpts` の `share` フィールドは非推奨となりました。
    ExHook サーバーは、共有サブスクライブを完全な表現（例：`$share/group1/topic1/#`）で受け取り、メッセージ `SubOpts` から `share` プロパティが削除されます。
  * ルールエンジンのイベント `session.subscribed` および `session.unsubscribed` では、共有サブスクライブの `topic` が完全な表現（例：`$share/group1/topic1/#`）で提供され、単に `topic1/#` ではなくなります。

- [#12129](https://github.com/emqx/emqx/pull/12129) デフォルトライセンスの同時接続数の上限を100から25に調整しました。

- [#12114](https://github.com/emqx/emqx/pull/12114) ExHook Proto が変更されました。メッセージ `TopicFilter` の `qos` フィールドは非推奨となりました。ExHook サーバーはメッセージ `SubOpts` 内で、`qos`、`rh`、`rap`、`nl` の完全なサブスクライブオプションを受け取るようになります。
