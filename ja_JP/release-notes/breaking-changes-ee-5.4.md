# EMQX 5.4 における非互換変更

## 5.4.0

- [#11998](https://github.com/emqx/emqx/pull/11998) MacOS 11（Big Sur）向けパッケージのリリースを終了しました。

- [#12112](https://github.com/emqx/emqx/pull/12112) UDPマルチキャストに基づくクラスタリング戦略のサポートを終了しました。

- [#10976](https://github.com/emqx/emqx/pull/10976) 共有サブスクライブにおけるトピックフィルターの重複処理を修正しました。
  * フックコールバック `session.subscribed` および `client.subscribe` は、共有サブスクライブを完全な表現（例：`$share/group1/topic1/#`）で受け取り、`subopts` から `share` プロパティが削除されます。
  * フックコールバック `session.unsubscribed` および `client.unsubscribe` は、共有サブスクライブを完全な表現（例：`$share/group1/topic1/#`）で受け取り、単に `topic1/#` ではなくなります。
  * ExHook Proto が変更されました。メッセージ `SubOpts` の `share` フィールドは非推奨となりました。
    ExHook サーバーは、共有サブスクライブを完全な表現（例：`$share/group1/topic1/#`）で受け取り、メッセージ `SubOpts` から `share` プロパティが削除されます。
  * ルールエンジンのイベント `session.subscribed` および `session.unsubscribed` は、`topic` に共有サブスクライブを完全な表現（例：`$share/group1/topic1/#`）で含み、単に `topic1/#` ではなくなります。

- [#12129](https://github.com/emqx/emqx/pull/12129) デフォルトのライセンス容量を、同時接続100から同時接続25に調整しました。

- [#12114](https://github.com/emqx/emqx/pull/12114) ExHook Proto が変更されました。メッセージ `TopicFilter` の `qos` フィールドは非推奨となりました。ExHook サーバーは、メッセージ `SubOpts` にて完全なサブスクライブオプション（`qos`、`rh`、`rap`、`nl`）を受け取るようになります。
