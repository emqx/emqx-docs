# EMQX 5.10 の互換性のない変更点

## e5.10.0

- [#15289](https://github.com/emqx/emqx/pull/15289) すべてのコネクター、アクション、ソースに新しい `resource_opts.health_check_timeout` 設定を追加しました。デフォルト値は60秒です。ヘルスチェックがこの時間を超えて応答しない場合、コネクター／アクション／ソースは `disconnected` と見なされます。

  注意：デフォルトが60秒のため、以前は60秒以上かかって正常な応答を返していたコネクター／アクション／ソースは、今回の変更によりそのような状況で `disconnected` と見なされるようになります。

- [#15286](https://github.com/emqx/emqx/pull/15286) 設定オプション `broker.routing.storage_schema` は非推奨となり無視されるようになりました。旧バージョンの `v1` ルーティングストレージスキーマはサポートされず、これを使用しているクラスターでは EMQX の起動が拒否されます。

- [#15239](https://github.com/emqx/emqx/pull/15239) `multi_tenancy.default_max_sessions` の型は、これまで `0` も受け入れていましたが、現在は `infinity` または正の整数のみとなりました。

- [#15156](https://github.com/emqx/emqx/pull/15156) `dashboard.sso.oidc.issuer` フィールドにスキーマ検証が追加されました。この値は有効なURLであることがチェックされます。
