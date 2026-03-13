# EMQX 5.10 の互換性のない変更点

## 5.10.0

- [#15289](https://github.com/emqx/emqx/pull/15289) すべてのコネクター、アクション、およびソースに新しい `resource_opts.health_check_timeout` 設定を追加しました。デフォルト値は60秒です。ヘルスチェックがこの時間を超えて応答しない場合、コネクター／アクション／ソースは `disconnected` と見なされます。

  注意：デフォルトが60秒のため、以前は60秒以上かかって正常な応答を返していたコネクター／アクション／ソースは、今後はそのような状況で切断されたと見なされます。

- [#15286](https://github.com/emqx/emqx/pull/15286) 設定オプション `broker.routing.storage_schema` は非推奨となり無視されます。レガシーの `v1` ルーティングストレージスキーマはサポートされなくなり、古いバージョンで `v1` を使用しているクラスターでは EMQX の起動が拒否されます。`v1` ルーティングスキーマを使用しているクラスターのアップグレード手順については、[EMQX 5.10以降のローリングアップグレードの考慮事項](../deploy/rolling-upgrades.md#rolling-upgrade-considerations-for-emqx-5.10-or-later) を参照してください。

- [#15239](https://github.com/emqx/emqx/pull/15239) `multi_tenancy.default_max_sessions` の型は、現在 `infinity` または正の整数のみ受け付けます。以前は `0` も許容されていました。

- [#15156](https://github.com/emqx/emqx/pull/15156) `dashboard.sso.oidc.issuer` フィールドにスキーマ検証を追加しました。この値は有効なURLであることがチェックされます。
