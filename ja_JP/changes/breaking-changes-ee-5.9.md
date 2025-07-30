# EMQX 5.9 における互換性のない変更点

## 5.9.1

- [#15156](https://github.com/emqx/emqx/pull/15156) `dashboard.sso.oidc.issuer` フィールドに対して厳格なスキーマ検証を追加しました。このフィールドには有効なURLを指定する必要があります。以前は無効な設定でもAPIがエラーを返さず受け入れてしまい、その結果EMQXが再起動に失敗し、クラッシュ（`erl_crash.dump`）を引き起こす可能性がありました。

## 5.9.0

- [#14865](https://github.com/emqx/emqx/pull/14865) 旧LDAP認証設定レイアウト（v5.4以降非推奨）を廃止しました。  
  `password_attribute` と `is_superuser_attribute` は `method` ブロック内に移動してください：
    ```hcl
    method {
      type = hash
      password_attribute = "userPassword"
      is_superuser_attribute = "isSuperuser"
    }
    ```

- [#14765](https://github.com/emqx/emqx/pull/14765) SQL ServerコネクターにおけるNamed Instances使用時の追加検証を実装しました。  
  以前はユーザーが明示的にポートを指定したかどうかを判別できず、未指定の場合は常にデフォルトポートが追加されていました。

  Named Instancesの場合、ODBCドライバーで接続する際に明示的にポートを指定する必要があります。ドライバーは指定されたインスタンス名を無視し、そのポートで稼働しているインスタンスに接続します。

  今後はインスタンス名が指定された場合、ポートを明示的に定義することを必須とし、ヘルスチェック時に接続先インスタンス名と希望インスタンス名の差異を推測する処理も追加しました。

- [#14773](https://github.com/emqx/emqx/pull/14773) レート制限設定オプションが変更されました。  
  - この変更はv5.1.0より前のバージョンとの互換性がありません。  
  - また、v5.1.0より前のバージョンの構造を用いて手動で変更されたリミッター設定とも互換性がありません。  
  - 非公開エンドポイント `/configs/limiter` は削除されました。

- [#14703](https://github.com/emqx/emqx/pull/14703) `force_shutdown.max_heap_size` の最大許容値を `128GB` に変更しました。

- [#14957](https://github.com/emqx/emqx/pull/14957) プラグインの設定更新方法が変更されました。  
  システムはプラグイン設定を更新する際に `on_config_changed` コールバックの結果を尊重するようになりました。この変更はダッシュボード経由で行われる新しい設定更新にのみ影響し、既にクラスターに保存されている設定に対しては `on_config_changed` の結果は引き続き無視されます。

  さらに、プラグインのインストール時にプラグインアプリがロードされるようになり、停止中のプラグインに対しても `on_config_changed` コールバックが呼び出されることを保証します。
