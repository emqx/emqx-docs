# EMQX 5.9 における互換性のない変更点

## 5.9.1

- [#15156](https://github.com/emqx/emqx/pull/15156) `dashboard.sso.oidc.issuer` フィールドに対して厳密なスキーマ検証を追加しました。このフィールドには有効なURLを指定する必要があります。以前は無効な設定でもAPIがエラーを返さず受け入れてしまい、EMQXの再起動に失敗し、クラッシュ（`erl_crash.dump`）を引き起こす可能性がありました。

## 5.9.0

- [#14865](https://github.com/emqx/emqx/pull/14865) 古いLDAP認証設定レイアウト（v5.4以降非推奨）を廃止しました。  
  `password_attribute` と `is_superuser_attribute` は `method` ブロック内に移動してください：
    ```hcl
    method {
      type = hash
      password_attribute = "userPassword"
      is_superuser_attribute = "isSuperuser"
    }
    ```

- [#14765](https://github.com/emqx/emqx/pull/14765) SQL ServerコネクターにおけるNamed Instances使用時の追加検証を実装しました。  
  以前はユーザーが明示的にポートを指定したかどうかを判別できず、明示されていない場合は常にデフォルトポートが追加されていました。

  Named Instancesの場合、ODBCドライバーで接続する際に明示的にポートを指定する必要があります。ドライバーは指定されたインスタンス名を無視し、そのポートで動作しているインスタンスに接続します。

  今後はインスタンス名が指定された場合、ポートの明示的な指定を必須とし、ヘルスチェック時に接続先と希望インスタンス名の不一致を検出するようにしています。

- [#14773](https://github.com/emqx/emqx/pull/14773) レート制限設定オプションが変更されました。  
  - この変更は5.1.0以前のバージョンとは互換性がありません。  
  - 5.1.0以前の構造を用いて手動で変更されたリミッター設定とも互換性がありません。  
  - 非公開エンドポイント `/configs/limiter` は削除されました。

- [#14703](https://github.com/emqx/emqx/pull/14703) `force_shutdown.max_heap_size` の最大許容値を `128GB` に変更しました。

- [#14957](https://github.com/emqx/emqx/pull/14957) プラグイン設定の更新方法が変更されました。  
  システムはプラグイン設定更新時に `on_config_changed` コールバックの結果を尊重するようになりました。この変更はダッシュボード経由で行われる新しい設定更新にのみ影響し、既にクラスターに保存されている設定に対しては引き続きコールバック結果を無視します。

  さらに、プラグインのインストール時にプラグインアプリをロードするようになり、停止中のプラグインに対しても `on_config_changed` コールバックが呼ばれることを保証します。
