# EMQX 5.9 における非互換変更点

## 5.9.1

- [#15156](https://github.com/emqx/emqx/pull/15156) `dashboard.sso.oidc.issuer` フィールドに対して厳密なスキーマバリデーションを追加しました。このフィールドには有効なURLを指定する必要があります。以前は無効な設定でもAPIがエラーを返さず受け入れてしまい、EMQXの再起動に失敗してクラッシュ（`erl_crash.dump`）を引き起こす可能性がありました。

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

- [#14765](https://github.com/emqx/emqx/pull/14765) SQL ServerコネクターにおけるNamed Instances使用時の追加バリデーションを実装しました。  
  以前はユーザーが明示的にポートを指定したかどうかを判別できず、明示的に指定されていない場合は常にデフォルトポートを付加していました。

  Named Instancesの場合、ODBCドライバーで接続する際にポートを明示的に指定する必要があります。ドライバーは指定されたインスタンス名を無視し、そのポートで稼働しているインスタンスに接続します。

  今後はインスタンス名が指定された場合にポートを明示的に定義することを必須とし、ヘルスチェック時に希望するインスタンス名と接続先インスタンス名の違いを推測する処理も追加しました。

- [#14773](https://github.com/emqx/emqx/pull/14773) レート制限設定オプションを変更しました。  
  - この変更は5.1.0より前のバージョンとは非互換です。  
  - 5.1.0より前のバージョンの構造を利用した手動変更済みのリミッター設定とも非互換です。  
  - 非公開エンドポイント `/configs/limiter` は削除されました。

- [#14703](https://github.com/emqx/emqx/pull/14703) `force_shutdown.max_heap_size` の最大許容値を `128GB` に変更しました。

- [#14957](https://github.com/emqx/emqx/pull/14957) プラグイン設定の更新方法を変更しました。  
  システムはプラグイン設定更新時に `on_config_changed` コールバックの結果を尊重するようになりました。この変更はDashboard経由の新規設定更新にのみ影響し、クラスタに既に保存されている設定については引き続きコールバック結果を無視します。

  さらに、プラグインのインストール時にプラグインアプリをロードするようにし、停止中のプラグインに対しても `on_config_changed` コールバックが呼ばれるようにしました。
