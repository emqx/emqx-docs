# プラグインカタログ

このカタログは、組み込みの製品ドキュメント外で管理されているカスタムEMQXプラグインに関するプラグイン固有のドキュメントを集約しています。

EMQXプラグインは、標準の製品機能が要件を完全に満たさない場合や、ドメイン固有の問題を組み込み機能ではなく拡張機能として解決する方が適切な場合に構築されることが一般的です。

一部のプラグインは専門的なままですが、他のプラグインは実際の幅広いユースケースで有用であることが証明されれば、後に標準のEMQX機能として昇格することもあります。

このページに掲載されているプラグインは、[`emqx.git` モノレポ](https://github.com/emqx/emqx/tree/master/plugins)の一部として管理されています。

## メッセージ配信

[Sync Request](./plugin-catalog/6.0/emqx-sync-request.md)

このプラグインは、HTTP呼び出し元がEMQX REST APIを通じてMQTTリクエストをパブリッシュし、最初に一致するMQTTレスポンスを同期的に待機することを可能にします。

## 運用

[Hot Upgrade (Relup)](./plugin-catalog/6.0/emqx-relup.md)

このプラグインは、実行中のEMQXノードに対して`.relup`コード変更指示を適用し、オペレーターがVMを再起動せずにパッチリリースを展開できるようにします。

[Backup Sync](./plugin-catalog/6.0/emqx-backup-sync.md)

このプラグインは、Data Backup APIを使用してプライマリEMQXクラスターからセカンダリクラスターへ選択されたバックアップデータを定期的に同期し、災害復旧のためにセカンダリを最新状態に保ちます。

## 接続管理

[Per-username Session Quota](./plugin-catalog/6.0/emqx-username-quota.md)

このプラグインはクラスター全体でユーザー名ごとのセッションクォータを強制し、ユーザー名が設定された制限に達した場合は認証を`quota_exceeded`で拒否します。
