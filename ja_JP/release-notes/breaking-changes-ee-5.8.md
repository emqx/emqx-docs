# EMQX 5.8 における非互換変更

## e5.8.9

- [#16062](https://github.com/emqx/emqx/pull/16062) RocketMQ アクションが指定されたペイロードテンプレートを無視し、ルール出力全体をレンダリングしてしまう問題を修正しました。

- [#16491](https://github.com/emqx/emqx/pull/16491) macOS 13 (Ventura) 向けのパッケージリリースを停止しました。

## e5.8.6

- [#14802](https://github.com/emqx/emqx/pull/14802) 本バージョンより、REST API またはダッシュボード経由でのプラグインインストールには明示的な権限が必要になります。インストール前に以下の CLI コマンドで権限を取得してください。

  ```bash
  emqx ctl plugins allow NAME-VSN
  ```

  この変更により、無許可のプラグインインストールを防止しセキュリティを強化します。API やダッシュボードでプラグインを管理しているユーザーは、ワークフローの調整が必要です。

## e5.8.5

- [#14703](https://github.com/emqx/emqx/pull/14703) `force_shutdown.max_heap_size` の最大許容値を `128GB` に変更しました。以前に 128GB を超える値に設定していた場合、アップグレード後の設定リロードや更新時に問題が発生する可能性があります。

## e5.8.4

- [#14360](https://github.com/emqx/emqx/pull/14360) Prometheus メトリクスを JSON 形式で要求した際、`client` トップレベルキーは単一の JSON オブジェクトではなく、常に JSON オブジェクトの配列となるよう変更されました。この変更により監視ツールのデータ処理に影響が出る可能性があります。

- [#14370](https://github.com/emqx/emqx/pull/14370) IoTDB データ統合の設定変更:

  - セルフディスクリビングテンプレートが廃止されました。EMQX は設定されたデータテンプレートのみを使用し、メッセージペイロードからテンプレートを抽出しなくなりました。
  
  - 各 MQTT メッセージは単一の `payload` のみを保持可能となり、ペイロードの配列はサポートされなくなりました。そのため、MQTT メッセージは IoTDB への単一の原子挿入操作（単一またはバッチ挿入）として処理されます。1つの MQTT メッセージから複数の IoTDB 操作を生成することはできません。
  
  - `data type` はテンプレート値ではなくプレーンな値として扱われます。
  
  - REST API ドライバーは IoTDB 1.3.x 以降のみをサポートします。
  
  - Thrift ドライバーは「バッチ」モードをサポートするようになりました。
  
    **重要**：バッチモードでタイムスタンプの重複を防ぐために、MQTT メッセージのタイムスタンプ（`${timestamp}`）を使用するか、ペイロードに時間フィールド（例：`${payload.time}`）を含めることを推奨します。

## e5.8.3

- [#14305](https://github.com/emqx/emqx/pull/14305) 認証におけるハッシュアルゴリズム `MD4`、`MD5`、`RIPEMD-160` のサポートを廃止しました。これらは [NIST Secure Hash Standard](https://www.nist.gov/publications/secure-hash-standard) に準拠していません。

## e5.8.2

- [#14004](https://github.com/emqx/emqx/pull/14004) クラスターリンクの `topics` 設定において、重複するトピックフィルターがあるとクロスクラスターのメッセージルーティングが不整合かつ不完全になる問題を修正しました。各トピックフィルターは個別に処理されるため、`t/1` と `t/+` のような冗長なトピックフィルターは無効とみなされます。該当設定が検出されるとリンクは起動に失敗します。

- [#14015](https://github.com/emqx/emqx/pull/14015) 動的トピック（プレースホルダーを含むトピック）を持つ Kafka/Confluent/Azure Event Hub のプロデューサーはディスクバッファリングをサポートしなくなりました。現在はメモリモードとハイブリッドモードのみサポートしています。

- [#14106](https://github.com/emqx/emqx/pull/14106) 単一の Kafka コンシューマーコネクター内で同一 Kafka トピックを繰り返し持つソースを禁止するバリデーションを追加しました。トピックを繰り返す必要がある場合は、新しいコネクターとソースを作成してください。

## e5.8.1

- [#13792](https://github.com/emqx/emqx/pull/13792) `until` 値なしで作成された禁止アイテムのデフォルト有効期限を無期限（以前は最大1年）に変更しました。

- [#13742](https://github.com/emqx/emqx/pull/13742) クライアントがトピック `#` または `+` にサブスクライブした際に、`$` で始まるトピックの保持メッセージを受信する問題を修正しました。

  この修正は [MQTT-4.7.2-1](https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901246) の要件を満たします。

## e5.8.0

- [#13080](https://github.com/emqx/emqx/pull/13080) `mqtt.retry_interval` のデフォルト値を 30秒から無限大（`infinity`）に変更しました。

  これまでは EMQX がメッセージ配信を30秒ごとに自動再試行していましたが、新しいデフォルトでは自動再試行を行いません。この変更は MQTT 仕様に準拠し、セッション内でのメッセージ再試行は一般的に推奨されていません。

  再試行機能を利用しているユーザーのために、特定の再試行間隔を設定するオプションは引き続き利用可能です。

- [#13190](https://github.com/emqx/emqx/pull/13190) CentOS 7 および Ubuntu 18 向けリリースのサポートを終了しました。これらの OS はサポート終了のため、EMQX のビルド提供を停止します。

- [#13248](https://github.com/emqx/emqx/pull/13248) `builtin` 耐久ストレージバックエンドを廃止し、柔軟性とスケーラビリティを向上させるために以下の2つの新バックエンドに置き換えました：

  - **`builtin_local`**：レプリケーション非対応の耐久ストレージバックエンドで、シングルノード環境に適しています。オープンソース版およびエンタープライズ版で利用可能ですが、マルチノードクラスターには対応していません。
  - **`builtin_raft`**：Raft コンセンサスアルゴリズムを用いた耐久ストレージバックエンドで、複数ノード間でのデータレプリケーションを実現します。エンタープライズ版専用で、データ耐久性とフォールトトレランスを強化します。

  また、以下の Prometheus メトリクス名が機能をより正確に反映するよう改名されました：

  - `emqx_ds_egress_batches` → `emqx_ds_buffer_batches`
  - `emqx_ds_egress_batches_retry` → `emqx_ds_buffer_batches_retry`
  - `emqx_ds_egress_batches_failed` → `emqx_ds_buffer_batches_failed`
  - `emqx_ds_egress_messages` → `emqx_ds_buffer_messages`
  - `emqx_ds_egress_bytes` → `emqx_ds_buffer_bytes`
  - `emqx_ds_egress_flush_time` → `emqx_ds_buffer_flush_time`

- [#13526](https://github.com/emqx/emqx/pull/13526) オープンソース版から Core-replicant 機能を削除しました。5.8 以降、オープンソース版の全ノードは Core ロールで動作します。この変更はエンタープライズ版には影響ありません。不要となった `cluster.core_nodes` 設定パラメータも削除されました。

- [#13372](https://github.com/emqx/emqx/pull/13372) ゲートウェイが受け入れる接続数はライセンス条件に従うようになり、許可された接続数の制限を遵守します。
