# EMQX 5.8 の互換性のない変更点

## e5.8.9

- [#16062](https://github.com/emqx/emqx/pull/16062) RocketMQ アクションが指定されたペイロードテンプレートを無視し、ルールの全出力をレンダリングしてしまう問題を修正しました。

- [#16491](https://github.com/emqx/emqx/pull/16491) macOS 13 (Ventura) 向けのパッケージリリースを停止しました。

## e5.8.6

- [#14802](https://github.com/emqx/emqx/pull/14802) このバージョン以降、REST API またはダッシュボード経由でのプラグインインストールには明示的な許可が必要になります。プラグインをインストールする前に、以下の CLI コマンドで許可を取得してください。

  ```bash
  emqx ctl plugins allow NAME-VSN
  ```

  この変更により、不正なプラグインインストールを防止しセキュリティが強化されます。API やダッシュボード経由でプラグインを管理しているユーザーは、ワークフローの調整が必要です。

## e5.8.5

- [#14703](https://github.com/emqx/emqx/pull/14703) `force_shutdown.max_heap_size` の最大許容値を `128GB` に変更しました。以前に 128GB を超える値を設定していた場合、アップグレード後の設定リロードや更新時に問題が発生する可能性があります。

## e5.8.4

- [#14360](https://github.com/emqx/emqx/pull/14360) Prometheus メトリクスの JSON 形式リクエスト時に、トップレベルの `client` キーが単一の JSON オブジェクトではなく、常に JSON オブジェクトの配列となるよう変更しました。この変更により、監視ツールのデータ処理に影響が出る場合があります。

- [#14370](https://github.com/emqx/emqx/pull/14370) IoTDB データ統合の設定変更：

  - セルフディスクリプティングテンプレートが廃止されました。EMQX は今後、設定されたデータテンプレートのみを使用し、メッセージペイロードからテンプレートを抽出しません。

  - 各 MQTT メッセージは単一の `payload` のみを保持可能となり、ペイロードの配列はサポートされなくなりました。そのため、MQTT メッセージは単一またはバッチ挿入のいずれかの単位で IoTDB に原子的に処理されます。1つの MQTT メッセージから複数の IoTDB 操作を生成することはできません。

  - `data type` はテンプレート値ではなく、プレーンな値として扱われます。

  - REST API ドライバーは IoTDB 1.3.x 以降のみをサポートします。

  - Thrift ドライバーは「バッチ」モードをサポートします。

    **重要**：バッチモードでのタイムスタンプ重複を防ぐため、MQTT メッセージのタイムスタンプ（`${timestamp}`）を使用するか、ペイロードに時間フィールド（例：`${payload.time}`）を含めることを推奨します。

## e5.8.3

- [#14305](https://github.com/emqx/emqx/pull/14305) 認証でのハッシュアルゴリズム `MD4`、`MD5`、`RIPEMD-160` のサポートを廃止しました。これらは [NIST Secure Hash Standard](https://www.nist.gov/publications/secure-hash-standard) に準拠していません。

## e5.8.2

- [#14004](https://github.com/emqx/emqx/pull/14004) クラスターリンクにおいて、`topics` 設定内の重複するトピックフィルターが原因でクロスクラスターのメッセージルーティングが不整合かつ不完全になる問題を修正しました。各トピックフィルターは個別に処理されるため、`topics` 設定内で冗長なトピックフィルター（例：`t/1` と `t/+`）は無効とみなされます。このような設定が検出された場合、リンクは起動に失敗します。

- [#14015](https://github.com/emqx/emqx/pull/14015) 動的トピック（プレースホルダーを含むトピック）を使用する Kafka/Confluent/Azure Event Hub プロデューサーは、ディスクバッファリングをサポートしなくなりました。現在はメモリおよびハイブリッドモードのみサポートしています。

- [#14106](https://github.com/emqx/emqx/pull/14106) 単一の Kafka コンシューマーコネクター内で同じ Kafka トピックを持つソースの重複を禁止するバリデーションを追加しました。トピックを重複させたい場合は、新しいコネクターとソースを作成してください。

## e5.8.1

- [#13792](https://github.com/emqx/emqx/pull/13792) `until` 値なしで作成された禁止アイテムのデフォルト有効期限を無期限（以前は最大1年）に変更しました。

- [#13742](https://github.com/emqx/emqx/pull/13742) クライアントが `$` で始まるトピックの保持メッセージを、トピック `#` または `+` をサブスクライブした際に受信してしまう問題を修正しました。

  この修正は [MQTT-4.7.2-1](https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901246) の要件を満たします。

## e5.8.0

- [#13080](https://github.com/emqx/emqx/pull/13080) `mqtt.retry_interval` のデフォルト値を 30秒から無限大（`infinity`）に変更しました。

  これまでは EMQX がデフォルトで30秒ごとにメッセージ配信を自動リトライしていましたが、新しいデフォルトでは自動リトライを行いません。この変更は MQTT 仕様に準拠しており、セッション内でのメッセージ配信リトライは一般的に推奨されていません。

  リトライ機能を利用しているユーザーのために、特定のリトライ間隔を設定するオプションは引き続き利用可能です。

- [#13190](https://github.com/emqx/emqx/pull/13190) CentOS 7 および Ubuntu 18 向けのリリースサポートを終了しました。これらの OS はサポート終了のため、EMQX のビルド提供を停止します。

- [#13248](https://github.com/emqx/emqx/pull/13248) `builtin` の耐久ストレージバックエンドを廃止し、柔軟性とスケーラビリティを向上させるために以下の新しいバックエンドに置き換えました：

  - **`builtin_local`**：レプリケーションをサポートしない耐久ストレージバックエンドで、単一ノード展開に適しています。オープンソース版およびエンタープライズ版で利用可能ですが、マルチノードクラスターには対応していません。
  - **`builtin_raft`**：Raft コンセンサスアルゴリズムを用いて複数ノード間でデータをレプリケートする耐久ストレージバックエンドです。エンタープライズ版専用で、データ耐久性とフォールトトレランスを強化します。

  また、以下の Prometheus メトリクス名を機能をより正確に反映するように変更しました：

  - `emqx_ds_egress_batches` → `emqx_ds_buffer_batches`
  - `emqx_ds_egress_batches_retry` → `emqx_ds_buffer_batches_retry`
  - `emqx_ds_egress_batches_failed` → `emqx_ds_buffer_batches_failed`
  - `emqx_ds_egress_messages` → `emqx_ds_buffer_messages`
  - `emqx_ds_egress_bytes` → `emqx_ds_buffer_bytes`
  - `emqx_ds_egress_flush_time` → `emqx_ds_buffer_flush_time`

- [#13526](https://github.com/emqx/emqx/pull/13526) オープンソース版から Core-replicant 機能を削除しました。5.8 以降、オープンソース版の全ノードは Core ロールで動作します。この変更はエンタープライズ版には影響しません。不要となった `cluster.core_nodes` 設定パラメータも削除しました。

- [#13372](https://github.com/emqx/emqx/pull/13372) ゲートウェイが受け入れる接続数はライセンス条件に従うようになり、許可された接続数の制限を遵守します。
