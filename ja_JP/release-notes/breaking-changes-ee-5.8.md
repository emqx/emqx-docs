# EMQX 5.8 の互換性のない変更点

## e5.8.6

- [#14802](https://github.com/emqx/emqx/pull/14802) 本バージョン以降、REST API またはダッシュボード経由でのプラグインインストールには明示的な許可が必要となりました。インストール前に以下の CLI コマンドで許可を取得する必要があります。

  ```bash
  emqx ctl plugins allow NAME-VSN
  ```

  この変更により、不正なプラグインインストールを防止しセキュリティが強化されます。API やダッシュボードでプラグインを管理しているユーザーはワークフローの調整が必要です。

## e5.8.5

- [#14703](https://github.com/emqx/emqx/pull/14703) `force_shutdown.max_heap_size` の最大許容値が `128GB` に変更されました。以前に 128GB を超える値に設定していた場合、アップグレード後の設定リロードや更新時に問題が発生する可能性があります。

## e5.8.4

- [#14360](https://github.com/emqx/emqx/pull/14360) Prometheus メトリクスの JSON フォーマット要求時に、`client` トップレベルキーが単一の JSON オブジェクトではなく、常に JSON オブジェクトの配列となるよう変更されました。この変更により監視ツールのデータ処理に影響が出る場合があります。

- [#14370](https://github.com/emqx/emqx/pull/14370) IoTDB データ連携設定の変更点：

  - セルフディスクリプティングテンプレートが廃止されました。EMQX は設定されたデータテンプレートのみを使用し、メッセージペイロードからテンプレートを抽出しなくなりました。

  - 各 MQTT メッセージは単一の `payload` のみを保持可能となり、ペイロードの配列はサポートされなくなりました。そのため、MQTT メッセージは単一またはバッチ挿入のいずれかの単位で IoTDB に原子的に処理されます。1つの MQTT メッセージから複数の IoTDB 操作を生成することはできません。

  - `data type` はテンプレート値ではなくプレーン値として扱われます。

  - REST API ドライバーは IoTDB 1.3.x 以降のみをサポートします。

  - Thrift ドライバーは「バッチ」モードをサポートするようになりました。

    **重要**：バッチモードでタイムスタンプの重複を防ぐため、MQTT メッセージのタイムスタンプ（`${timestamp}`）を使用するか、ペイロード内に時間フィールド（例：`${payload.time}`）を含めることを推奨します。

## e5.8.3

- [#14305](https://github.com/emqx/emqx/pull/14305) 認証でのハッシュアルゴリズム `MD4`、`MD5`、`RIPEMD-160` のサポートを廃止しました。これらは [NIST Secure Hash Standard](https://www.nist.gov/publications/secure-hash-standard) に準拠していません。

## e5.8.2

- [#14004](https://github.com/emqx/emqx/pull/14004) クラスターリンクの `topics` 設定において、重複するトピックフィルターが存在するとクロスクラスターのメッセージルーティングが不整合かつ不完全になる問題を修正しました。各トピックフィルターは個別に処理されるため、`topics` 設定内で冗長なトピックフィルター（例：`t/1` と `t/+`）は無効とみなされます。このような設定が検出された場合、リンクは起動に失敗します。

- [#14015](https://github.com/emqx/emqx/pull/14015) Kafka/Confluent/Azure Event Hub の動的トピック（プレースホルダーを含むトピック）を持つプロデューサーはディスクバッファリングをサポートしなくなりました。現在はメモリモードとハイブリッドモードのみサポートしています。

- [#14106](https://github.com/emqx/emqx/pull/14106) 単一の Kafka コンシューマーコネクター内で同じ Kafka トピックを複数のソースに重複して含めることを禁止するバリデーションを追加しました。トピックを重複させたい場合は、新しいコネクターとソースを作成してください。

## e5.8.1

- [#13792](https://github.com/emqx/emqx/pull/13792) `until` 値なしで作成された禁止アイテムのデフォルト有効期限が無期限（以前は最大1年）に変更されました。

- [#13742](https://github.com/emqx/emqx/pull/13742) クライアントがトピック `#` または `+` をサブスクライブした際に、`$` で始まるトピックの保持メッセージを受信してしまう問題を修正しました。

  この修正は [MQTT-4.7.2-1](https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901246) の要件を満たしています。

## e5.8.0

- [#13080](https://github.com/emqx/emqx/pull/13080) `mqtt.retry_interval` のデフォルト値を 30秒から無限大（`infinity`）に変更しました。

  これまでは EMQX が自動的に30秒ごとにメッセージ配信をリトライしていましたが、新しいデフォルトでは自動リトライを行いません。この変更は MQTT 仕様に準拠したもので、セッション内でのメッセージ配信リトライは一般的に推奨されていません。

  ただし、一部ユーザーがリトライ機能を利用しているため、特定のリトライ間隔を設定する機能は後方互換性のために引き続き利用可能です。

- [#13190](https://github.com/emqx/emqx/pull/13190) CentOS 7 および Ubuntu 18 向けリリースのサポートを終了しました。これらの OS はサポート終了のため、EMQX はビルドを提供しません。

- [#13248](https://github.com/emqx/emqx/pull/13248) `builtin` 耐久ストレージバックエンドを廃止し、柔軟性とスケーラビリティを向上させるために以下の2つの新バックエンドに置き換えました：

  - **`builtin_local`**：レプリケーションをサポートしない耐久ストレージバックエンドで、単一ノード展開に適しています。オープンソース版およびエンタープライズ版で利用可能ですが、マルチノードクラスターには対応していません。
  - **`builtin_raft`**：Raft コンセンサスアルゴリズムを用いて複数ノード間でデータレプリケーションを行う耐久ストレージバックエンドです。エンタープライズ版限定で提供され、高いデータ耐久性とフォールトトレランスを実現します。

  また、以下の Prometheus メトリクス名が機能をより正確に表現するために変更されました：

  - `emqx_ds_egress_batches` → `emqx_ds_buffer_batches`
  - `emqx_ds_egress_batches_retry` → `emqx_ds_buffer_batches_retry`
  - `emqx_ds_egress_batches_failed` → `emqx_ds_buffer_batches_failed`
  - `emqx_ds_egress_messages` → `emqx_ds_buffer_messages`
  - `emqx_ds_egress_bytes` → `emqx_ds_buffer_bytes`
  - `emqx_ds_egress_flush_time` → `emqx_ds_buffer_flush_time`

- [#13526](https://github.com/emqx/emqx/pull/13526) オープンソース版から Core-replicant 機能を削除しました。5.8 以降、オープンソース版の全ノードは Core ロールで動作します。この変更はエンタープライズ版には影響ありません。不要となった `cluster.core_nodes` 設定パラメータも削除されました。

- [#13372](https://github.com/emqx/emqx/pull/13372) ゲートウェイが受け入れる接続数はライセンス条件により制御されるようになり、許可された接続数の制限に準拠します。
