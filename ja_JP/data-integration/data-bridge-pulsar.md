# Apache Pulsar への MQTT データストリーム

[Apache Pulsar](https://pulsar.apache.org/) は、アプリケーションやシステム間でリアルタイムデータストリームを効率的に送信するために設計された、人気のあるオープンソースの分散イベントストリーミングプラットフォームです。Apache Pulsar は、より高いスケーラビリティ、より高速なスループット、低いレイテンシを提供します。IoT アプリケーションでは、デバイスから生成されるデータは通常、軽量な MQTT プロトコルを使用して送信されます。Apache Pulsar と EMQX 間のデータ統合により、ユーザーは MQTT データを簡単に Apache Pulsar にストリームし、IoT デバイスから生成されるデータのリアルタイム処理、保存、分析のために他のデータシステムと接続できます。

本ページでは、EMQX と Pulsar 間のデータ統合の詳細な概要と、データ統合の作成および検証に関する実践的な手順を提供します。

## 動作原理

Apache Pulsar とのデータ統合は、EMQX の標準機能であり、EMQX のデバイス接続およびメッセージ送信機能と Pulsar の強力なデータ処理機能を組み合わせています。組み込みのルールエンジンコンポーネントにより、両プラットフォーム間のデータストリーミングと処理が簡素化されています。これにより、複雑なコーディングを必要とせずに MQTT データを Pulsar に簡単に送信し、Pulsar の強力なデータ処理機能を活用できるため、IoT データの管理と活用がより効率的かつ便利になります。

![EMQX データ統合 - Apache Pulsar](./assets/emqx-integration-pulsar.jpg)

EMQX はルールエンジンと設定された Sink を介して MQTT データを Apache Pulsar に転送し、その全体の流れは以下の通りです：

1. **メッセージのパブリッシュと受信**：IoT デバイスは MQTT プロトコルを通じて正常に接続を確立し、その後特定のトピックにテレメトリおよびステータスデータをパブリッシュします。EMQX がこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンを使用して、特定のソースからの MQTT メッセージをトピックマッチングに基づいて処理できます。ルールエンジンは対応するルールをマッチングし、データフォーマット変換、特定情報のフィルタリング、コンテキスト情報の付加などのメッセージ処理を行います。
3. **Apache Pulsar へのデータストリーミング**：ルールがトリガーされると、メッセージを Pulsar に転送するアクションが実行されます。データは Pulsar のメッセージキーおよび値に簡単にマッピング可能です。MQTT トピックは Pulsar トピックにマッピングすることもでき、データの整理や識別が容易になり、後続のデータ処理や分析が促進されます。

MQTT メッセージデータが Apache Pulsar に書き込まれた後は、以下のような柔軟なアプリケーション開発が可能です：

- Pulsar のコンシューマーアプリケーションを書いてこれらのメッセージをサブスクライブし処理します。ビジネスニーズに応じて、MQTT データを他のデータソースと関連付けたり集約したり変換したりして、リアルタイムのデータ同期と統合を実現できます。
- 特定の MQTT メッセージを受信した際に、Pulsar のルールエンジンコンポーネントを使って対応するアクションやイベントをトリガーし、システム間やアプリケーション間のイベント駆動機能を実装できます。
- Pulsar 内で MQTT データストリームをリアルタイムに分析し、異常や特定のイベントパターンを検出してアラート通知を行ったり、条件に応じた対応アクションを実行したりできます。
- 複数の MQTT トピックからのデータを統合し、Pulsar の計算能力を活用してリアルタイム集約や計算、分析を行い、より包括的なデータインサイトを得ることができます。

## 特徴と利点

Pulsar とのデータ統合は、以下の特徴と利点をビジネスにもたらします：

- **信頼性の高い IoT データメッセージ配信**：EMQX は MQTT メッセージをバッチ処理で確実に Pulsar に送信でき、IoT デバイスと Pulsar およびアプリケーションシステムの統合を可能にします。
- **MQTT メッセージ変換**：ルールエンジンを活用して、EMQX は MQTT メッセージのフィルタリングや変換を行えます。メッセージは Pulsar に送信される前にデータ抽出、フィルタリング、付加情報の追加、変換が可能です。
- **柔軟なトピックマッピング**：Pulsar Sink は MQTT トピックを Pulsar トピックに柔軟にマッピングでき、Pulsar メッセージのキー（Key）や値（Value）の設定も簡単に行えます。
- **柔軟なパーティション選択**：Pulsar Sink は MQTT トピックやクライアントに基づき、異なる戦略で Pulsar のパーティションを選択可能で、データの整理や識別に柔軟性を提供します。
- **高スループットシナリオでの処理能力**：Pulsar Sink は同期および非同期の書き込みモードをサポートし、シナリオに応じてレイテンシとスループットのバランスを柔軟に調整できます。

## はじめる前に

このセクションでは、EMQX ダッシュボードで Pulsar データ統合を作成する前に必要な準備について説明します。

### 前提条件

- EMQX データ統合の [ルール](./rules.md) に関する知識
- [データ統合](./data-bridges.md) に関する知識

### Pulsar のインストール

Docker で Pulsar を起動します。

```bash
docker run --rm -it -p 6650:6650 --name pulsar apachepulsar/pulsar:2.11.0 bin/pulsar standalone -nfw -nss
```

詳細な操作手順は、[Pulsar ドキュメントのクイックスタート](https://pulsar.apache.org/docs/2.11.x/getting-started-home/) を参照してください。

### Pulsar トピックの作成

EMQX でデータ統合を作成する前に、関連する Pulsar トピックを作成しておく必要があります。以下のコマンドで、`public` テナントの `default` ネームスペースに、パーティション数 1 の `my-topic` トピックを作成します。

```bash
docker exec -it pulsar bin/pulsar-admin topics create-partitioned-topic persistent://public/default/my-topic -p 1
```

## コネクターの作成

このセクションでは、Sink を Pulsar サーバーに接続するためのコネクターの作成方法を説明します。

以下の手順は、EMQX と Pulsar の両方をローカルマシンで実行していることを前提としています。リモートで実行している場合は設定を適宜調整してください。

1. EMQX ダッシュボードに入り、**Integration** -> **Connectors** をクリックします。
2. ページ右上の **Create** をクリックします。
3. **Create Connector** ページで **Pulsar** を選択し、**Next** をクリックします。
4. **Configuration** ステップで以下の情報を設定します：
   - コネクター名を入力します。大文字・小文字の英数字の組み合わせで、例：`my_pulsar`
   - **Bridge Role** はデフォルトで `Producer` が選択されています。
   - Pulsar サーバーへの接続およびメッセージ書き込み情報を設定します：
     - **Servers**：`pulsar://localhost:6650` と入力します。リモート環境の場合は適宜調整してください。
     - **Authentication**：認証方式を選択します。`none`、`Basic auth`、`token` から選べます。`Basic auth` の場合、EMQX は `Username` と `Password` を `:` で結合して認証文字列を作成します。
     - **Enable TLS**：暗号化接続を確立したい場合はトグルスイッチをオンにします。TLS 接続の詳細は [TLS for External Resource Access](../network/overview.md#tls-for-external-resource-access) を参照してください。
5. 詳細設定（任意）：[Advanced Configurations](#advanced-configurations) を参照してください。
6. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが Pulsar サーバーに接続できるかテストできます。
7. ページ下部の **Create** ボタンをクリックしてコネクターの作成を完了します。ポップアップダイアログで **Back to Connector List** または **Create Rule** をクリックして、ルールと Sink の作成を続行できます。詳細は [Create a Rule with Pulsar Sink](#create-a-rule-with-pulsar-sink) を参照してください。

## Pulsar Sink を使ったルールの作成

このセクションでは、Dashboard でソース MQTT トピック `t/#` からのメッセージを処理し、処理済みデータを設定済みの Pulsar トピック `my-topic` に Sink 経由で保存するルールの作成方法を説明します。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルール ID を入力します。例：`my_rule`

4. **SQL Editor** に以下のステートメントを入力します。これはトピック `t/#` の MQTT メッセージを Pulsar に保存する例です。

   注意：独自の SQL 文を指定する場合は、Sink が必要とするすべてのフィールドを `SELECT` 部分に含めていることを確認してください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   注意：初心者の方は **SQL Examples** と **Enable Test** をクリックして SQL ルールの学習とテストが可能です。

5. **+ Add Action** ボタンをクリックして、ルールでトリガーされるアクションを定義します。このアクションにより、EMQX はルールで処理したデータを Pulsar に送信します。

6. **Action Type** ドロップダウンリストから `Pulsar` を選択します。

7. **Action** ドロップダウンはデフォルトの `Create Action` のままにします。既に作成済みの Sink があれば選択も可能です。本例では新しい Sink を作成します。

8. Sink の名前を入力します。大文字・小文字の英数字の組み合わせで指定してください。

9. **Connector** ドロップダウンから先ほど作成した `my_pulsar` を選択します。新しいコネクターを作成する場合は、ドロップダウン横のボタンをクリックしてください。設定パラメータは [Create a Connector](#create-a-connector) を参照してください。

10. Sink の以下のオプションを設定します：

    - **Pulsar Topic Name**：事前に作成した `persistent://public/default/my-topic` を入力します。変数はサポートされていません。
    - **Partition Strategy**：プロデューサーが Pulsar のパーティションにメッセージを振り分ける方法を選択します。`random`、`roundrobin`、`key_dispatch` から選べます。
    - **Compression**：圧縮アルゴリズムの使用有無と種類を指定します。選択肢は `no_compression`、`snappy`、`zlib` です。
    - **Retention Period**：Pulsar トピックにパブリッシュされたメッセージの保持期間を定義します。デフォルトは `infinity` で、メッセージの自動期限切れはありません。秒数で数値を指定すると、その時間を超えたメッセージは自動的に期限切れとなりトピックから削除されます。
    - **Message Key**：Pulsar メッセージのキーを指定します。プレースホルダー `${var}` を含む文字列も可能です。
    - **Message Value**：Pulsar メッセージの値を指定します。こちらもプレースホルダー `${var}` を含む文字列が使用可能です。

11. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は [Fallback Actions](./data-bridges.md#fallback-actions) を参照してください。

12. **詳細設定（任意）**：[Advanced Configurations](#advanced-configurations) を参照してください。

13. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが Pulsar サーバーに接続できるかテスト可能です。

14. **Create** ボタンをクリックして Sink の設定を完了します。新しい Sink が **Action Outputs** に追加されます。

15. **Create Rule** ページに戻り、設定内容を確認してから **Create** ボタンをクリックし、ルールを作成します。

これでルールの作成が完了しました。**Integration** -> **Rules** ページで新規作成したルールを確認できます。**Actions(Sink)** タブをクリックすると新しい Pulsar Sink が表示されます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーが表示され、トピック `t/#` のメッセージが Pulsar に送信・保存されている様子を確認できます。

## ルールのテスト

MQTTX を使ってトピック `t/1` にメッセージを送信します：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Pulsar" }'
```

Sink の稼働状況を確認すると、新しい受信メッセージと送信メッセージがそれぞれ 1 件ずつあるはずです。

以下の Pulsar コマンドで、メッセージがトピック `persistent://public/default/my-topic` に書き込まれているか確認します：

```bash
docker exec -it pulsar bin/pulsar-client consume -n 0 -s mysubscriptionid -p Earliest persistent://public/default/my-topic
```

## 詳細設定

このセクションでは、Pulsar Sink のパフォーマンスを最適化し、特定のシナリオに合わせて動作をカスタマイズするための詳細設定オプションを説明します。Sink 作成時に **Advanced Settings** を展開し、ビジネスニーズに応じて以下の設定を行えます。

| フィールド                         | 説明                                                         | 推奨値             |
| --------------------------------- | ------------------------------------------------------------ | ------------------ |
| Max Inflight                      | プロデューサーが各パーティションに送信できるメッセージバッチの最大数。<br/>この数を増やすとスループットが向上します。 | `10`               |
| Sync Publish Timeout              | 同期パブリッシュ操作で、メッセージが正常に配信されたことを確認するためにパブリッシャーが待機する最大時間（秒）。<br/>配信問題やネットワーク障害時に無限待機を防ぎ、データ信頼性を確保します。 | `3` 秒             |
| Socket Send Buffer Size           | ネットワーク送信性能を最適化するためのソケットバッファサイズ。 | `1` MB             |
| Batch Size                        | 1つの Pulsar メッセージ内にバッチングする最大リクエスト数。 | `100`              |
| Max Batch Bytes                   | Pulsar バッチ内で収集するメッセージの最大バイト数。通常、Pulsar ブローカーのデフォルトは 1 MB ですが、EMQX はメッセージエンコードのオーバーヘッドを考慮し、デフォルト値をやや低めの 900 KB に設定しています。単一メッセージがこの制限を超える場合は別バッチで送信されます。 | `900` KB           |
| Query Mode                        | メッセージ送信の最適化のため、`asynchronous`（非同期）または `synchronous`（同期）モードを選択可能。非同期モードでは Pulsar への書き込みが MQTT メッセージのパブリッシュ処理をブロックしませんが、クライアントが Pulsar 到着前にメッセージを受信する可能性があります。 | `Async`            |
| Buffer Mode                       | メッセージ送信前にバッファリングするかどうかを定義。メモリバッファリングは送信速度を向上させます。<br/>`memory`: メモリにバッファリング。EMQX ノード再起動時にメッセージは失われます。<br/>`disk`: ディスクにバッファリング。EMQX ノード再起動後もメッセージは保持されます。<br/>`hybrid`: 初めはメモリにバッファリングし、一定サイズ（`segment_bytes` 設定参照）に達すると徐々にディスクにオフロード。メモリモード同様、ノード再起動時はメッセージが失われます。 | `memory`           |
| Pulsar Per-partition Buffer Limit | 各 Pulsar パーティションに許容される最大バッファサイズ（バイト）。この制限に達すると、古いメッセージを破棄してバッファスペースを確保します。<br/>メモリ使用量とパフォーマンスのバランスを取るための設定です。 | `2` GB             |
| Segment File Bytes                | バッファモードが `disk` または `hybrid` の場合に適用。メッセージ保存用の分割ファイルサイズを制御し、ディスクストレージの最適化に影響します。 | `100` MB           |
| Memory Overload Protection        | バッファモードが `memory` の場合に適用。EMQX はメモリ使用率が高くなると古いバッファメッセージを自動破棄し、システムの安定性を維持します。<br/>**注意**：Linux システムのみ有効です。 | `disabled`         |
| Start Timeout                     | コネクターが自動起動したリソースの正常状態を待機する最大時間（秒）。リソース（例：Polar のインスタンス）が完全に稼働しデータ処理準備が整うまで操作を進めないようにします。 | `5` 秒             |
| Health Check Interval             | Sink の稼働状態をチェックする間隔（秒）。 | `1` 秒             |
