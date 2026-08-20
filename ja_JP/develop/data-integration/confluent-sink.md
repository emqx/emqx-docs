# Confluent への MQTT データストリーム

[Confluent Cloud](https://www.confluent.io/) は Apache Kafka をベースにした、レジリエントでスケーラブルかつフルマネージドのストリーミングデータサービスです。EMQX はルールエンジンとSinkを通じて Confluent とのデータ統合をサポートしており、MQTT データを Confluent に簡単にストリーミングしてリアルタイム処理、保存、分析が可能です。

![EMQX Confluent Integration](./assets/confluent-integration.png)

本ページでは主に Confluent 統合の機能と利点を紹介し、Confluent Cloud の設定および EMQX における Confluent Producer Sink の作成方法を案内します。

## 動作概要

Confluent データ統合は EMQX のすぐに使える機能であり、MQTT ベースの IoT データと Confluent の強力なデータ処理機能を橋渡しします。組み込みの[ルールエンジン](./rules.md)コンポーネントを利用することで、両プラットフォーム間のデータフローと処理を簡素化し、複雑なコーディングを不要にします。

以下の図は、自動車 IoT における EMQX と Confluent データ統合の典型的なアーキテクチャを示しています。

![Confluent Architecture](./assets/confluent-architecture.png)

Confluent へのデータの入出力は、Confluent Sink（Confluent へのメッセージ送信）と Confluent Source（Confluent からのメッセージ受信）で行われます。Confluent Sink を作成した場合、そのワークフローは以下の通りです。

1. **メッセージのパブリッシュと受信**：車両に接続された IoT デバイスは MQTT プロトコルを介して EMQX に正常に接続し、定期的に状態データを含むメッセージをパブリッシュします。EMQX はこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータ処理**：これらの MQTT メッセージは、組み込みのルールエンジンとメッセージサーバーの協調動作により、トピックマッチングルールに従って処理されます。メッセージが到着してルールエンジンを通過すると、あらかじめ定義された処理ルールが評価されます。ペイロード変換を指定したルールがあれば、データ形式変換、特定情報のフィルタリング、追加コンテキストによるペイロードの強化などの変換が適用されます。
3. **Confluent へのブリッジ**：ルールエンジンで定義されたルールがトリガーとなり、メッセージを Confluent に転送するアクションが実行されます。Confluent Sink 機能を使い、MQTT トピックを Confluent の事前定義された Kafka トピックにマッピングし、すべての処理済みメッセージとデータをこれらのトピックに書き込みます。

車両データが Confluent に入力されると、以下のように柔軟にデータを活用できます。

- サービスは Confluent と直接連携し、特定トピックからリアルタイムのデータストリームを消費してカスタマイズされた業務処理を行えます。
- Kafka Streams を利用してストリーム処理を実施し、車両状態をメモリ内で集約・相関させてリアルタイム監視が可能です。
- Confluent の Flowデザイナーコンポーネントを使い、MySQL や ElasticSearch など外部システムへのデータ出力用コネクターを選択して保存できます。

## 機能と利点

Confluent とのデータ統合は、ビジネスに以下の機能と利点をもたらします。

- **大規模メッセージ伝送の信頼性**：EMQX と Confluent Cloud はどちらも高信頼のクラスター機構を採用し、安定かつ信頼性の高いメッセージ伝送チャネルを構築しています。大規模 IoT デバイスからのメッセージの損失ゼロを保証し、ノード追加による水平スケールや動的リソース調整で突発的な大量メッセージにも対応し、メッセージ伝送の可用性を確保します。
- **強力なデータ処理能力**：EMQX のローカルルールエンジンと Confluent Cloud は、デバイスからアプリケーションまでの異なる段階で信頼性の高いストリーミングデータ処理機能を提供します。リアルタイムのデータフィルタリング、形式変換、集約分析などシナリオに応じた処理が可能で、より複雑な IoT メッセージ処理ワークフローを実現し、データ分析アプリケーションのニーズに応えます。
- **強力な統合機能**：Confluent Cloud が提供する多様なコネクターを通じて、EMQX は他のデータベース、データウェアハウス、データストリーム処理システムなどと容易に統合でき、柔軟なデータ分析アプリケーション向けの完全な IoT データワークフローを構築できます。
- **高スループット処理能力**：同期・非同期両方の書き込みモードをサポートし、リアルタイム優先や性能優先などシナリオに応じてデータ書き込み戦略を区別し、レイテンシとスループットのバランスを柔軟に調整可能です。
- **効果的なトピックマッピング**：ブリッジ設定を通じて多数の IoT 業務トピックを Kafka トピックにマッピングできます。EMQX は MQTT ユーザープロパティを Kafka ヘッダーにマッピング可能で、1対1、1対多、多対多など多様なトピックマッピング方式を採用し、MQTT トピックフィルター（ワイルドカード）もサポートしています。

これらの機能は統合能力と柔軟性を高め、効果的かつ堅牢な IoT プラットフォームアーキテクチャの構築を支援します。増大する IoT データは安定したネットワーク接続で伝送され、さらに効果的に保存・管理されます。

## はじめる前に

このセクションでは、EMQX ダッシュボードで Confluent データ統合を設定するための準備作業について説明します。

### 前提条件

- [ルールエンジン](./rules.md)の理解
- [Sink](./data-bridges.md)の理解

### Confluent Cloud の設定

Confluent データ統合を作成する前に、Confluent Cloud コンソールでクラスターを作成し、Confluent Cloud CLI を使ってトピックと API キーを作成する必要があります。

#### クラスターの作成

1. Confluent Cloud コンソールにログインし、クラスターを作成します。例として Standard クラスターを選択し、**Begin configuration** をクリックします。

![EMQX Confluent Create Cluster](./assets/confluent_create_cluster_1.2d537cc0.png)

2. リージョン/ゾーンを選択します。デプロイメントリージョンが Confluent Cloud のリージョンと一致していることを確認し、**Continue** をクリックします。

![EMQX Confluent Select Cluster Region](./assets/confluent_create_cluster_2.a8f517c4.png)

3. クラスター名を入力し、**Launch cluster** をクリックします。

![image-20231013105736218](./assets/confluent_create_cluster_3.d38c10a0.png)

#### Confluent Cloud CLI を使ったトピックと API キーの作成

クラスターが Confluent Cloud で稼働したら、**Cluster Overview** -> **Cluster Settings** ページから **Bootstrap server** の URL を取得できます。

![image-20231013111959327](./assets/confluent_cluster_info.773da650.png)

Confluent Cloud CLI を使ってクラスターを管理できます。以下は基本的な CLI コマンドです。

##### Confluent Cloud CLI のインストール

```bash
curl -sL --http1.1 https://cnfl.io/cli | sh -s -- -b /usr/local/bin
```

すでにインストール済みの場合は、以下のコマンドでアップデート可能です。

```bash
confluent update
```

##### アカウントにログイン

```bash
confluent login --save
```

##### 環境の選択

```bash
# 環境一覧表示
confluent environment list
# 環境選択
confluent environment use <environment_id>
```

##### クラスターの選択

```bash
# Kafka クラスター一覧表示
confluent kafka cluster list
# Kafka クラスター選択
confluent kafka cluster use <kafka_cluster_id>
```

##### API キーとシークレットの使用

既存の API キーを使う場合は、以下のコマンドで CLI に登録します。

```bash
confluent api-key store --resource <kafka_cluster_id>
Key: <API_KEY>
Secret: <API_SECRET>
```

API キーとシークレットを持っていない場合は、以下のコマンドで作成できます。

```bash
$ confluent api-key create --resource <kafka_cluster_id>

API キーの準備に数分かかる場合があります。
API キーとシークレットは保存してください。シークレットは後で取得できません。
+------------+------------------------------------------------------------------+
| API Key    | YZ6R7YO6Q2WK35X7                                                 |
| API Secret | ****************************************                         |
+------------+------------------------------------------------------------------+
```

登録後、以下のコマンドで API キーとシークレットを使用できます。

```bash
confluent api-key use <API_Key> --resource <kafka_cluster_id>
```

##### トピックの作成

`testtopic-in` という名前のトピックを以下のコマンドで作成できます。

```bash
confluent kafka topic create testtopic-in
```

トピック一覧は以下のコマンドで確認可能です。

```bash
confluent kafka topic list
```

##### トピックへのメッセージパブリッシュ

以下のコマンドでプロデューサーを作成できます。起動後、メッセージを入力して Enter を押すと、該当トピックにメッセージがパブリッシュされます。

```bash
confluent kafka topic produce testtopic-in
```

##### トピックからのメッセージ消費

以下のコマンドでコンシューマーを作成できます。該当トピック内のすべてのメッセージが出力されます。

```bash
confluent kafka topic consume -b testtopic-in
```

## コネクターの作成

Confluent Sink アクションを追加する前に、EMQX と Confluent Cloud 間の接続を確立するために Confluent Producer コネクターを作成する必要があります。

1. EMQX ダッシュボードにアクセスし、**Integration** -> **Connectors** をクリックします。
2. ページ右上の **Create** をクリックし、コネクター選択画面で **Confluent Producer** を選択して **Next** をクリックします。
3. `my-confluent` などの名前と説明を入力します。名前は Confluent Sink とコネクターを関連付けるために使用され、クラスター内で一意である必要があります。
4. Confluent Cloud への接続に必要なパラメータを設定します。
   - **Bootstrap Hosts**：Confluent クラスター設定ページの Endpoints 情報に対応します。
   - **Username** と **Password**：先に Confluent Cloud CLI で作成した API キーとシークレットを入力します。
   - **Request Timeout**：EMQX が Confluent からの応答を待つ最大時間（秒）を指定します。デフォルトは `30` 秒です。タイムアウトを超えると EMQX は接続を古いものとみなし再接続します。値が小さすぎると、Confluent がリクエストを受け入れても応答を遅延させる場合があり、再接続後に同じバッチを再送して重複メッセージや過剰な下流データ量を招く可能性があります。
   - その他のオプションはデフォルトのままか、業務要件に応じて設定してください。
5. **Create** ボタンをクリックしてコネクターの作成を完了します。

作成後、コネクターは自動的に Confluent Cloud に接続します。次に、このコネクターを基にルールを作成し、コネクターで設定した Confluent クラスターへデータを転送します。

## Confluent Sink を使ったルールの作成

このセクションでは、MQTT トピック `t/#` からのメッセージを処理し、処理結果を Confluent の `testtopic-in` トピックに送信するルールを EMQX で作成する方法を示します。

1. EMQX ダッシュボードに入り、**Integration** -> **Rules** をクリックします。

2. 右上の **Create** をクリックします。

3. ルール ID に `my_rule` などを入力します。

4. MQTT メッセージをトピック `t/#` から Confluent に転送したい場合、**SQL Editor** に以下のステートメントを入力します。

   注意：独自の SQL 構文を指定する場合、`SELECT` 部分に Sink が必要とするすべてのフィールドを含めてください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   注意：初心者の場合は、**SQL Example** と **Enable Test** をクリックして SQL ルールの学習とテストが可能です。

5. + **Add Action** ボタンをクリックして、ルールによってトリガーされるアクションを定義します。**Type of Action** のドロップダウンリストから `Confluent Producer` を選択し、**Action** ドロップダウンはデフォルトの `Create Action` のままか、既存の Confluent Producer アクションを選択します。この例では新規ルールを作成し、ルールに追加します。

6. Sink の名前と説明を対応するテキストボックスに入力します。

7. **Connector** ドロップダウンから先ほど作成した `my-confluent` コネクターを選択します。隣のボタンをクリックするとポップアップで新規コネクターを迅速に作成でき、必要な設定パラメータは[コネクターの作成](#コネクターの作成)を参照してください。

8. Sink のデータ送信方法を設定します。

   - **Kafka Topic**：`testtopic-in` と入力します。EMQX v5.7.2 以降、このフィールドは動的トピック設定もサポートしています。詳細は[Kafka 動的トピックの設定](./data-bridge-kafka.md#configure-kafka-dynamic-topics)を参照してください。
   - **Kafka Headers**：Kafka メッセージに関連するメタデータやコンテキスト情報を入力します（任意）。プレースホルダーの値はオブジェクトでなければなりません。ヘッダー値のエンコードタイプは **Kafka Header Value Encod Type** ドロップダウンから選択可能です。**Add** をクリックしてさらにキー・バリューを追加できます。
   - **Message Key**：Kafka メッセージのキーです。プレーンな文字列か `${var}` のようなプレースホルダーを含む文字列を入力します。
   - **Message Value**：Kafka メッセージの値です。プレーンな文字列かプレースホルダーを含む文字列を入力します。
   - **Partition Strategy**：プロデューサーが Kafka のパーティションにメッセージを分配する方法を選択します。
   - **Compression**：Kafka メッセージ内のレコードを圧縮／解凍するための圧縮アルゴリズムを指定します。

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のために、1つ以上のフォールバックアクションを定義できます。これらはプライマリ Sink がメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定（任意）**：[詳細設定](#advanced-configuration)を参照してください。

11. **Create** ボタンをクリックして Sink の作成を完了します。作成後、ページは **Create Rule** に戻り、新しい Sink がルールアクションに追加されます。

12. **Create** ボタンをクリックしてルール全体の作成を完了します。

これでルールが正常に作成され、**Integration** -> **Rules** ページで新規ルールが確認でき、**Actions(Sink)** タブで新規の Confluent Producer Sink も確認できます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーを確認できます。トポロジーを通じて、トピック `t/#` のメッセージがルール `my_rule` によって解析され、Confluent に送信・保存されている様子を直感的に把握できます。

## Confluent Producer ルールのテスト

Confluent Producer ルールが期待通りに動作するかテストするため、[MQTTX](https://mqttx.app/en) を使ってクライアントが EMQX に MQTT メッセージをパブリッシュする動作をシミュレートできます。

1. MQTTX を使い、トピック `t/1` にメッセージを送信します。

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Confluent" }'
   ```

2. **Actions(Sink)** ページで Sink 名をクリックし統計情報を表示します。Sink の稼働状況を確認し、新規の受信メッセージ数と送信メッセージ数がそれぞれ 1 件あることを確認します。

3. 以下の Confluent コマンドでメッセージが `testtopic-in` トピックに書き込まれているか確認します。

   ```bash
   confluent kafka topic consume -b testtopic-in
   ```

## 詳細設定

このセクションでは、コネクターや Sink/Source のパフォーマンス最適化やシナリオに応じたカスタマイズ操作が可能な詳細設定オプションを説明します。対応するオブジェクト作成時に **Advanced Settings** を展開し、業務要件に応じて以下の設定を行えます。

### コネクター設定

| 項目                             | 説明                                                         | 推奨値             |
| -------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation         | （Producer のみ）有効にすると、クライアントがメタデータ取得リクエストを送信した際に Kafka トピックが存在しなければ自動作成を許可します。 | `Disabled`         |
| Connect Timeout                   | TCP 接続確立の最大待機時間（認証有効時は認証時間も含む）です。 | `5` 秒             |
| Start Timeout                     | コネクターが自動起動したリソースの正常状態到達を待つ最大秒数です。これにより Sink は接続先リソース（例：Confluent クラスター）が完全に稼働しデータ処理可能になるまで操作を進めません。 | `5` 秒             |
| Health Check Interval             | コネクターの稼働状態をチェックする間隔です。                 | `15` 秒            |
| Min Metadata Refresh Interval     | Kafka ブローカーやトピックのメタデータ更新を行う最短間隔です。短すぎると Kafka サーバーの負荷が増加します。 | `3` 秒             |
| Metadata Request Timeout          | Kafka からメタデータ取得要求の最大待機時間です。               | `5` 秒             |
| Socket Send / Receive Buffer Size | ネットワーク伝送性能最適化のためのソケットバッファサイズ管理です。 | `1 ` MB            |
| No Delay                          | システムカーネルが TCP ソケットを即時送信するか遅延送信するかを選択します。トグルオンで即時送信、オフで最小 40 ミリ秒の遅延が発生する場合があります。 | `Enabled`          |
| TCP Keepalive                     | Kafka ブリッジ接続の TCP キープアライブ機能を有効にし、長時間の非通信による接続切断を防止します。値は `Idle, Interval, Probes` のカンマ区切り3数値で指定します。<br />Idle: 接続がアイドル状態になる秒数（Linux デフォルト 7200 秒）<br />Interval: キープアライブプローブ間隔秒数（Linux デフォルト 75 秒）<br />Probes: 応答なしとみなすまでの最大プローブ送信回数（Linux デフォルト 9 回）<br />例：`240,30,5,` は 240 秒アイドル後にプローブ開始、30 秒間隔で最大 5 回送信し応答なければ切断と判断します。 | `none`             |

### Confluent Producer Sink 設定

| 項目                             | 説明                                                         | 推奨値             |
| -------------------------------- | ------------------------------------------------------------ | ------------------ |
| Health Check Interval            | Sink の稼働状態をチェックする間隔です。                       | `15` 秒            |
| Max Batch Bytes                  | Kafka バッチ内で収集するメッセージの最大サイズ（バイト）です。Kafka ブローカーのデフォルトは 1 MB ですが、EMQX は Kafka メッセージのエンコードオーバーヘッドを考慮し、特に小さなメッセージが多い場合に備えて 1 MB より少し小さく設定しています。単一メッセージがこのサイズを超える場合は別バッチで送信されます。 | `896` KB           |
| Required Acks                    | Kafka パーティションリーダーがフォロワーから受け取る必要のあるアックの種類です。<br />`all_isr`: 全てのインシンクレプリカからのアックを要求<br />`leader_only`: パーティションリーダーのみからのアックを要求<br />`none`: Kafka からのアック不要 | `all_isr`          |
| Partition Count Refresh Interval | Kafka プロデューサーがパーティション数の増加を検知する間隔です。パーティション数が増加すると、EMQX は指定された `partition_strategy` に基づき新パーティションにメッセージを配信します。 | `60` 秒            |
| Max Inflight                     | Kafka プロデューサー（パーティションごと）がアックを受け取る前に送信可能な最大バッチ数です。値が大きいほどスループットは向上しますが、1 を超えるとメッセージの順序入れ替わりリスクがあります。未アックメッセージ数を制御し、システム負荷のバランスを取ります。 | `10` 秒            |
| Query Mode (Producer)            | 非同期または同期クエリモードを選択し、要件に応じてメッセージ送信を最適化します。非同期モードでは Kafka 書き込みが MQTT メッセージパブリッシュをブロックしませんが、クライアントが Kafka 到着より先にメッセージを受信する可能性があります。 | `Async`            |
| Synchronous Query Timeout        | 同期クエリモード時の最大待機時間で、メッセージ送信完了を保証し長時間待機を防ぎます。ブリッジクエリモードが `Sync` の場合のみ適用されます。 | `5` 秒             |
| Buffer Mode                      | メッセージを送信前にバッファに保存するかどうかを定義します。メモリバッファリングは送信速度を向上させます。<br />`memory`: メモリにバッファ。EMQX ノード再起動時にメッセージは失われます。<br />`disk`: ディスクにバッファ。EMQX ノード再起動時もメッセージは保持されます。<br />`hybrid`: 初めはメモリにバッファし、一定サイズ（`segment_bytes` 設定参照）を超えると順次ディスクにオフロードします。メモリモード同様、EMQX 再起動時はメッセージが失われます。 | `memory`           |
| Per-partition Buffer Limit       | Kafka パーティションごとの最大バッファサイズ（バイト）です。上限に達すると古いメッセージを破棄してバッファ領域を確保します。メモリ使用量と性能のバランス調整に役立ちます。 | `2` GB             |
| Segment File Bytes               | バッファモードが `disk` または `hybrid` の場合に適用される設定で、メッセージ保存用の分割ファイルサイズを制御し、ディスクストレージの最適化に影響します。 | `100` MB           |
| Memory Overload Protection       | バッファモードが `memory` の場合に適用され、高メモリ圧迫時に古いバッファメッセージを自動破棄し、システム安定性を確保します。<br />**注意**：Linux システムのみ有効です。 | Disabled           |
| Max Batch Age                    | プロデューサーバッファ内でメッセージが送信されずに保持できる最大期間です。期間を超えたバッチは破棄され、破棄されたメッセージは `dropped.expired` メトリクスにカウントされます。バッファオーバーフロー時もメッセージは破棄される可能性があります。デフォルトの `infinity` はメッセージの期限切れを防ぎます。 | `infinity`         |
| Max Retries                      | Confluent がリトライ可能なエラー（例：パーティションリーダー変更）を返した場合の最大リトライ回数です。初回試行とリトライがすべて失敗するとバッチは破棄され、各メッセージは `failed` メトリクスにカウントされます。明示的なエラー応答のみリトライ回数にカウントされ、接続喪失による再送はカウントされず `max_batch_age` によって制限されます。デフォルトの `infinity` は無制限リトライを許可します。 | `infinity`         |
| Reconnect Delay                  | 接続喪失後にプロデューサーが Confluent へ再接続を試みるまでの遅延時間です。切断中もメッセージはバッファに蓄積され、バッファ制限や `max_batch_age` の影響を受けます。デフォルトは `2` 秒です。 | `2` 秒             |
| Max Linger Time                  | パーティションごとのプロデューサーがより大きなバッチを形成するためにメッセージを蓄積する最大待機時間です。すべてのバッファモードに適用されます。デフォルトの `0` は待機なしでメッセージ遅延を最適化します。小さな遅延を許容すると Confluent へのリクエスト数を削減可能です。ディスクバッファの場合はバッチ書き込み前の待機であり、ディスク IOPS 削減のため最低 `5ms` の設定を推奨します。 | `0` ミリ秒         |
| Max Linger Bytes                 | パーティションごとのプロデューサーが待機を終了しバッチ送信を開始する最大バイト数です。 | `10` MB            |

### <!-- Confluent Consumer Source Configuration -->

## 追加情報

EMQX は Confluent/Kafka とのデータ統合に関する豊富な学習リソースを提供しています。以下のリンクもご参照ください。

**ブログ：**

- [MQTT と Kafka を使ったコネクテッドビークルのストリーミングデータパイプライン構築](https://www.emqx.com/en/blog/building-connected-vehicle-streaming-data-pipelines-with-mqtt-and-kafka)
- [MQTT と Kafka | IoT メッセージングとストリームデータ統合の実践](https://www.emqx.com/en/blog/mqtt-and-kafka)
- [MQTT パフォーマンスベンチマークテスト：EMQX-Kafka 統合](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-kafka-integration)

**ベンチマークレポート：**

- [EMQX Enterprise パフォーマンスベンチマークテスト：Kafka 統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)
