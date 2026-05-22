# EMQX Tables に MQTT データを取り込む

EMQX Tables は、[EMQX Cloud](https://docs.emqx.com/en/cloud/latest/) に組み込まれたネイティブでフルマネージドの時系列データストレージサービスです。高スループットかつ低レイテンシで MQTT データの取り込みと解析を最適化しており、IoT（Internet of Things）ユースケースに最適です。

GreptimeDB を基盤とする EMQX Tables は EMQX ブローカーとシームレスに統合され、InfluxDB Line Protocol をサポートすることで、テレメトリデータの効率的な保存、クエリ、可視化を可能にします。詳細は [EMQX Tables 概要](https://docs.emqx.com/en/cloud/latest/emqx_tables/emqx_tables_overview.html) をご覧ください。

EMQX Enterprise 6.1 以降では、EMQX Tables コネクターとシンクが提供されており、オンプレミスの EMQX Enterprise 環境から EMQX Cloud 上の EMQX Tables へ MQTT データを安全に書き込み、集中クエリおよび処理が可能です。

![enterprise_tables_integration](./assets/enterprise_tables_integration.png)

本ページでは、EMQX Enterprise から EMQX Cloud の EMQX Tables へ MQTT データを取り込む手順を以下の流れで説明します。

- EMQX Enterprise と EMQX Tables 間のネットワーク接続を確立する
- EMQX Tables コネクターを作成する
- EMQX Tables アクションを含むルールを作成する
- データ取り込みとクエリ結果をテストする

## 前提条件

開始前に以下の要件を満たしていることを確認してください。

- オンプレミスまたはプライベート環境に EMQX Enterprise バージョン 6.1 以降がデプロイされていること。

- [EMQX Cloud コンソール](https://accounts.emqx.com/signin?continue=https://cloud-intl.emqx.com/console/) 上に EMQX Tables のデプロイメントが作成され、稼働していること。

  - EMQX Tables の作成手順は [EMQX Tables デプロイメントの作成](https://docs.emqx.com/en/cloud/latest/emqx_tables/emqx_tables_create_deployment.html) を参照してください。
  - **Deployment Overview** ページで接続情報を取得します。

  ![emqx_tables_connection_info](./assets/emqx_tables_connection_info.png)

- EMQX Enterprise のデプロイメントから EMQX Tables のエンドポイントへネットワーク経由で到達可能であること（パブリックエンドポイントまたはプライベート接続は環境により異なります）。

- 以下に習熟していること：
  - [EMQX ルール](./rules.md)
  - [データ統合](./data-bridges.md)
  - [InfluxDB Line Protocol](https://docs.influxdata.com/influxdb/v2.5/reference/syntax/line-protocol/)

## EMQX Tables コネクターを作成する

データを書き込む前に、EMQX Enterprise 環境で EMQX Tables へのコネクターを作成します。

1. EMQX Enterprise ダッシュボードで、**データ統合** -> **コネクター** に移動します。

2. **+ 新しいコネクター** をクリックし、**EMQX Tables** を選択します。

3. **コネクター作成** ページで以下の設定を行います。

   - **コネクター名**：コネクターの一意の名前を入力します。
   
   - **説明**（任意）：識別用の簡単な説明を追加します。
   
   - **サーバーホスト**：EMQX Tables サービスのアドレスを `<host>:<port>` 形式で入力します。例：`tables.example.emqx.com:4001`
   
   - **データベース**：EMQX Tables の対象データベース名を指定します。例：`public`
   
     ::: tip
     
     EMQX Tables デプロイメント作成時にデフォルトの `public` データベースが作成されます。カスタムデータベースを作成する場合は [カスタムデータベースの作成](https://docs.emqx.com/en/cloud/latest/emqx_tables/emqx_tables_quick_start.html#create-a-custom-database) を参照してください。
     
     :::
   
   - **ユーザー名**：EMQX Tables デプロイメントから提供されたユーザー名を入力します。
   
   - **パスワード**：対応するパスワードを入力します。
   
   - **TLS を有効化**：EMQX Tables への接続時に TLS 暗号化を使用する場合は有効にします。TLS は本番環境で推奨されます。
   
   - **詳細設定**（任意）：接続プールサイズ、タイムアウト、リトライ動作などの詳細オプションを必要に応じて設定します。
   
4. **接続テスト** をクリックして接続確認を行います。EMQX Tables サービスに接続できれば成功メッセージが表示されます。

5. **作成** をクリックしてコネクター作成を完了します。

このコネクターはルールやアクション定義時に利用可能です。

## EMQX Tables へのデータ取り込み用ルールを作成する

次に、どの MQTT メッセージをどのように EMQX Tables に書き込むかを指定するルールを作成します。

### SQL ルールの定義

1. **データ統合** -> **ルール** に移動します。

2. **+ 作成** をクリックします。

3. **SQL エディター**でルールロジックを定義します。例として、クライアントが `temp_hum/emqx` トピックに温度と湿度データをパブリッシュしたときにトリガーされるルールは以下の通りです。

   ```sql
   SELECT
     timestamp,
     payload.location AS location,
     payload.temp AS temp,
     payload.hum AS hum
   FROM "temp_hum/emqx"
   ```

   ::: tip

   EMQX ルールが初めての場合は、**Try It Out** をクリックして SQL ルールを対話的に学習・テストできます。

   :::

4. **+ アクション追加** をクリックしてルールにアクションを追加します。

### EMQX Tables アクションを追加する

SQL ルールを定義した後、ルールがトリガーされた際に選択されたデータを EMQX Tables に書き込むアクションを追加します。

1. **アクションの種類** で **EMQX Tables** を選択します。

2. **アクション** は **アクション作成** のままにします。

3. 以下の項目を設定します。

   - **名前**：アクションの名前を入力します。

   - **コネクター**：先ほど作成した EMQX Tables コネクターを選択します。

   - **説明**（任意）：このアクションの説明を追加します。

   - **書き込み構文**：EMQX Tables への書き込みに使用する InfluxDB Line Protocol の形式を定義します。

     書き込み構文内のプレースホルダー（例：`${location}`, `${temp}`）は SQL ルールで選択したフィールドに対応している必要があります。ルールがトリガーされると、EMQX はこれらのプレースホルダーを SQL クエリで生成された値に置き換えます。

     Line Protocol の先頭の measurement がテーブル名となります。データが初めて正常に書き込まれるとテーブルが自動作成されます。

     例：

     ```pgsql
     temp_hum,location=${location} temp=${temp},hum=${hum} ${timestamp}
     ```

     この例では、

     - `temp_hum` が measurement でテーブル名として使用されます。
     - `location` はタグとして書き込まれます。
     - `temp` と `hum` はフィールドとして書き込まれます。
     - `${timestamp}` はルールエンジンで生成されたタイムスタンプを提供します。

     > 注意：
     >
     > - 符号付き整数値を書き込む場合はプレースホルダーの後に `i` を付けます。例：`${payload.int}i`
     > - 符号なし整数値の場合は `u` を付けます。例：`${payload.int}u`
     > - サフィックスを付けない場合、整数値はデフォルトで符号付き整数として扱われ、小数点を含む値は浮動小数点数として扱われます。
     > - 値が負の可能性があるか符号付き整数として保存する必要がある場合は `i` を使用し、非負の値で符号なし整数として保存したい場合（カウンター、ID、単調増加するメトリクスなど）は `u` を使用してください。

   - **時間精度**：タイムスタンプの時間精度を選択します。デフォルトは `millisecond` です。

   - **フォールバックアクション**（任意）：このアクションが失敗した場合に実行するフォールバックアクションを設定できます。デフォルトではフォールバックアクションは設定されていません。詳細は [フォールバックアクション](./data-bridges.md#fallback-actions) を参照してください。

   - **詳細設定**（任意）：バッチ処理やリトライポリシーなどの詳細動作を必要に応じて設定します。

   ![emqx_tables_action](./assets/emqx_tables_action.png)

4. **作成** をクリックしてアクションを保存します。

5. **ルール作成** ページで **保存** をクリックしてルールを保存します。

## ルールのテストとデータのクエリ

[MQTTX](https://mqttx.app/) などのクライアントツールを使って温度・湿度データの送信をシミュレーションすることを推奨します。簡単なデモとしては、ダッシュボード内の組み込み診断ツールを使うことも可能です。

### Websocket クライアントでテストデータをパブリッシュする

1. EMQX Enterprise ダッシュボードで、左メニューから **診断ツール** -> **Websocket クライアント** をクリックします。

2. ユーザー名／パスワード認証または自動生成認証でシミュレートクライアントとして接続します。

3. **パブリッシュ** セクションで以下の設定でメッセージをパブリッシュします。

   - **トピック**：`temp_hum/emqx`
   
   - **ペイロード**：
   
     ```json
     {
       "temp": 27.5,
       "hum": 41.8,
       "location": "Prague"
     }
     ```
   

![emqx_tables_publish](./assets/emqx_tables_publish.png)

このメッセージによりルールがトリガーされ、EMQX Tables に書き込まれます。

### EMQX Tables でデータをクエリする

1. EMQX Cloud コンソールにログインします。

2. EMQX Tables のデプロイメントに移動します。

3. **データエクスプローラー** をクリックします。

4. 以下の SQL クエリを実行します。

   ```sql
   SELECT * FROM "temp_hum"
   ```

クエリ結果に新しく取り込まれたレコードが表示されるはずです。

![emqx_tables_query](./assets/emqx_tables_query.png)

## ルール統計情報の確認

実行時の動作やパフォーマンスを確認するには：

1. EMQX Enterprise ダッシュボードに戻ります。

2. **データ統合** -> **ルール** に移動します。

3. 作成したルール ID をクリックします。

ルールおよび関連する EMQX Tables アクションの成功数や失敗数などの実行統計を確認できます。

![emqx_tables_statistics](./assets/emqx_tables_statistics.png)
