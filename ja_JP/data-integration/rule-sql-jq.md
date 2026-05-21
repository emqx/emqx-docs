# jq 関数

[jq](https://stedolan.github.io/jq/) は、主に [JSON](https://www.json.org/json-en.html) でエンコードされたデータの変換やクエリに特化した強力なコマンドラインツールおよびプログラミング言語です。

EMQX のルールでは、SQL ライクなルールを定義してメッセージを処理・ルーティングできます。これらのルールには、ブローカーを通過する JSON ペイロードに対して複雑な変換を行うために jq 関数を含めることが可能です。

<<<<<<< HEAD
jq 関数を初めて使う場合は、[リファレンス](#references) セクションを参照するとスムーズに始められます。
=======
jq 関数を初めて使う場合は、[リファレンス](#references) セクションを参照して素早く学習できます。
>>>>>>> origin/release-5.10

::: tip

jq 関数は、ルールの SQL 言語だけでは難しい変換を行う際に便利です。

<<<<<<< HEAD
ただし、効率的なメッセージ処理を維持するために、ルール内での長時間実行される計算は避けることを推奨します。また、バグのある jq プログラムを防ぐために、タイムアウト機能（設定項目 `rule_engine.jq_function_default_timeout`）の利用を推奨します。 <!--tech review-->
=======
ただし、効率的なメッセージ処理を維持するために、ルール内での長時間実行される計算は避けることを推奨します。また、バグのある jq プログラムを防ぐために、`rule_engine.jq_function_default_timeout` 設定項目によるタイムアウト機能を利用してください。<!--tech review-->
>>>>>>> origin/release-5.10

:::

## jq 関数

<<<<<<< HEAD
ルールエンジンの SQL における `jq` ステートメントの基本形式は以下の通りです。
=======
ルールエンジンの SQL 内での `jq` 文の基本フォーマットは以下の通りです。
>>>>>>> origin/release-5.10

```
jq('<JQ_program>', '<JSON_input>', <timeout_value>)
```

ここで、

1. `<JQ_program>`：有効な jq プログラムを含む文字列。
2. `<JSON_input>`：jq プログラムの入力となる JSON エンコードされた文字列またはオブジェクト。
<<<<<<< HEAD
3. `<timeout_value>`：省略可能な整数のタイムアウト値（ミリ秒単位）、デフォルトは 10 秒。

`jq` 関数は、指定された入力に対して与えられた jq プログラムを実行し、生成されたオブジェクトのリストを返します。タイムアウト前に実行が終了しない場合や jq プログラムが例外を発生させた場合、関数はエラーをスローします。
=======
3. `<timeout_value>`：省略可能な整数のタイムアウト値（ミリ秒単位）、デフォルトは10秒。

`jq` 関数は、指定された入力に対して与えられた jq プログラムが生成するオブジェクトのリストを返します。実行がタイムアウト前に終了しない場合や、jq プログラムが例外を検出した場合、関数はエラーをスローします。
>>>>>>> origin/release-5.10

## ユースケース

<<<<<<< HEAD
以下は、簡単な `jq` 関数呼び出しの例とその結果です。
=======
以下は簡単な `jq` 関数呼び出し例とその結果です。
>>>>>>> origin/release-5.10

### JSON データ操作

<<<<<<< HEAD
この例では、JSON データのアクセス、変換、値の計算など、`jq` を使ったさまざまな操作方法を示しています。
=======
この例では、JSON データの操作方法として、JSON オブジェクト内の値のアクセス、変換、計算を含むさまざまな方法を示しています。
>>>>>>> origin/release-5.10

コード例：

```SQL
jq('.', '{"temperature": 10}') =
[json_decode('{"temperature": 10}')]

jq('.', json_decode('{"temperature": 10}')) =
[json_decode('{"temperature": 10}')]

jq('.temperature', '{"temperature": 10}') =
[10]

jq('{temperature_C:.temperature,
     temperature_F: (.temperature * 1.8 + 32)}',
   '{"temperature": 10}') =
[json_decode('{"temperature_C": 10, "temperature_F": 50}')]

jq('.temperature,(.temperature * 1.8 + 32)', '{"temperature": 10}') =
[10, 50]
```

### 外れ値を除いた平均値の計算

<<<<<<< HEAD
以下の JSON オブジェクトは、日付と複数のセンサーを含み、それぞれのセンサーは名前とデータポイントの配列を持ち、特定の日付のセンサー読み取り値を表しています。
=======
以下の JSON オブジェクトは、日付と複数のセンサーを含み、それぞれのセンサーは名前と特定の日付のセンサー読み取り値の配列を持っています。
>>>>>>> origin/release-5.10

```json
{
  "date": "2020-04-24",
  "sensors": [
    {
      "name": "a",
      "data": [3, 1, 2, 4, 5, 5]
    },
    {
      "name": "b",
      "data": [1, -100, 2, 3, 4, 5, 2000]
    },
    {
      "name": "c",
      "data": [3, 7, 9]
    }
  ]
}
```

<<<<<<< HEAD
`jq` 関数と `FOREACH` ステートメントを組み合わせることで、jq の出力オブジェクトを複数のメッセージに分割できます。各メッセージは日付フィールドと、外れ値を除いたセンサーのデータフィールドの平均値を含みます。
=======
`jq` 関数と `FOREACH` 文を組み合わせることで、jq の出力オブジェクトを複数のメッセージに分割し、それぞれに日付フィールドと外れ値を除いたセンサーのデータフィールドの平均値を含めることができます。
>>>>>>> origin/release-5.10

```sql
FOREACH   jq('def rem_first:
                 if length > 2 then del(.[0]) else . end;
              def rem_last:
                 if length > 1 then del(.[-1]) else . end;
              .date as $date |
              .sensors[] |
                (.data | sort | rem_first | rem_last | add / length) as $average |
                {$average, $date}',
             payload)
FROM    "jq_demo/complex_rule/jq/#"
```

<<<<<<< HEAD
このとき、3つの出力メッセージのペイロードは以下の通りになります。
=======
この場合、3つの出力メッセージのペイロードは以下のようになります。
>>>>>>> origin/release-5.10

メッセージ 1:

```json
{
  "average": 3.5,
  "date": "2020-04-24"
}
```

メッセージ 2:

```json
{
  "average": 3,
  "date": "2020-04-24"
}
```

メッセージ 3:

```json
{
  "average": 7,
  "date": "2020-04-24"
}
```

### 1つのメッセージを複数のメッセージに分割する

<<<<<<< HEAD
この例では、複数のセンサー測定値を含む入力メッセージを処理し、センサータイプごとに別々のメッセージに分割しています。動作の概要は以下の通りです。

- `FOREACH` は jq 関数を使って、入力メッセージを `sensor_type` と `value` フィールドを持つオブジェクトの配列に変換します。
- `DO` 節は出力メッセージに必要なフィールドを選択します。
- `FROM` 節は、トピックフィルター `car/measurements` に一致するメッセージにルールを適用します。
=======
この例では、複数のセンサー測定値を含む入力メッセージを処理し、センサータイプごとに別々のメッセージに分割します。動作の概要は以下の通りです。

- `FOREACH` は jq 関数を使って、入力メッセージを `sensor_type` と `value` フィールドを持つオブジェクトの配列に変換します。
- `DO` 節は出力メッセージに必要なフィールドを選択します。
- `FROM` 節は、トピックフィルター `car/measurements` にマッチするメッセージにルールを適用します。
>>>>>>> origin/release-5.10

```sql
FOREACH
    ## データは配列である必要があります
    jq('
       [{
         sensor_type: "temperature",
         value: .temperature
        },
        {
         sensor_type: "humidity",
         value: .humidity
        },
        {
         sensor_type: "pressure",
         value: .pressure
        },
        {
         sensor_type: "light",
         value: .light
        },
        {
         sensor_type: "battery",
         value: .battery
        },
        {
         sensor_type: "speed",
         value: .speed
        }]',
        payload) as sensor  
DO
    payload.client_id,
    payload.timestamp,
    sensor.sensor_type,
    sensor.value
FROM "car/measurements"
```

### メッセージ分割の別の方法

<<<<<<< HEAD
この例は、複数のセンサー測定値を含む入力メッセージをセンサータイプごとに分割する別の方法を示しています。

`FOREACH` 節内の jq 関数は、入力とすべてのセンサータイプを保存し、各センサータイプごとに関連フィールドを含むオブジェクトを出力します。
=======
この例は、複数のセンサー測定値を含む入力メッセージをセンサータイプごとに別々のメッセージに分割する別の方法を示しています。

`FOREACH` 節内の jq 関数は入力と全センサータイプを保存し、各センサータイプごとに関連フィールドを持つオブジェクトを出力します。
>>>>>>> origin/release-5.10

```sql
FOREACH
    jq('
       # 入力を保存
       . as $payload |
       
<<<<<<< HEAD
       # すべてのセンサータイプ
=======
       # 全センサータイプ
>>>>>>> origin/release-5.10
       [ 
         "temperature",
         "humidity",
         "pressure",
         "light",
         "battery",
         "speed" 
       ] as $sensor_types |
       
       # 各センサータイプごとにオブジェクトを出力
       $sensor_types[] |
       {
         client_id: $payload.client_id,
         timestamp: $payload.timestamp,
         sensor_type: .,
         value: $payload[.] 
       }
       ',
       payload) as sensor  
FROM "car/measurements"
```

## リファレンス

<<<<<<< HEAD
jq 関数を初めて使う場合は、以下の資料を参照すると良いでしょう。
=======
jq 関数を初めて使う場合は、以下の資料を参照することをおすすめします。
>>>>>>> origin/release-5.10

-  [jq ドキュメント](https://stedolan.github.io/jq/manual/)
- [オンライン jq プログラミングプレイグラウンド](https://jqplay.org/)
- [EMQX の jq 入門動画](https://www.youtube.com/watch?v=_GwF8zvhNcQ)
