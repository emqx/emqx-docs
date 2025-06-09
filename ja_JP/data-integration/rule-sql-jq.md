# jq 関数

[jq](https://stedolan.github.io/jq/) は、主に [JSON](https://www.json.org/json-en.html) 形式でエンコードされたデータの変換やクエリに特化した強力なコマンドラインツール兼プログラミング言語です。

EMQX のルールでは、SQL ライクなルールを定義してメッセージを処理・ルーティングできます。これらのルールには、ブローカーを通過する JSON ペイロードに対して複雑な変換を行うために jq 関数を組み込むことが可能です。

jq 関数を初めて使う方は、[参考資料](#references) セクションをご覧いただくとスムーズに始められます。

::: tip

jq 関数は、ルールの SQL 言語だけでは難しい変換を行う際に便利です。

ただし、効率的なメッセージ処理を維持するために、ルール内での長時間実行される計算は避けることを推奨します。また、バグのある jq プログラムを防ぐために、タイムアウト機能（設定項目 `rule_engine.jq_function_default_timeout`）を利用してください。 <!--tech review-->

:::

## jq 関数の基本

ルールエンジンの SQL における `jq` 文の基本形式は以下の通りです。

```
jq('<JQ_program>', '<JSON_input>', <timeout_value>)
```

ここで、

1. `<JQ_program>`: 有効な jq プログラムを含む文字列。
2. `<JSON_input>`: jq プログラムの入力となる JSON エンコードされた文字列またはオブジェクト。
3. `<timeout_value>`: オプションの整数型タイムアウト値（ミリ秒単位）。デフォルトは 10 秒。

`jq` 関数は、指定された入力に対して与えられた jq プログラムを実行し、生成されたオブジェクトのリストを返します。実行がタイムアウト前に終了しない場合や jq プログラムが例外を発生させた場合は、エラーをスローします。

## 利用例

以下に、簡単な `jq` 関数呼び出し例とその結果を示します。

### JSON データの操作

この例では、JSON データのアクセス、変換、計算など、`jq` を用いたさまざまな操作方法を示しています。

コード例:

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

以下の JSON オブジェクトは、特定の日付のセンサー読み取り値を表す、名前とデータ配列を持つ複数のセンサー情報を含みます。

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

`jq` 関数と `FOREACH` 文を組み合わせて、外れ値を除いたセンサーのデータ平均値と日付を含む複数のメッセージに分割できます。

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

これにより、3つの出力メッセージは以下のペイロードを持ちます。

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

### 1つのメッセージを複数のメッセージに分割

この例では、複数のセンサー測定値を含む入力メッセージを、センサー種別ごとに分割して別々のメッセージにする方法を示します。

- `FOREACH` は jq 関数を使い、入力メッセージを `sensor_type` と `value` フィールドを持つオブジェクトの配列に変換します。
- `DO` 節で出力メッセージに必要なフィールドを選択します。
- `FROM` 節は、トピックフィルター `car/measurements` にマッチするメッセージにルールを適用します。

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

この例は、複数のセンサー測定値を含む入力メッセージをセンサー種別ごとに分割する別の方法を示します。

`FOREACH` 節内の jq 関数は入力と全センサー種別を保存し、各センサー種別ごとに関連フィールドを持つオブジェクトを出力します。

```sql
FOREACH
    jq('
       # 入力を保存
       . as $payload |
       
       # 全センサー種別
       [ 
         "temperature",
         "humidity",
         "pressure",
         "light",
         "battery",
         "speed" 
       ] as $sensor_types |
       
       # 各センサー種別ごとにオブジェクトを出力
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

## 参考資料

jq 関数を初めて使う方には、以下の資料をおすすめします。

-  [jq ドキュメント](https://stedolan.github.io/jq/manual/)
- [オンライン jq プログラミングプレイグラウンド](https://jqplay.org/)
- [EMQX の jq 入門動画](https://www.youtube.com/watch?v=_GwF8zvhNcQ)
