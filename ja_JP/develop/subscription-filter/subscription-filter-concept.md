# Subscription Filters

EMQX 6.2で導入されたSubscription Filter機能は、MQTT 5.0のパブリッシュ／サブスクライブモデルを拡張し、サブスクリプションレベルでのコンテンツベースフィルタリングを可能にします。これにより、クライアントはトピックフィルターと追加のフィルター式の両方にマッチするメッセージのみを受信でき、不必要なメッセージ配信やネットワークのオーバーヘッドを削減できます。

本ページでは、EMQXにおけるSubscription Filtersの設計動機、主要概念、フィルター式の構文、動作仕様、実際のユースケースまでを包括的に解説します。

## Subscription Filterとは？

Subscription Filterは、MQTTサブスクリプションに付加できるオプションのフィルター条件です。パブリッシュされたメッセージがサブスクリプションのトピックフィルターにマッチした場合、EMQXはメッセージのMQTT 5.0 User Propertiesに対してフィルター式を評価し、両方の条件を満たすメッセージのみをサブスクライバーに転送します。

標準のMQTTルーティングでは、トピックにマッチしたすべてのメッセージがサブスクライバーに転送されます：

```
Publisher --> Topic -- (Filter) --> Subscription --> Subscriber
```

Subscription Filtersはメッセージルーティング経路に第2のフィルタリングレイヤーを導入します：

```
Publisher --> Topic -- (Filter) --> Subscription -- (Filter) --> Subscriber
```

この2段階のフィルタリングにより、トピックベースとコンテンツベースの両方のフィルタリングが可能となり、クライアントは受信したいメッセージをより正確に指定できます。

## なぜSubscription Filtersを使うのか？

標準のMQTT 5.0サブスクリプションはトピックマッチングのみに基づいてメッセージをルーティングします。マッチするトピックにパブリッシュされたすべてのメッセージは、メッセージ内容に関係なくすべてのサブスクライバーに配信されます。これは以下のようなケースで制約となります：

- サブスクライバーが特定の地域、デバイスグループ、カテゴリのメッセージのみを受信したい場合
- 高頻度のトピックに混在するデータを異なるコンシューマーが独立して分割して処理したい場合
- すべてのメッセージをクライアントに配信するとネットワーク使用量や処理負荷が不必要に増大する場合

Subscription Filtersは、サブスクリプション時に正確でコンテンツを意識した配信ルールを宣言可能にし、パブリッシャーやトピック構造の変更、データ軸ごとのトピック分割を不要にします。

## 主要概念

- **トピックフィルター**：サブスクリプションの標準MQTTトピックフィルター部分（`?`より前の部分）。どのメッセージがルーティング段階に入るかを決定します。

- **フィルター式**：コンテンツベースのフィルター条件（`?`以降の部分）。トピックフィルターを通過したメッセージのMQTT 5.0 User Propertiesに対して評価されます。

- **User Properties**：MQTT 5.0メッセージに付加されるキーと値のメタデータ。パブリッシャーが`location`、`device_type`、`region`などの追加情報を含め、サブスクライバーはこれを基にフィルタリングできます。

- **2段階配信**：トピックフィルターの評価に続きフィルター式の評価を行い、両方を満たしたメッセージのみをサブスクライバーに配信する仕組み。

- **フィルターなしサブスクリプション**：`?`区切りがないサブスクリプション。標準のMQTTサブスクリプションとして扱われ、トピックマッチしたすべてのメッセージが配信されます。

## Subscription Filtersの動作原理

Subscription FiltersはMQTT 5.0の**User Properties**をフィルタリング対象とします。クライアントがメッセージをパブリッシュする際、User Propertiesヘッダーにキーと値のペアを含めることができます。EMQXは各フィルター式をこれらのキー・バリューに対して評価し、式にマッチした場合のみメッセージを配信します。

Subscription Filtersはデフォルトで無効化されています。機能の有効化方法は[Subscription Filtersの使い始め](./subscription-filter-get-started.md)をご参照ください。

### フィルター構文

Subscription Filterはトピックフィルターの末尾に`?`を区切り文字として付加します：

```
<topic-filter>?<filter-expression>
```

| コンポーネント | 説明 |
|---|---|
| `<topic-filter>` | 標準のMQTTトピックフィルター（例：`sensor/+/temperature`や`home/#`） |
| `?` | トピックフィルターとフィルター式を区切るデリミタ |
| `<filter-expression>` | メッセージのUser Propertiesに対して評価されるキー・バリューのフィルター条件 |

### フィルター式の形式

フィルター式は等価比較や大小比較演算子をサポートし、複数条件は`&`（論理AND）で結合します：

```
key1=value1&key2>value2
```

| 要素 | 説明 |
|---|---|
| `key` | パブリッシュされたメッセージのUser Propertyキー名 |
| `=` | 等価比較（キーの値が指定された文字列と一致すること） |
| `>` | 数値比較（キーの値が指定数値より大きいこと） |
| `>=` | 数値比較（キーの値が指定数値以上であること） |
| `<` | 数値比較（キーの値が指定数値より小さいこと） |
| `<=` | 数値比較（キーの値が指定数値以下であること） |
| `&` | 複数条件の結合。すべての条件が真である必要がある |

フィルター式は**大文字小文字を区別**します。メッセージのUser Propertiesに指定キーが存在しない場合、メッセージはフィルタリングされ配信されません。

::: tip

Subscription FiltersはMQTT 5.0クライアントのみに適用されます。MQTT 3.1.1クライアントが`?`を含むトピック文字列でサブスクライブした場合、その文字列全体がリテラルトピックフィルターとして扱われます。

:::

## 動作仕様

- EMQXはメッセージをサブスクライバーに配信する際、**トピックフィルターがマッチし**かつ**フィルター式が真と評価された場合のみ**配信します。
- フィルター式がメッセージのUser Propertiesに存在しないキーを参照している場合、そのメッセージは該当サブスクライバーに配信されません。
- 各サブスクリプションのフィルター式は独立して評価されます。あるサブスクライバーにメッセージが配信されるかどうかは、同じトピックの他のサブスクライバーの配信に影響しません。
- `?`区切りのないサブスクリプションは標準のMQTTサブスクリプションと同様に動作します。
- フィルター式の評価はサーバー側で行われ、クライアント側でのフィルタリングロジックは不要です。

## フィルター式の例

以下は一般的なサブスクリプションパターンの例です：

| サブスクリプション文字列 | 意味 |
|---|---|
| `sensor/+/temperature?location=roomA` | User Propertiesに`location=roomA`を含む温度メッセージを受信 |
| `sensor/+/temperature?value>25` | `value` User Propertyが数値で25より大きい温度メッセージを受信 |
| `sensor/+/temperature?location=roomA&unit=celsius` | `location=roomA`かつ`unit=celsius`の両方を満たす温度メッセージを受信 |
| `home/lights/#` | 標準サブスクリプション。マッチするトピックのすべてのメッセージを受信 |

### パブリッシャー側

パブリッシャーは`sensor/1/temperature`に以下のUser Propertiesを付けてメッセージを送信します：

```json
{
  "location": "roomA",
  "unit": "celsius"
}
```

### サブスクライバー側

| サブスクリプション | 配信されるか？ | 理由 |
|---|---|---|
| `sensor/+/temperature?location=roomA` | はい | `location=roomA`が一致 |
| `sensor/+/temperature?location=roomB` | いいえ | `location`の値が一致しない |
| `sensor/+/temperature?location=roomA&unit=celsius` | はい | 両方の条件が一致 |
| `sensor/+/temperature?location=roomA&unit=fahrenheit` | いいえ | `unit`の値が一致しない |
| `sensor/+/temperature` | はい | フィルター式なしの標準サブスクリプション |

## 認可に関する考慮事項

[認可](../../guides/access-control/authz/authz.md)が有効な場合、EMQXはサブスクリプショントピックを設定されたルールに照らして検証します。認可に使用されるトピックは**ベーストピックフィルター**（`?`区切りより前の部分）です。フィルター式は認可評価前に除去されます。

例えば、`sensor/+/temperature?location=roomA`にサブスクライブするクライアントは、`sensor/+/temperature`に対して認可チェックされます。Subscription Filtersで使用するベーストピックパターンに合わせて認可ルールを設定してください。

## 関連機能リファレンス

Subscription FiltersはEMQXの他のメッセージング機能と補完的に利用できます：

- [共有サブスクリプション](../../get-started/messaging/mqtt-shared-subscription.md)：サブスクライバーグループ間でメッセージを分散し負荷分散を実現。コンテンツベースフィルタリングはサポートしません。
- [保持メッセージ](../../get-started/messaging/mqtt-retained-message.md)：トピックごとに最後のメッセージを保存し、新規サブスクライバーに配信。保持メッセージの配信はSubscription Filter式の影響を受けません。
- [トピック書き換え](../../get-started/messaging/mqtt-topic-rewrite.md)：ルーティング前にトピック文字列を変換。トピック書き換えルールはSubscription Filter評価の前に適用されます。
- [ワイルドカードサブスクリプション](../../get-started/messaging/mqtt-wildcard-subscription.md)：`+`や`#`のワイルドカードで複数トピックにマッチ。ワイルドカードトピックフィルターはSubscription Filtersと組み合わせ可能です。
- [メッセージキュー](../message-queue/message-queue-concept.md)：永続化ストレージと設定可能なディスパッチ戦略を備えた耐久性のある非同期メッセージキューを提供します。<!-- 要確認 -->

## 次のステップ

Subscription Filterの概念を理解したら、実際の利用方法を学びましょう：

- [Subscription Filtersの使い始め](./subscription-filter-get-started.md)：機能を有効化し、MQTTX CLIを使ったフィルター動作のエンドツーエンド検証をステップバイステップで解説します。
