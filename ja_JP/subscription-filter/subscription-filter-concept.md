# Subscription Filters

EMQX 6.2で導入されたSubscription Filter機能は、MQTT 5.0のパブリッシュ／サブスクライブモデルにコンテンツベースのフィルタリングをサブスクリプションレベルで拡張します。これにより、クライアントはトピックフィルターと追加のフィルター式の両方に一致するメッセージのサブセットのみを受信でき、不必要なメッセージ配信やネットワークオーバーヘッドを削減できます。

本ページでは、EMQXにおけるSubscription Filtersの設計動機、主要概念、フィルター式の構文、動作仕様、実際のユースケースまでを包括的に解説します。

## Subscription Filterとは？

Subscription Filterは、MQTTサブスクリプションに付加できるオプションのフィルター条件です。パブリッシュされたメッセージがサブスクリプションのトピックフィルターに一致した場合、EMQXはメッセージのMQTT 5.0 User Propertiesに対してフィルター式を評価し、両方の条件を満たすメッセージのみをサブスクライバーに転送します。

標準のMQTTルーティングでは、トピックに一致したすべてのメッセージがサブスクライバーに転送されます。

```
Publisher --> Topic -- (Filter) --> Subscription --> Subscriber
```

Subscription Filtersはメッセージルーティング経路に第2のフィルタリング段階を導入します。

```
Publisher --> Topic -- (Filter) --> Subscription -- (Filter) --> Subscriber
```

この2段階のフィルタリングにより、トピックベースとコンテンツベースの両方のフィルタリングが可能となり、クライアントは受信したいメッセージを正確に指定できます。

## なぜSubscription Filtersを使うのか？

標準のMQTT 5.0サブスクリプションはトピックマッチングのみでメッセージをルーティングします。マッチしたトピックにパブリッシュされたすべてのメッセージは、メッセージ内容に関係なくすべてのサブスクライバーに配信されます。これは以下のようなシナリオで制約となることがあります。

- サブスクライバーが特定の地域、デバイスグループ、カテゴリのメッセージのみを受信したい場合
- 高頻度のトピックに混在するデータを異なるコンシューマーが独立して分割して処理したい場合
- すべてのメッセージをクライアントに配信するとネットワーク使用量や処理負荷が不必要に増大する場合

Subscription Filtersは、サブスクリプション時に正確かつコンテンツを考慮した配信ルールを宣言できることでこれらの課題を解決します。パブリッシャーやトピック構造、データ軸ごとの個別トピックの変更は不要です。

## 主要概念

- **トピックフィルター**：サブスクリプションの標準MQTTトピックフィルター部分（`?`の前の部分）。どのメッセージがルーティング段階に入るかを決定します。

- **フィルター式**：コンテンツベースのフィルター条件（`?`の後の部分）。トピックフィルターを通過した各メッセージのMQTT 5.0 User Propertiesに対して評価されます。

- **User Properties**：MQTT 5.0メッセージに付加されるキー・バリュー形式のメタデータ。パブリッシャーが`location`、`device_type`、`region`などの追加情報を提供し、サブスクライバーはこれを基にフィルタリングできます。

- **2段階配信**：トピックフィルターの評価に続き、フィルター式の評価を行い、両方を満たしたメッセージのみをサブスクライバーに配信する仕組み。

- **非フィルタリングサブスクリプション**：`?`区切りのないサブスクリプション。標準のMQTTサブスクリプションとして扱われ、トピックに一致するすべてのメッセージが配信されます。

## Subscription Filtersの仕組み

Subscription FiltersはMQTT 5.0の**User Properties**をフィルタリング対象とします。クライアントがメッセージをパブリッシュする際、User Propertiesヘッダーにキー・バリューのペアを含めることができます。EMQXは各フィルター式をこれらのキー・バリューに対して評価し、式が一致した場合のみメッセージを配信します。

Subscription Filtersはデフォルトで無効化されています。有効化方法については[Subscription Filtersの使い始め](./subscription-filter-get-started.md)をご参照ください。

### フィルター構文

Subscription Filterはトピックフィルターに`?`区切りで付加します。

```
<topic-filter>?<filter-expression>
```

| コンポーネント | 説明 |
|---|---|
| `<topic-filter>` | 標準のMQTTトピックフィルター（例：`sensor/+/temperature`、`home/#`） |
| `?` | トピックフィルターとフィルター式を区切るデリミタ |
| `<filter-expression>` | メッセージのUser Propertiesに対して評価されるキー・バリューのフィルター条件 |

### フィルター式の形式

フィルター式は等価比較や大小比較演算子をサポートし、複数条件は`&`（論理AND）で結合します。

```
key1=value1&key2>value2
```

| 要素 | 説明 |
|---|---|
| `key` | パブリッシュされたメッセージのUser Propertyキー名 |
| `=` | 等価比較（キーの値が指定文字列と一致すること） |
| `>` | 数値比較（キーの値が指定数値より大きいこと） |
| `>=` | 数値比較（キーの値が指定数値以上であること） |
| `<` | 数値比較（キーの値が指定数値より小さいこと） |
| `<=` | 数値比較（キーの値が指定数値以下であること） |
| `&` | 複数条件の結合。すべての条件が真である必要がある |

フィルター式は**大文字・小文字を区別**します。指定したキーがメッセージのUser Propertiesに存在しない場合、そのメッセージはフィルタリングされ配信されません。

::: tip

Subscription FiltersはMQTT 5.0クライアントのみに適用されます。MQTT 3.1.1クライアントが`?`を含むトピック文字列でサブスクライブすると、その文字列全体がリテラルトピックフィルターとして扱われます。

:::

## 動作仕様

- EMQXはトピックフィルターが一致し、かつフィルター式が真と評価された場合にのみメッセージをサブスクライバーに配信します。
- フィルター式が参照するキーがメッセージのUser Propertiesに存在しない場合、そのメッセージは当該サブスクライバーに配信されません。
- 各サブスクリプションのフィルター式は独立して評価されます。あるサブスクライバーにメッセージが配信されるか否かは、同じトピックの他のサブスクライバーには影響しません。
- `?`区切りのないサブスクリプションは標準のMQTTサブスクリプションと同様に動作します。
- フィルター式の評価はサーバー側で行われ、クライアントはフィルタリングロジックを担いません。

## フィルター式の例

以下は一般的なサブスクリプションパターンの例です。

| サブスクリプション文字列 | 意味 |
|---|---|
| `sensor/+/temperature?location=roomA` | User Propertiesに`location=roomA`を含む温度メッセージを受信 |
| `sensor/+/temperature?value>25` | `value` User Propertyが25より大きい温度メッセージを受信 |
| `sensor/+/temperature?location=roomA&unit=celsius` | `location=roomA`かつ`unit=celsius`の両方を満たす温度メッセージを受信 |
| `home/lights/#` | 標準サブスクリプション。マッチするトピックのすべてのメッセージを受信 |

### パブリッシャー側

パブリッシャーはUser Propertiesとして以下を含むメッセージを`sensor/1/temperature`に送信します。

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

[認可](../access-control/authz/authz.md)が有効な場合、EMQXはサブスクリプショントピックを設定されたルールに対して検証します。認可に使用されるトピックは**ベーストピックフィルター**（`?`区切りの前の部分）です。フィルター式は認可評価前に除去されます。

例えば、`sensor/+/temperature?location=roomA`にサブスクライブするクライアントは、`sensor/+/temperature`に対して認可されている必要があります。Subscription Filtersで使用するベーストピックパターンに対応した認可ルールを設定してください。

## 関連機能リファレンス

Subscription FiltersはEMQXの他のメッセージング機能と補完的に利用できます。

- [Shared Subscriptions](../messaging/mqtt-shared-subscription.md)：サブスクライバーグループ間でメッセージを分散しロードバランシングを実現。コンテンツベースフィルタリングは非対応。
- [Retained Messages](../messaging/mqtt-retained-message.md)：トピックごとの最新メッセージを保存し、新規サブスクライバーに配信。Subscription Filterのフィルター式は保持メッセージ配信に影響しません。
- [Topic Rewrite](../messaging/mqtt-topic-rewrite.md)：ルーティング前にトピック文字列を書き換え。トピック書き換えルールはSubscription Filter評価より先に適用されます。
- [Wildcard Subscription](../messaging/mqtt-wildcard-subscription.md)：`+`や`#`ワイルドカードで複数トピックにマッチ。ワイルドカードトピックフィルターとSubscription Filtersは組み合わせ可能です。
- [Message Queue](../message-queue/message-queue-concept.md)：永続化ストレージと設定可能なディスパッチ戦略を備えた耐久性のある非同期メッセージキューを提供。<!-- 要検証 -->

## 次のステップ

Subscription Filterの概念を理解したら、実際の利用方法を確認しましょう。

- [Subscription Filtersの使い始め](./subscription-filter-get-started.md)：機能の有効化手順とMQTTX CLIを使ったフィルター動作のエンドツーエンド検証をステップバイステップで解説しています。
