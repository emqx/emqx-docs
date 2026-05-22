# Subscription Filters

EMQX 6.2で導入されたSubscription Filter機能は、MQTT 5.0のパブリッシュ／サブスクライブモデルにサブスクリプションレベルでのコンテンツベースフィルタリングを拡張します。これにより、クライアントはトピックフィルターと追加のフィルター式の両方に一致するメッセージのサブセットのみを受信でき、不必要なメッセージ配信やネットワークのオーバーヘッドを削減できます。

本ページでは、EMQXにおけるSubscription Filtersの設計動機、主要概念、フィルター式の構文、動作セマンティクス、および実際のユースケースを包括的に解説します。

## Subscription Filterとは何か？

Subscription Filterは、MQTTサブスクリプションに付加できるオプションのフィルター条件です。パブリッシュされたメッセージがサブスクリプションのトピックフィルターに一致した場合、EMQXはメッセージのMQTT 5.0 User Propertiesに対してフィルター式を評価し、その両方を満たすメッセージのみをサブスクライバーに転送します。

標準のMQTTルーティングでは、トピックに一致したすべてのメッセージがサブスクライバーに転送されます。

```
Publisher --> Topic -- (Filter) --> Subscription --> Subscriber
```

Subscription Filtersはメッセージルーティング経路に第2のフィルタリングレベルを導入します。

```
Publisher --> Topic -- (Filter) --> Subscription -- (Filter) --> Subscriber
```

この2段階のフィルタリング機構により、トピックベースとコンテンツベースの両方のフィルタリングが可能となり、クライアントは受信したいメッセージを正確に指定できます。

## なぜSubscription Filtersを使うのか？

標準のMQTT 5.0サブスクリプションはトピックマッチングのみに基づいてメッセージをルーティングします。マッチしたトピックにパブリッシュされたすべてのメッセージは、メッセージ内容に関わらずすべてのサブスクライバーに配信されます。これは以下のようなシナリオで制約となることがあります。

- サブスクライバーが特定の地域、デバイスグループ、カテゴリのメッセージのみを受信したい場合
- 高頻度のトピックに混在するデータを異なるコンシューマーが独立して分割して処理したい場合
- すべてのメッセージをクライアントに配信するとネットワーク負荷や処理負荷が不必要に増大する場合

Subscription Filtersは、サブスクリプション時に正確でコンテンツ認識型の配信ルールを宣言できるようにし、パブリッシャーやトピック構造、データ軸ごとの個別トピックの変更を不要にします。

## 主要概念

- **トピックフィルター**：サブスクリプションの標準MQTTトピックフィルター部分（`?`より前の部分）。どのメッセージがルーティング段階に入るかを決定します。

- **フィルター式**：コンテンツベースのフィルター条件（`?`より後の部分）。トピックフィルターを通過したメッセージのMQTT 5.0 User Propertiesに対して評価されます。

- **User Properties**：MQTT 5.0メッセージに付加されるキーと値のメタデータ。パブリッシャーが`location`や`device_type`、`region`などの追加情報を提供し、サブスクライバーがこれを基にフィルタリングできます。

- **2段階配信**：トピックフィルターの評価に続きフィルター式の評価を行い、両方を満たすメッセージのみをサブスクライバーに配信する仕組み。

- **フィルターなしサブスクリプション**：`?`区切りがないサブスクリプション。標準MQTTサブスクリプションとして扱われ、トピックマッチしたすべてのメッセージが配信されます。

## Subscription Filtersの動作

Subscription FiltersはMQTT 5.0の**User Properties**をフィルタリング対象とします。クライアントがメッセージをパブリッシュする際、User Propertiesヘッダーにキーと値のペアを含めることができます。EMQXは各フィルター式をこれらのキー・バリューに対して評価し、式に一致した場合のみメッセージを配信します。

Subscription Filtersはデフォルトで無効です。有効化手順は[Subscription Filtersの使い始め](./subscription-filter-get-started.md)をご参照ください。

### フィルター構文

Subscription Filterはトピックフィルターに`?`を区切り文字として付加します。

```
<topic-filter>?<filter-expression>
```

| コンポーネント | 説明 |
|---|---|
| `<topic-filter>` | 標準のMQTTトピックフィルター（例：`sensor/+/temperature`や`home/#`） |
| `?` | トピックフィルターとフィルター式を区切るデリミタ |
| `<filter-expression>` | メッセージのUser Propertiesに対して評価されるキー・バリューのフィルター条件 |

### フィルター式の形式

フィルター式は等価比較や大小比較演算子をサポートします。複数条件は`&`（論理AND）で結合します。

```
key1=value1&key2>value2
```

| 要素 | 説明 |
|---|---|
| `key` | パブリッシュされたメッセージのUser Propertyキー名 |
| `=` | 等価比較（キーの値が指定した文字列と一致すること） |
| `>` | 数値比較（キーの値が指定した数値より大きいこと） |
| `>=` | 数値比較（キーの値が指定した数値以上であること） |
| `<` | 数値比較（キーの値が指定した数値より小さいこと） |
| `<=` | 数値比較（キーの値が指定した数値以下であること） |
| `&` | 複数条件の結合。すべての条件が真の場合にメッセージが配信される |

フィルター式は**大文字・小文字を区別**します。指定したキーがメッセージのUser Propertiesに存在しない場合、そのメッセージはフィルターアウトされます。

::: tip

Subscription FiltersはMQTT 5.0クライアントのみ適用されます。MQTT 3.1.1クライアントが`?`を含むトピック文字列でサブスクライブした場合、その文字列全体がリテラルトピックフィルターとして扱われます。

:::

## 動作セマンティクス

- EMQXはメッセージをサブスクライバーに配信するのは、**トピックフィルターに一致し**かつ**フィルター式が真と評価された場合のみ**です。
- フィルター式が参照するキーがメッセージのUser Propertiesに存在しない場合、そのメッセージは該当サブスクライバーに**配信されません**。
- 各サブスクリプションのフィルター式は独立して評価されます。あるサブスクライバーにメッセージが配信されるかどうかは、同じトピックの他のサブスクライバーへの配信に影響しません。
- `?`区切りのないサブスクリプションは標準のMQTTサブスクリプションと同様に動作します。
- フィルター式の評価はサーバー側で行われ、クライアント側でのフィルタリングロジックは不要です。

## フィルター式の例

以下は一般的なサブスクリプションパターンの例です。

| サブスクリプション文字列 | 意味 |
|---|---|
| `sensor/+/temperature?location=roomA` | User Propertiesに`location=roomA`を含む温度メッセージを受信 |
| `sensor/+/temperature?value>25` | `value` User Propertyが25より大きい温度メッセージを受信 |
| `sensor/+/temperature?location=roomA&unit=celsius` | `location=roomA`かつ`unit=celsius`の両方を満たす温度メッセージを受信 |
| `home/lights/#` | 標準サブスクリプション。マッチするトピックのすべてのメッセージを受信 |

### パブリッシャー側

パブリッシャーは`sensor/1/temperature`に以下のUser Propertiesを付けてメッセージを送信します。

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
| `sensor/+/temperature?location=roomA&unit=celsius` | はい | 両条件が一致 |
| `sensor/+/temperature?location=roomA&unit=fahrenheit` | いいえ | `unit`の値が一致しない |
| `sensor/+/temperature` | はい | フィルター式なしの標準サブスクリプション |

## 認可に関する考慮事項

[認可](../access-control/authz/authz.md)が有効な場合、EMQXはサブスクリプショントピックを設定されたルールに対して検証します。認可に使用されるトピックは**ベーストピックフィルター**（`?`区切りより前の部分）です。フィルター式は認可評価前に除去されます。

例えば、`sensor/+/temperature?location=roomA`にサブスクライブするクライアントは`sensor/+/temperature`に対して認可されている必要があります。認可ルールはSubscription Filtersで使われるベーストピックパターンを考慮してください。

## 関連機能のリファレンス

Subscription FiltersはEMQXの他のメッセージング機能と補完的に利用できます。

- [Shared Subscriptions](../messaging/mqtt-shared-subscription.md)：サブスクライバーグループ間でメッセージを分散しロードバランシングを実現。コンテンツベースフィルタリングはサポートしません。
- [Retained Messages](../messaging/mqtt-retained-message.md)：トピックごとに最後のメッセージを保存し、新規サブスクライバーに配信。Retainedメッセージの配信はSubscription Filter式の影響を受けません。
- [Topic Rewrite](../messaging/mqtt-topic-rewrite.md)：ルーティング前にトピック文字列を書き換え。トピックリライトルールはSubscription Filter評価前に適用されます。
- [Wildcard Subscription](../messaging/mqtt-wildcard-subscription.md)：`+`や`#`のワイルドカードで複数トピックにマッチ。ワイルドカートトピックフィルターはSubscription Filtersと組み合わせ可能です。
- [Message Queue](../message-queue/message-queue-concept.md)：永続化ストレージと設定可能なディスパッチ戦略を備えた耐久性のある非同期メッセージキューを提供します。 <!-- 要検証 -->

## 次のステップ

Subscription Filterの概念を理解したら、実際の利用方法を確認しましょう。

- [Subscription Filtersの使い始め](./subscription-filter-get-started.md)：機能の有効化方法とMQTTX CLIを用いたフィルター動作のエンドツーエンド検証手順を解説しています。
