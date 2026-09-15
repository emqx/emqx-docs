# サブスクリプションフィルターの使い方

このページでは、EMQXでサブスクリプションフィルター機能を有効化し、ハンズオンで動作を確認する手順を説明します。MQTTX CLIを使ってパブリッシャーと複数のサブスクライバーをシミュレートし、フィルター式が各サブスクライバーに届くメッセージをどのように制御するかを観察します。

## 前提条件

開始する前に、以下を準備してください。

- EMQX 6.2以上が稼働していること
- [MQTTX CLI](https://mqttx.app/cli) がインストールされていること

## ステップ1：サブスクリプションフィルターを有効化する

サブスクリプションフィルターはデフォルトで無効になっています。無効時は、`?`文字はトピック文字列の通常の一部として扱われ、既存のサブスクリプションとの完全な後方互換性が保たれます。

### ダッシュボードから有効化する

1. **Management** -> **MQTT Settings** -> **General**タブに移動します。
2. **Subscription Message Filter**項目を見つけて、**enable**に設定します。
3. **Save Changes**をクリックします。

変更はブローカーの再起動なしに即時反映されます。

### 設定ファイルから有効化する

`emqx.conf`に以下を追加します。

```hocon
mqtt.subscription_message_filter = enable
```

変更を反映するにはEMQXを再起動するか、デプロイメントが対応していれば設定のリロードを行ってください。

### REST APIから有効化する

```bash
curl -s -u key:secret -X PUT \
  -H "Content-Type: application/json" \
  http://localhost:18083/api/v5/configs/mqtt \
  -d '{"subscription_message_filter": "enable"}'
```

有効化後、クライアントはサブスクリプションにフィルター式を付加できます。構文の詳細や例は、サブスクリプションフィルターの概要にある[Filter Syntax](./subscription-filter-concept.md#filter-syntax)をご参照ください。

## ステップ2：サブスクライバーを起動する

この例では、センサーが`sensor/1/temperature`に温度データをパブリッシュします。各メッセージには`location`というユーザープロパティが含まれています。3つのサブスクライバーが同じトピックを異なるフィルター式でサブスクライブします。

| サブスクライバー | サブスクリプション | 受信するメッセージ条件 |
|---|---|---|
| `sub-roomA` | `sensor/+/temperature?location=roomA` | `location=roomA` |
| `sub-roomB` | `sensor/+/temperature?location=roomB` | `location=roomB` |
| `sub-all` | `sensor/+/temperature` | 全メッセージ（フィルターなし） |

3つのターミナルを開き、それぞれのサブスクライバーを起動します。

**ターミナル1：roomAサブスクライバー**

```bash
mqttx sub -h localhost -p 1883 \
  --mqtt-version 5 \
  --client-id sub-roomA \
  -t "sensor/+/temperature?location=roomA"
```

**ターミナル2：roomBサブスクライバー**

```bash
mqttx sub -h localhost -p 1883 \
  --mqtt-version 5 \
  --client-id sub-roomB \
  -t "sensor/+/temperature?location=roomB"
```

**ターミナル3：フィルターなしサブスクライバー**

```bash
mqttx sub -h localhost -p 1883 \
  --mqtt-version 5 \
  --client-id sub-all \
  -t "sensor/+/temperature"
```

::: tip

`--mqtt-version 5`オプションは必須です。サブスクリプションフィルターはMQTT 5.0の機能に依存しています。

:::

## ステップ3：Room A向けのメッセージをパブリッシュする

4つ目のターミナルで、ユーザープロパティに`location=roomA`を含むメッセージをパブリッシュします。

```bash
mqttx pub -h localhost -p 1883 \
  --mqtt-version 5 \
  --client-id publisher \
  -t "sensor/1/temperature" \
  -m '{"value": 23.5}' \
  --user-properties "location: roomA"
```

**期待される結果：**

| サブスクライバー | メッセージ受信の有無 |
|---|---|
| `sub-roomA` | 受信（`location=roomA`が一致） |
| `sub-roomB` | 非受信（`location`の値が不一致） |
| `sub-all` | 受信（フィルター式なし） |

## ステップ4：Room B向けのメッセージをパブリッシュする

```bash
mqttx pub -h localhost -p 1883 \
  --mqtt-version 5 \
  --client-id publisher \
  -t "sensor/1/temperature" \
  -m '{"value": 19.1}' \
  --user-properties "location: roomB"
```

**期待される結果：**

| サブスクライバー | メッセージ受信の有無 |
|---|---|
| `sub-roomA` | 非受信（`location`の値が不一致） |
| `sub-roomB` | 受信（`location=roomB`が一致） |
| `sub-all` | 受信（フィルター式なし） |

## ステップ5：複数条件（AND論理）のテスト

サブスクリプションフィルターは`&`で複数条件を結合できます。`location`と`unit`の両方が一致することを要求する新しいサブスクライバーを起動します。

```bash
mqttx sub -h localhost -p 1883 \
  --mqtt-version 5 \
  --client-id sub-roomA-celsius \
  -t "sensor/+/temperature?location=roomA&unit=celsius"
```

両条件を満たすメッセージをパブリッシュします。

```bash
mqttx pub -h localhost -p 1883 \
  --mqtt-version 5 \
  --client-id publisher \
  -t "sensor/1/temperature" \
  -m '{"value": 22.0}' \
  --user-properties "location: roomA" \
  --user-properties "unit: celsius"
```

`sub-roomA-celsius`サブスクライバーはメッセージを受信します。次に、`unit`が不一致のメッセージをパブリッシュします。

```bash
mqttx pub -h localhost -p 1883 \
  --mqtt-version 5 \
  --client-id publisher \
  -t "sensor/1/temperature" \
  -m '{"value": 71.6}' \
  --user-properties "location: roomA" \
  --user-properties "unit: fahrenheit"
```

`sub-roomA-celsius`サブスクライバーは、このメッセージを受信しません。`location=roomA`は一致していますが、`unit`条件が満たされていないためです。

## ステップ6：ユーザープロパティなしのメッセージをパブリッシュする

ユーザープロパティを含まないメッセージをパブリッシュします。

```bash
mqttx pub -h localhost -p 1883 \
  --mqtt-version 5 \
  --client-id publisher \
  -t "sensor/1/temperature" \
  -m '{"value": 20.0}'
```

**期待される結果：**

| サブスクライバー | メッセージ受信の有無 |
|---|---|
| `sub-roomA` | 非受信（`location`キーが存在しない） |
| `sub-roomB` | 非受信（`location`キーが存在しない） |
| `sub-all` | 受信（フィルター式なし） |

これは、必須のユーザープロパティキーが欠落している場合、フィルター式付きサブスクライバーにはメッセージがフィルタリングされることを示しています。

## まとめ

| シナリオ | 動作 |
|---|---|
| メッセージのユーザープロパティがフィルター式に一致 | 配信される |
| メッセージのユーザープロパティが部分的に一致（AND条件未達成） | 配信されない |
| 必須のユーザープロパティキーが欠落 | 配信されない |
| サブスクリプションにフィルター式がない | トピック一致する全メッセージが配信される |

## 次のステップ

- [サブスクリプションフィルター概要](./subscription-filter-concept.md)：設計、概念、ユースケースを詳しく理解する
- [ワイルドカードサブスクリプション](../../get-started/messaging/mqtt-wildcard-subscription.md)：ワイルドカートピックフィルターとサブスクリプションフィルターを組み合わせて柔軟なルーティングを実現する
