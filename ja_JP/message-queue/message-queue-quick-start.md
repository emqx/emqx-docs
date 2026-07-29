# Message Queue クイックスタート

このページでは、Message Queue 機能を素早く利用する手順を説明します。MQTTX を使ってクライアントをシミュレートし、EMQX ダッシュボードからメッセージキューを作成・管理し、メッセージがどのように確実に保存・配信されるかを確認します。

## 目的

このクイックスタートでは、EMQX Message Queue が以下を実現できることを紹介します。

- サブスクライバーがオフラインでもメッセージを永続化できる
- 設定可能なディスパッチ戦略をサポートする
- メッセージの圧縮における Last-Value Semantics を有効にできる

## 前提条件

開始前に以下を準備してください。

- EMQX 6.0 以上が稼働している（Message Queue 機能が有効）
- [MQTTX](https://mqttx.app/)（または MQTT 5.0 対応のクライアント）
- EMQX ダッシュボードへのアクセス（デフォルト：`http://localhost:18083`）

## Message Queue の基本機能を試す

このセクションでは、EMQX Message Queue がメッセージをどのように永続化し配信するかを示します。MQTTX を使ってクライアントをシミュレートし、サブスクライバーがオフラインでもメッセージが保持・配信される様子を確認します。

### ステップ 1: キューを作成する

1. 左メニューの **Queues** に移動します。
2. ページ右上の **Create** ボタンをクリックします。

3. **Create Queue** ダイアログで以下の設定を行います。
   - **Name**: `my_queue`
   - **Topic Filter**: `demo/topic`
   - **Dispatch Strategy**: `Random`
   - **Data Retention Period**: `7` 日
   - **Last Value Semantics**: `Disabled`
4. **Create** をクリックします。

### ステップ 2: メッセージをパブリッシュする

MQTTX を使い、**パブリッシャー**としてクライアントをシミュレートします。

1. MQTTX を開き、クライアント（例：`publisher`）を作成します。
2. EMQX に接続します（`mqtt://localhost:1883`）。
3. トピック `demo/topic` に QoS 1 でメッセージをパブリッシュします。

例：

```
Topic: demo/topic
QoS: 1
Payload: {"msg": "Hello 1"}
```

`{"msg": "Hello 2"}` など、他のペイロードでも繰り返します。

この時点ではサブスクライバーがいません。メッセージは EMQX によってキューイングされ永続化されます。

### ステップ 3: サブスクライブしてメッセージを受信する

MQTTX を使い、**サブスクライバー**としてクライアントをシミュレートします。

1. 2つ目のクライアント（例：`worker-a`）を開きます。

2. EMQX に接続します。

3. キュートピックにサブスクライブします。

   ```json
   Topic: $queue/my_queue/demo/topic
   QoS: 1
   ```

これで、キューに保存されていたすべてのメッセージを受信できます。

<img src="./assets/consume_message.png" alt="メッセージ受信画面" style="zoom:67%;" />

## 複数サブスクライバーのシミュレーションとディスパッチ戦略

このセクションでは、同じ Message Queue に複数のサブスクライバーが接続した場合の動作をシミュレートし、異なるディスパッチ戦略がメッセージ配信に与える影響を確認します。

1. `publisher` クライアントで、元のトピック（`$queue/` プレフィックスなし）に複数のメッセージをパブリッシュします。

   ```bash
   for i in {1..10}; do
     mqttx pub -t demo/topic -m "message-$i" -q 1
   done
   ```

2. もう一つ MQTTX クライアント（例：`worker-b`）を作成します。

3. EMQX に接続し、同じキュートピックにサブスクライブします。

   ```json
   Topic: $queue/my_queue/demo/topic
   QoS: 1
   ```

   これで `worker-a` と `worker-b` の両方が同じキューからメッセージを消費しています。

4. 両方のサブスクライバーでメッセージの流れを観察します。

### ディスパッチ戦略が配信に与える影響

キューの **Dispatch Strategy** によってメッセージ配信の挙動が変わります。

| Dispatch Strategy           | 挙動                                                         | ユースケース                             |
| --------------------------- | ------------------------------------------------------------ | -------------------------------------- |
| `Least Inflight Subscriber` | 未アック（未確認）メッセージが少ないサブスクライバーを優先 | 不均一な消費者間の負荷分散               |
| `Round Robin`               | サブスクライバーに順番にメッセージを配信                     | 速度に関係なく公平に配信                 |
| `Random` (デフォルト)       | ランダムに選ばれたサブスクライバーにメッセージを送信         | 予測不能なシナリオやデモ用途             |

`worker-a` と `worker-b` へのメッセージ配信を観察し、これらの挙動を確認できます。

### ディスパッチ戦略の変更

戦略は動的に変更可能です。

1. ダッシュボードの **Queues** に移動します。
2. 対象キューの **Edit** をクリックします。
3. 新しい **Dispatch Strategy** を選択し保存します。

ただし、サブスクライバーがオンラインの間は新しい戦略は適用されません。クライアントを切断し再接続する必要があります。

切り替え後、再度メッセージパブリッシュを試し、サブスクライバー間の配信パターンの違いを確認してください。

## Last-Value Semantics のテスト

このセクションでは、**Last-Value Semantics** を有効にする方法を示します。これは、キーごとに最新のメッセージのみをキューに保持し、デバイス設定の更新などに最適です。

### ステップ 1: 既存キューの削除

1. EMQX ダッシュボードの **Queues** に移動します。
2. トピックフィルターが `demo/topic` のキューを見つけます。
3. **Actions** 列の **Delete** をクリックします。
4. 確認ダイアログで削除を承認します。

これで前のキューと保存されていたメッセージが削除されます。

### ステップ 2: Last-Value Semantics を有効にしたキューを作成

1. **Queues** ページで **Create** をクリックします。
2. **Create Queue** ダイアログで以下を設定します。
   - **Name**: `my_queue`
   - **Topic Filter**: `device/config`
   - **Dispatch Strategy**: `Random`（または任意）
   - **Data Retention Period**: `7` 日
   - **Last Value Semantics**: 有効にする
   - **Queue Key Expression**: `message.from`（またはキーとして使う任意のフィールド名）
3. **Create** をクリックします。

「Queue Key Expression」は、EMQX が各メッセージからキーを抽出し、Last-Value Queue での重複排除に使う方法を定義します。このフィールドは [Variform 式](../configuration/configuration.md#variform-expressions) を使って設定可能です。

このクイックスタートでは `message.from` を使い、メッセージパブリッシャーのクライアント ID からキーを抽出しています。

> Queue Key Expression の詳細な使い方やカスタムキー、メッセージ構造の例は [Queue Key Expression](./message-queue-task.md#queue-key-expression) を参照してください。

### ステップ 3: メッセージをパブリッシュする

1. MQTTX を開き、クライアント（例：`publisher`）を選択または作成します。

2. EMQX に接続します（`mqtt://localhost:1883`）。

3. `device/config` にメッセージをパブリッシュします。

   例：

   | フィールド   | 値                  |
   | ----------- | ------------------- |
   | **Topic**   | `device/config`     |
   | **QoS**     | 1                   |
   | **Payload** | `{"ssid": "wifi1"}` |

4. 同じクライアント（同じクライアント ID）で内容を更新したメッセージをパブリッシュします。

   ```json
   Payload: {"ssid": "wifi2"}
   ```

`Queue Key Expression` が `message.from` に設定されているため、EMQX は各メッセージからクライアント ID を抽出し、それをキューキーとして使用します。同じクライアントからのメッセージは、未消費の前メッセージを上書きします。

### ステップ 4: キューにサブスクライブする

1. 2つ目の MQTTX クライアント（例：`subscriber`）を作成し、EMQX に接続します。

3. キュートピックにサブスクライブします。

   ```json
   Topic: $queue/my_queue/device/config
   QoS: 1
   ```

**期待される挙動**：
最新のメッセージのみが配信されます。この例では `{"ssid": "wifi2"}` のみ受信されます。
