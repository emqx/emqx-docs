# Message Queue クイックスタート

このページでは、EMQX 6.0 の Message Queue 機能の使い方を説明します。MQTTX を使ってクライアントをシミュレートし、EMQX ダッシュボードからメッセージキューを作成・管理し、メッセージがどのように確実に保存・配信されるかを確認します。

## 目的

このクイックスタートでは、EMQX Message Queue が以下を実現できることを紹介します。

- サブスクライバーがオフラインでもメッセージを永続化する
- 設定可能なディスパッチ戦略をサポートする
- メッセージ圧縮のための Last-Value Semantics を有効にする

## 前提条件

開始する前に、以下を準備してください。

- EMQX 6.0 以上が稼働している（Message Queue 機能が有効）
- [MQTTX](https://mqttx.app/)（または MQTT 5.0 対応のクライアント）
- EMQX ダッシュボードへのアクセス（デフォルト：`http://localhost:18083`）

## Message Queue の基本機能を試す

このセクションでは、EMQX Message Queue がメッセージをどのように永続化し配信するかを示します。MQTTX を使って MQTT クライアントをシミュレートし、サブスクライバーがオフラインのときでもメッセージが保持され、配信される様子を確認します。

### ステップ 1: メッセージキューを作成する

1. 左メニューの **Message Queue** に移動します。
2. ページ右上の **Create** ボタンをクリックします。

3. **Create Message Queue** ダイアログで以下の設定を行います。
   - **Topic Filter**: `demo/topic`
   - **Dispatch Strategy**: `Random`
   - **Data Retention Period**: `1` 日
   - **Last Value Semantics**: `Disabled`
4. **Create** をクリックします。

### ステップ 2: メッセージをパブリッシュする

MQTTX を使って **パブリッシャー** としてクライアントをシミュレートします。

1. MQTTX を開き、クライアントを作成します（例：`publisher`）。
2. EMQX に接続します（`mqtt://localhost:1883`）。
3. トピック `demo/topic` に QoS 1 でメッセージをパブリッシュします。

例：

```
Topic: demo/topic
QoS: 1
Payload: {"msg": "Hello 1"}
```

`{"msg": "Hello 2"}` など、ペイロードを変えて複数回繰り返します。

この時点ではサブスクライバーがいません。メッセージは EMQX によってキューに格納され永続化されます。

### ステップ 3: サブスクライブしてメッセージを受信する

MQTTX を使って **サブスクライバー** としてクライアントをシミュレートします。

1. 2つ目のクライアントを開きます（例：`worker-a`）。
2. EMQX に接続します。
3. キュートピックにサブスクライブします。

   ```json
   Topic: $q/demo/topic
   QoS: 1
   ```

これで、キューに蓄積されたすべてのメッセージを受信できます。

<img src="./assets/consume_message.png" alt="メッセージを受信する様子" style="zoom:67%;" />

## 複数サブスクライバーとディスパッチ戦略のシミュレーション

このセクションでは、同じ Message Queue に複数のサブスクライバーが接続している状況をシミュレートし、異なるディスパッチ戦略がメッセージ配信に与える影響を確認します。

1. `publisher` クライアントで、元のトピック（`$q/` プレフィックスなし）に複数のメッセージをパブリッシュします。例：

   ```bash
   for i in {1..10}; do
     mqttx pub -t demo/topic -m "message-$i" -q 1
   done
   ```

2. もう一つ MQTTX クライアントを作成します（例：`worker-b`）。
3. EMQX に接続し、同じキュートピックにサブスクライブします。

   ```json
   Topic: $q/demo/topic
   QoS: 1
   ```

これで、`worker-a` と `worker-b` の両方が同じキューからメッセージを受信します。

4. 両方のサブスクライバーでメッセージの流れを観察します。

### ディスパッチ戦略が配信に与える影響

キューの **Dispatch Strategy** によってメッセージ配信の挙動が変わります。

| Dispatch Strategy           | 挙動                                                         | 利用シーン                             |
| --------------------------- | ------------------------------------------------------------ | ------------------------------------ |
| `Least Inflight Subscriber` | 未アック（未確認）メッセージが少ないサブスクライバーを優先 | 負荷が偏ったコンシューマ間の負荷分散 |
| `Round Robin`               | サブスクライバーに順番にメッセージを配信                     | 速度に関係なく公平に配信したい場合   |
| `Random` (デフォルト)       | ランダムにサブスクライバーを選んでメッセージを送信           | 予測不能な配信やデモ用               |

`worker-a` と `worker-b` のメッセージ配信状況を見て、これらの挙動を確認できます。

### ディスパッチ戦略の変更

戦略は動的に変更可能です。

1. ダッシュボードの **Message Queue** に移動します。
2. 対象のキューの横にある **Edit** をクリックします。
3. 新しい **Dispatch Strategy** を選択して保存します。

ただし、アクティブなサブスクライバーがオンラインの場合は新しい戦略は適用されません。クライアントを一旦切断し、再接続してください。

切り替え後、再度メッセージをパブリッシュして、サブスクライバー間の配信パターンの違いを観察してください。

## Last-Value Semantics を試す

このセクションでは、**Last-Value Semantics** を有効にする方法を示します。これは、キーごとに最新のメッセージのみをキューに保持する機能で、デバイス設定の更新などに適しています。

### ステップ 1: 既存のキューを削除する

1. EMQX ダッシュボードの **Message Queue** に移動します。
2. トピックフィルターが `demo/topic` のキューを探します。
3. **Actions** 列の **Delete** をクリックします。
4. 確認ダイアログで削除を確定します。

これで、以前のキューと保存されていたメッセージが削除されます。

### ステップ 2: Last-Value Semantics を有効にしたキューを作成する

1. **Message Queue** ページで **Create** をクリックします。
2. **Create Message Queue** ダイアログで以下を設定します。
   - **Topic Filter**: `device/config`
   - **Dispatch Strategy**: `Random`（または任意）
   - **Data Retention Period**: `1` 日
   - **Last Value Semantics**: 有効にする（オンに切り替え）
   - **Queue Key Expression**: `message.from`（またはキーとして使うフィールド名）
3. **Create** をクリックします。

「Queue Key Expression」は、EMQX が各メッセージからキーを抽出し、Last-Value Queue で重複排除に使う方法を定義します。このフィールドは [Variform 式](../../guides/configuration/configuration.md#variform-expressions)で設定可能です。

このクイックスタートでは `message.from` を使い、メッセージのパブリッシャーのクライアントIDをキーとして抽出します。

> Queue Key Expression の高度な使い方やカスタムキー、メッセージ構造の例は [Queue Key Expression](./message-queue-task.md#queue-key-expression) を参照してください。

### ステップ 3: メッセージをパブリッシュする

1. MQTTX を開き、クライアントを選択または作成します（例：`publisher`）。
2. EMQX に接続します（`mqtt://localhost:1883`）。
3. `device/config` にメッセージをパブリッシュします。

例：

| フィールド   | 値                   |
| ------------ | -------------------- |
| **Topic**    | `device/config`      |
| **QoS**      | 1                    |
| **Payload**  | `{"ssid": "wifi1"}`  |

4. 同じクライアント（同じクライアントID）で内容を更新したメッセージを再度パブリッシュします。

```json
Payload: {"ssid": "wifi2"}
```

**Queue Key Expression** が `message.from` に設定されているため、EMQX は各メッセージからクライアントIDを自動的に抽出し、キューキーとして使用します。同じクライアントからのメッセージは、未消費の古いメッセージを上書きします。

### ステップ 4: キューにサブスクライブする

1. 2つ目の MQTTX クライアントを作成し（例：`subscriber`）、EMQX に接続します。
2. キュートピックにサブスクライブします。

```json
Topic: $q/device/config
QoS: 1
```

**期待される動作**：  
最新のメッセージのみが配信されます。この場合、`{"ssid": "wifi2"}` のみが受信されます。
