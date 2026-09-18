# Message Queue クイックスタート

このページでは、Message Queue 機能の簡単な使い方を説明します。MQTTX を使ってクライアントをシミュレートし、EMQX ダッシュボードからメッセージキューを作成・管理し、メッセージがどのように確実に保存・配信されるかを確認します。

## 目的

このクイックスタートでは、EMQX Message Queue が以下のことを実現できることを紹介します。

- サブスクライバーがオフラインでもメッセージを永続化する
- 設定可能な配信戦略をサポートする
- メッセージの圧縮に Last-Value Semantics を有効にする

## 前提条件

開始前に以下を準備してください。

- EMQX 6.0 以上が稼働していること（Message Queue 機能が有効）
- [MQTTX](https://mqttx.app/)（または MQTT 5.0 対応クライアント）
- EMQX ダッシュボードへのアクセス（デフォルト：`http://localhost:18083`）

## Message Queue の基本機能を試す

このセクションでは、EMQX Message Queue がメッセージをどのように永続化し配信するかを示します。MQTTX を使って MQTT クライアントをシミュレートし、サブスクライバーがオフラインの間もメッセージが保持・配信される様子を確認します。

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

MQTTX を使ってパブリッシャーとしてクライアントをシミュレートします。

1. MQTTX を開き、クライアント（例：`publisher`）を作成します。
2. EMQX に接続します（`mqtt://localhost:1883`）。
3. トピック `demo/topic` に QoS 1 でメッセージをパブリッシュします。

例：

```
Topic: demo/topic
QoS: 1
Payload: {"msg": "Hello 1"}
```

続けて `{"msg": "Hello 2"}` などのペイロードで複数回パブリッシュしてください。

この時点ではサブスクライバーはいません。メッセージは EMQX によってキューに入り永続化されます。

### ステップ 3: サブスクライブしてメッセージを受信する

MQTTX を使ってサブスクライバーとしてクライアントをシミュレートします。

1. 2つ目のクライアント（例：`worker-a`）を開きます。

2. EMQX に接続します。

3. キュートピックにサブスクライブします。

   ```json
   Topic: $queue/my_queue/demo/topic
   QoS: 1
   ```

これでキューに溜まっていたすべてのメッセージを受信できます。

<img src="./assets/consume_message.png" alt="メッセージを消費する様子" style="zoom:67%;" />

## 複数サブスクライバーと配信戦略のシミュレーション

このセクションでは、複数のサブスクライバーが同じ Message Queue に接続した場合の動作をシミュレートし、異なる配信戦略がメッセージ配信に与える影響を確認します。

1. `publisher` クライアントで元のトピック（`$queue/` プレフィックスなし）に複数のメッセージをパブリッシュします。

   ```bash
   for i in {1..10}; do
     mqttx pub -t demo/topic -m "message-$i" -q 1
   done
   ```

2. 別の MQTTX クライアント（例：`worker-b`）を作成します。

3. EMQX に接続し、同じキュートピックにサブスクライブします。

   ```json
   Topic: $queue/my_queue/demo/topic
   QoS: 1
   ```

   これで `worker-a` と `worker-b` の両方が同じキューからメッセージを消費します。

4. 両方のサブスクライバーでメッセージの流れを観察してください。

### 配信戦略が配信に与える影響

キューの **Dispatch Strategy** によってメッセージの配信挙動は異なります。

| 配信戦略（Dispatch Strategy）       | 挙動                                                         | 利用ケース                             |
| ---------------------------------- | ------------------------------------------------------------ | ------------------------------------ |
| `Least Inflight Subscriber`        | 未アックのメッセージが少ないサブスクライバーを優先して配信する | 不均一な消費者間での負荷分散           |
| `Round Robin`                      | サブスクライバーに順番に交互に配信する                         | 速度に関係なく公平に配信したい場合     |
| `Random`（デフォルト）             | ランダムに選ばれたサブスクライバーに配信する                   | 予測不能な配信やデモ用途               |

これらの挙動は `worker-a` と `worker-b` のメッセージ受信状況を見て確認できます。

### 配信戦略の変更

配信戦略は動的に変更可能です。

1. ダッシュボードの **Queues** に移動します。
2. 対象キューの **Edit** をクリックします。
3. 新しい **Dispatch Strategy** を選択し保存します。

ただし、アクティブなサブスクライバーがオンラインの間は新しい配信戦略は適用されません。クライアントを一旦切断し、再接続してください。

切り替え後、再度メッセージをパブリッシュしてサブスクライバー間の配信パターンの違いを観察してください。

## Last-Value Semantics を試す

このセクションでは、**Last-Value Semantics** を有効にする方法を説明します。これはキーごとに最新のメッセージのみをキューに保持する機能で、デバイス設定の更新などに適しています。

### ステップ 1: 既存キューの削除

1. EMQX ダッシュボードの **Queues** に移動します。
2. トピックフィルターが `demo/topic` のキューを探します。
3. **Actions** 列の **Delete** をクリックします。
4. 確認ダイアログで削除を確定します。

これで以前のキューと保存されていたメッセージが削除されます。

### ステップ 2: Last-Value Semantics を有効にしたキューを作成

1. **Queues** ページで **Create** をクリックします。
2. **Create Queue** ダイアログで以下を設定します。
   - **Name**: `my_queue`
   - **Topic Filter**: `device/config`
   - **Dispatch Strategy**: `Random`（または任意）
   - **Data Retention Period**: `7` 日
   - **Last Value Semantics**: 有効にする（トグルオン）
   - **Queue Key Expression**: `message.from`（またはキーに使いたい任意のフィールド名）
3. **Create** をクリックします。

「Queue Key Expression」は、EMQX がメッセージからキーを抽出し、Last-Value Queue で重複排除に使う方法を定義します。このフィールドは [Variform 式](../../guides/configuration/configuration.md#variform-expressions) で設定可能です。

このクイックスタートでは `message.from` を使い、メッセージのパブリッシャーのクライアント ID をキーとして抽出しています。

> キューキー式の高度な使い方（カスタムキーやメッセージ構造の例）は [Queue Key Expression](./message-queue-task.md#queue-key-expression) をご参照ください。

### ステップ 3: メッセージをパブリッシュする

1. MQTTX を開き、クライアント（例：`publisher`）を選択または作成します。

2. EMQX に接続します（`mqtt://localhost:1883`）。

3. `device/config` にメッセージをパブリッシュします。

   例：

   | フィールド   | 値                 |
   | ------------ | ------------------ |
   | **Topic**    | `device/config`    |
   | **QoS**      | 1                  |
   | **Payload**  | `{"ssid": "wifi1"}` |

4. 同じクライアント（同じクライアント ID）で内容を更新したメッセージをパブリッシュします。

   ```json
   Payload: {"ssid": "wifi2"}
   ```

**Queue Key Expression** が `message.from` に設定されているため、EMQX は各メッセージからクライアント ID を自動抽出し、同じクライアントからのメッセージはキュー内の未消費メッセージを上書きします。

### ステップ 4: キューにサブスクライブする

1. 2つ目の MQTTX クライアント（例：`subscriber`）を作成し、EMQX に接続します。

3. キュートピックにサブスクライブします。

   ```json
   Topic: $queue/my_queue/device/config
   QoS: 1
   ```

**期待される動作**：

最新のメッセージのみが配信されます。この例では `{"ssid": "wifi2"}` のみが受信されます。
