# Message Queue クイックスタート

このページでは、Message Queue 機能の簡単な使い方を説明します。MQTTX を使ってクライアントをシミュレートし、EMQX ダッシュボードからメッセージキューを作成・管理し、メッセージがどのように確実に保存・配信されるかを確認します。

## 目的

このクイックスタートでは、EMQX Message Queue が以下のことを実現できることを紹介します。

- サブスクライバーがオフラインでもメッセージを永続化する
- 設定可能なディスパッチ戦略をサポートする
- メッセージ圧縮のための Last-Value Semantics を有効にする

## 前提条件

開始前に以下を準備してください。

- EMQX 6.0 以上が稼働中（Message Queue 機能が有効）
- [MQTTX](https://mqttx.app/)（または MQTT 5.0 対応のクライアント）
- EMQX ダッシュボードへのアクセス（デフォルト：`http://localhost:18083`）

## Message Queue 基本機能のテスト

このセクションでは、EMQX Message Queue がメッセージをどのように永続化し配信するかを示します。MQTTX を使って MQTT クライアントをシミュレートし、サブスクライバーがオフラインでもメッセージが保持・配信される様子を確認します。

### ステップ 1: キューの作成

1. 左メニューの **Queues** に移動します。
2. ページ右上の **Create** ボタンをクリックします。

3. **Create Queue** ダイアログで以下の設定を行います。
   - **Name**: `my_queue`
   - **Topic Filter**: `demo/topic`
   - **Dispatch Strategy**: `Random`
   - **Data Retention Period**: `7` 日
   - **Last Value Semantics**: `Disabled`
4. **Create** をクリックします。

### ステップ 2: メッセージのパブリッシュ

MQTTX を使い、**パブリッシャー** としてクライアントをシミュレートします。

1. MQTTX を開き、クライアント（例：`publisher`）を作成します。
2. EMQX に接続します（`mqtt://localhost:1883`）。
3. トピック `demo/topic` に QoS 1 でメッセージをパブリッシュします。

例：

```
Topic: demo/topic
QoS: 1
Payload: {"msg": "Hello 1"}
```

`{"msg": "Hello 2"}` など、ペイロードを変えて複数回繰り返します。

この時点ではサブスクライバーは存在しません。メッセージは EMQX によってキューに保存されます。

### ステップ 3: サブスクライブしてメッセージを消費

MQTTX を使い、**サブスクライバー** としてクライアントをシミュレートします。

1. 2つ目のクライアント（例：`worker-a`）を開きます。

2. EMQX に接続します。

3. キュートピックにサブスクライブします。

   ```json
   Topic: $queue/my_queue/demo/topic
   QoS: 1
   ```

これでキューに保存されていたすべてのメッセージを受信できます。

<img src="./assets/consume_message.png" alt="メッセージの消費" style="zoom:67%;" />

## 複数サブスクライバーとディスパッチ戦略のシミュレーション

このセクションでは、同じ Message Queue に複数のサブスクライバーが接続した場合の動作をシミュレートし、異なるディスパッチ戦略がメッセージ配信にどう影響するかを確認します。

1. `publisher` クライアントで、元のトピック（`$queue/` プレフィックスなし）に複数のメッセージをパブリッシュします。例：

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

   これで `worker-a` と `worker-b` の両方が同じキューからメッセージを消費します。

4. 両サブスクライバーのメッセージ受信状況を観察してください。

### ディスパッチ戦略が配信に与える影響

キューの **Dispatch Strategy** によってメッセージ配信の挙動が変わります。

| Dispatch Strategy           | 挙動                                                         | 利用ケース                             |
| --------------------------- | ------------------------------------------------------------ | ------------------------------------ |
| `Least Inflight Subscriber` | 未アック（未確認）メッセージが少ないサブスクライバーを優先 | 不均一な消費者間の負荷分散             |
| `Round Robin`               | サブスクライバーに順番に交互にメッセージを配信               | 処理速度に関係なく公平に配信           |
| `Random` (デフォルト)       | ランダムに選んだサブスクライバーにメッセージを送信           | 予測不能なシナリオやデモ用途           |

`worker-a` と `worker-b` のメッセージ配信状況を見て、これらの挙動を確認できます。

### ディスパッチ戦略の変更

戦略は動的に変更可能です。

1. ダッシュボードの **Queues** に移動します。
2. 対象キューの **Edit** をクリックします。
3. 新しい **Dispatch Strategy** を選択し、保存します。

ただし、サブスクライバーがオンラインの間は新しい戦略は適用されません。クライアントを切断し、再接続する必要があります。

切り替え後、再度メッセージパブリッシュを試し、サブスクライバー間の配信パターンの違いを観察してください。

## Last-Value Semantics のテスト

このセクションでは、**Last-Value Semantics** を有効にする方法を示します。これはキーごとに最新のメッセージのみをキューに保持する機能で、デバイス設定の更新などに適しています。

### ステップ 1: 既存キューの削除

1. EMQX ダッシュボードの **Queues** に移動します。
2. トピックフィルターが `demo/topic` のキューを探します。
3. **Actions** 列の **Delete** をクリックします。
4. 確認ダイアログで削除を承認します。

これで前のキューと保存されていたメッセージが削除されます。

### ステップ 2: Last-Value Semantics を有効にしたキューの作成

1. **Queues** ページで **Create** をクリックします。
2. **Create Queue** ダイアログで以下を設定します。
   - **Name**: `my_queue`
   - **Topic Filter**: `device/config`
   - **Dispatch Strategy**: `Random`（または任意）
   - **Data Retention Period**: `7` 日
   - **Last Value Semantics**: 有効にする
   - **Queue Key Expression**: `message.from`（またはキーに使う任意のフィールド名）
3. **Create** をクリックします。

「Queue Key Expression」は、EMQX が各メッセージからキーを抽出し、Last-Value Queue 内で重複排除に使う方法を定義します。このフィールドは [Variform expressions](../../guides/configuration/configuration.md#variform-expressions) による設定をサポートしています。

このクイックスタートでは `message.from` を使い、メッセージパブリッシャーのクライアントIDをキーとして抽出しています。

> Queue Key Expression の高度な使い方やカスタムキー、メッセージ構造の詳細は [Queue Key Expression](./message-queue-task.md#queue-key-expression) を参照してください。

### ステップ 3: メッセージのパブリッシュ

1. MQTTX を開き、クライアント（例：`publisher`）を選択または作成します。

2. EMQX に接続します（`mqtt://localhost:1883`）。

3. `device/config` にメッセージをパブリッシュします。

   例：

   | フィールド   | 値                   |
   | ----------- | -------------------- |
   | **Topic**   | `device/config`      |
   | **QoS**     | 1                    |
   | **Payload** | `{"ssid": "wifi1"}`  |

4. 同じクライアント（同じクライアントID）で内容を更新したメッセージをパブリッシュします。

   ```json
   Payload: {"ssid": "wifi2"}
   ```

Queue Key Expression が `message.from` に設定されているため、EMQX は各メッセージからクライアントIDを自動的に抽出し、キューキーとして使用します。同じクライアントからのメッセージは、未消費の以前のメッセージを上書きします。

### ステップ 4: キューへのサブスクライブ

1. 2つ目の MQTTX クライアント（例：`subscriber`）を作成し、EMQX に接続します。

3. キュートピックにサブスクライブします。

   ```json
   Topic: $queue/my_queue/device/config
   QoS: 1
   ```

**期待される動作**：  
最新のメッセージのみが配信されます。この例では `{"ssid": "wifi2"}` のみが受信されます。
