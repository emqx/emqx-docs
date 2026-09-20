# Message Queue クイックスタート

このページでは、EMQX 6.0 の Message Queue 機能の使い方を説明します。MQTTX を使ってクライアントをシミュレートし、EMQX ダッシュボードからメッセージキューを作成・管理し、メッセージがどのように確実に保存・配信されるかを確認します。

## 目的

このクイックスタートでは、EMQX Message Queue が以下を実現できることを紹介します。

- サブスクライバーがオフラインでもメッセージを永続化する
- 設定可能な配信戦略をサポートする
- メッセージの圧縮における Last-Value セマンティクスを有効にする

## 前提条件

開始前に以下を準備してください。

- EMQX 6.0 以上が稼働している（Message Queue 機能が有効）
- [MQTTX](https://mqttx.app/)（または MQTT 5.0 対応クライアント）
- EMQX ダッシュボードへのアクセス（デフォルト: `http://localhost:18083`）

## Message Queue 基本機能のテスト

このセクションでは、EMQX Message Queue がメッセージを永続化し配信する様子を示します。MQTTX を使って MQTT クライアントをシミュレートし、サブスクライバーがオフラインの間もメッセージが保持・配信されることを確認します。

### ステップ 1: メッセージキューの作成

1. 左メニューの **Message Queue** に移動します。
2. ページ右上の **Create** ボタンをクリックします。

3. **Create Message Queue** ダイアログで以下の設定を行います。
   - **Topic Filter**: `demo/topic`
   - **Dispatch Strategy**: `Random`
   - **Data Retention Period**: `1` 日
   - **Last Value Semantics**: `Disabled`
4. **Create** をクリックします。

### ステップ 2: メッセージのパブリッシュ

MQTTX を使って **パブリッシャー** としてクライアントをシミュレートします。

1. MQTTX を開き、クライアントを作成します（例: `publisher`）。
2. EMQX に接続します（`mqtt://localhost:1883`）。
3. トピック `demo/topic` に QoS 1 でメッセージをパブリッシュします。

例:

```
Topic: demo/topic
QoS: 1
Payload: {"msg": "Hello 1"}
```

`{"msg": "Hello 2"}` など、ペイロードを変えて繰り返します。

この時点ではサブスクライバーがいません。メッセージは EMQX によってキューに入り永続化されます。

### ステップ 3: サブスクライブしてメッセージを消費

MQTTX を使って **サブスクライバー** としてクライアントをシミュレートします。

1. 2つ目のクライアントを開きます（例: `worker-a`）。
2. EMQX に接続します。
3. キュートピックをサブスクライブします。

   ```json
   Topic: $q/demo/topic
   QoS: 1
   ```

これでキューに溜まっていたすべてのメッセージを受信できます。

<img src="./assets/consume_message.png" alt="メッセージ消費画面" style="zoom:67%;" />

## 複数サブスクライバーと配信戦略のシミュレーション

このセクションでは、同じ Message Queue に複数のサブスクライバーが接続した場合の動作をシミュレートし、配信戦略がメッセージ分配に与える影響を確認します。

1. `publisher` クライアントで元のトピック（`$q/` プレフィックスなし）に複数のメッセージをパブリッシュします。例:

   ```bash
   for i in {1..10}; do
     mqttx pub -t demo/topic -m "message-$i" -q 1
   done
   ```

2. 別の MQTTX クライアントを作成します（例: `worker-b`）。
3. EMQX に接続し、同じキュートピックをサブスクライブします。

   ```json
   Topic: $q/demo/topic
   QoS: 1
   ```

これで `worker-a` と `worker-b` の両方が同じキューからメッセージを消費します。

4. 両サブスクライバーでメッセージの流れを観察します。

### 配信戦略が配信に与える影響

キューの **Dispatch Strategy** によってメッセージ分配の挙動が異なります。

| Dispatch Strategy           | 挙動                                                         | 利用ケース                             |
| --------------------------- | ------------------------------------------------------------ | ------------------------------------ |
| `Least Inflight Subscriber` | 未アック（未確認）メッセージ数が少ないサブスクライバーを優先 | 不均一な消費者間の負荷分散             |
| `Round Robin`               | サブスクライバーに順番にメッセージを配信                     | 速度に関係なく公平に分配               |
| `Random` (デフォルト)       | ランダムにサブスクライバーを選んでメッセージを送信           | 予測不能なシナリオやデモ用途           |

`worker-a` と `worker-b` のメッセージ受信状況を見て、これらの挙動を確認できます。

### 配信戦略の変更

配信戦略は動的に変更可能です。

1. ダッシュボードの **Message Queue** に移動します。
2. 対象のキューの **Edit** をクリックします。
3. 新しい **Dispatch Strategy** を選択し、保存します。

ただし、サブスクライバーがオンラインの間は新しい配信戦略は適用されません。クライアントを切断し、再接続してください。

切り替え後、再度メッセージをパブリッシュして、サブスクライバー間の配信パターンの違いを観察してください。

## Last-Value セマンティクスのテスト

このセクションでは、**Last-Value セマンティクス** を有効にする方法を示します。これは、キーごとに最新のメッセージのみをキューに保持し、デバイス設定の更新などに適しています。

### ステップ 1: 既存キューの削除

1. EMQX ダッシュボードの **Message Queue** に移動します。
2. トピックフィルターが `demo/topic` のキューを探します。
3. **Actions** 列の **Delete** をクリックします。
4. 確認ダイアログで削除を確定します。

これで以前のキューと保存されていたメッセージが削除されます。

### ステップ 2: Last-Value セマンティクス付きキューの作成

1. **Message Queue** ページで **Create** をクリックします。
2. **Create Message Queue** ダイアログで以下を設定します。
   - **Topic Filter**: `device/config`
   - **Dispatch Strategy**: `Random`（または任意）
   - **Data Retention Period**: `1` 日
   - **Last Value Semantics**: 有効にする
   - **Queue Key Expression**: `message.from`（またはキーとして使う任意のフィールド名）
3. **Create** をクリックします。

「Queue Key Expression」は、EMQX が各メッセージからキーを抽出し、Last-Value キューで重複排除に使う方法を定義します。このフィールドは [Variform 式](../../guides/configuration/configuration.md#variform-expressions) で設定可能です。

このクイックスタートでは `message.from` を使い、メッセージパブリッシャーのクライアントIDをキーとして抽出しています。

> Queue Key Expression の高度な使い方やカスタムキー、メッセージ構造の例は [Queue Key Expression](./message-queue-task.md#queue-key-expression) を参照してください。

### ステップ 3: メッセージのパブリッシュ

1. MQTTX を開き、クライアントを選択または作成します（例: `publisher`）。
2. EMQX に接続します（`mqtt://localhost:1883`）。
3. `device/config` にメッセージをパブリッシュします。

例:

| フィールド   | 値                 |
| ------------ | ------------------ |
| **Topic**    | `device/config`    |
| **QoS**      | 1                  |
| **Payload**  | `{"ssid": "wifi1"}` |

4. 同じクライアント（同じクライアントID）で内容を更新したメッセージをパブリッシュします。

```json
Payload: {"ssid": "wifi2"}
```

**Queue Key Expression** が `message.from` に設定されているため、EMQX は各メッセージからクライアントIDを抽出し、キューキーとして使用します。同じクライアントからのメッセージは、未消費のメッセージを上書きします。

### ステップ 4: キューのサブスクライブ

1. 2つ目の MQTTX クライアントを作成し（例: `subscriber`）、EMQX に接続します。
2. キュートピックをサブスクライブします。

```json
Topic: $q/device/config
QoS: 1
```

**期待される動作**:  
最新のメッセージのみが配信されます。この場合、`{"ssid": "wifi2"}` のみが受信されます。
