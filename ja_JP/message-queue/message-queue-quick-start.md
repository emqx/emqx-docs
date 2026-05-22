# Message Queue クイックスタート

<<<<<<< HEAD
このページでは、Message Queue 機能の簡単な使い方を説明します。MQTTX を使ってクライアントをシミュレートし、EMQX ダッシュボードからメッセージキューを作成・管理し、メッセージがどのように確実に保存および配信されるかを確認します。
=======
このページでは、EMQX 6.0 の Message Queue 機能の使い方を説明します。MQTTX を使ってクライアントをシミュレートし、EMQX ダッシュボードからメッセージキューを作成・管理し、メッセージがどのように確実に保存・配信されるかを確認します。
>>>>>>> origin/release-6.1

## 目的

このクイックスタートでは、EMQX Message Queue が以下を実現できることを紹介します。

- サブスクライバーがオフラインでもメッセージを永続化する
- 設定可能なディスパッチ戦略をサポートする
<<<<<<< HEAD
- メッセージ圧縮のためのラストバリューセマンティクスを有効にする
=======
- メッセージ圧縮のための Last-Value Semantics を有効にする
>>>>>>> origin/release-6.1

## 前提条件

開始する前に、以下を準備してください。

- EMQX 6.0 以上が稼働していること（Message Queue 機能が有効）
- [MQTTX](https://mqttx.app/)（または MQTT 5.0 対応のクライアント）
- EMQX ダッシュボードへのアクセス（デフォルト：`http://localhost:18083`）

## Message Queue 基本機能のテスト

<<<<<<< HEAD
このセクションでは、EMQX Message Queue がメッセージをどのように永続化し配信するかを示します。MQTTX を使ってクライアントをシミュレートし、サブスクライバーがオフラインでもメッセージが保持され配信される様子を確認します。

### ステップ 1: キューの作成

1. 左メニューの **Queues** に移動します。
2. ページ右上の **Create** ボタンをクリックします。

3. **Create Queue** ダイアログで以下の設定を行います。
   - **Name**: `my_queue`
   - **Topic Filter**: `demo/topic`
   - **Dispatch Strategy**: `Random`
   - **Data Retention Period**: `7` 日
=======
このセクションでは、EMQX Message Queue がメッセージをどのように永続化し配信するかを示します。MQTTX でクライアントをシミュレートし、サブスクライバーがオフラインでもメッセージが保持・配信される様子を確認します。

### ステップ 1: メッセージキューの作成

1. 左メニューの **Message Queue** に移動します。
2. 画面右上の **Create** ボタンをクリックします。

3. **Create Message Queue** ダイアログで以下の設定を行います。
   - **Topic Filter**: `demo/topic`
   - **Dispatch Strategy**: `Random`
   - **Data Retention Period**: `1` 日
>>>>>>> origin/release-6.1
   - **Last Value Semantics**: `Disabled`
4. **Create** をクリックします。

### ステップ 2: メッセージのパブリッシュ

MQTTX を使って **パブリッシャー** としてクライアントをシミュレートします。

<<<<<<< HEAD
1. MQTTX を開き、クライアント（例：`publisher`）を作成します。
=======
1. MQTTX を開き、クライアントを作成します（例：`publisher`）。
>>>>>>> origin/release-6.1
2. EMQX に接続します（`mqtt://localhost:1883`）。
3. トピック `demo/topic` に QoS 1 でメッセージをパブリッシュします。

例：

```
Topic: demo/topic
QoS: 1
Payload: {"msg": "Hello 1"}
```

`{"msg": "Hello 2"}` など、ペイロードを変えて複数回繰り返します。

この時点ではサブスクライバーがいません。メッセージは EMQX によってキューに保存され永続化されます。

### ステップ 3: サブスクライブしてメッセージを消費

MQTTX を使って **サブスクライバー** としてクライアントをシミュレートします。

<<<<<<< HEAD
1. 2つ目のクライアント（例：`worker-a`）を開きます。

2. EMQX に接続します。

=======
1. 2つ目のクライアントを開きます（例：`worker-a`）。
2. EMQX に接続します。
>>>>>>> origin/release-6.1
3. キュートピックにサブスクライブします。

   ```json
   Topic: $queue/my_queue/demo/topic
   QoS: 1
   ```

これで、キューに保存されていたすべてのメッセージを受信できます。

<<<<<<< HEAD
<img src="./assets/consume_message.png" alt="メッセージを消費する様子" style="zoom:67%;" />

## 複数サブスクライバーのシミュレーションとディスパッチ戦略

このセクションでは、同じ Message Queue に複数のサブスクライバーが接続している状況をシミュレートし、異なるディスパッチ戦略がメッセージ配信にどのように影響するかを確認します。

1. `publisher` クライアントで、元のトピック（`$queue/` プレフィックスなし）に複数のメッセージをパブリッシュします。
=======
<img src="./assets/consume_message.png" alt="メッセージ消費画面" style="zoom:67%;" />

## 複数サブスクライバーとディスパッチ戦略のシミュレーション

このセクションでは、同じメッセージキューに複数のサブスクライバーが接続した場合の動作をシミュレートし、異なるディスパッチ戦略がメッセージ配信にどのように影響するかを確認します。

1. `publisher` クライアントで、元のトピック（`$q/` プレフィックスなし）に複数のメッセージをパブリッシュします。
>>>>>>> origin/release-6.1

   ```bash
   for i in {1..10}; do
     mqttx pub -t demo/topic -m "message-$i" -q 1
   done
   ```

<<<<<<< HEAD
2. もう一つ MQTTX クライアント（例：`worker-b`）を作成します。

=======
2. もう1つ MQTTX クライアントを作成します（例：`worker-b`）。
>>>>>>> origin/release-6.1
3. EMQX に接続し、同じキュートピックにサブスクライブします。

   ```json
   Topic: $queue/my_queue/demo/topic
   QoS: 1
   ```

<<<<<<< HEAD
   これで `worker-a` と `worker-b` の両方が同じキューからメッセージを消費しています。
=======
これで、`worker-a` と `worker-b` の両方が同じキューからメッセージを消費します。
>>>>>>> origin/release-6.1

4. 両方のサブスクライバーでメッセージの流れを観察します。

### ディスパッチ戦略が配信に与える影響

キューの **Dispatch Strategy** によってメッセージ配信の挙動が変わります。

| Dispatch Strategy           | 挙動                                                         | 利用ケース                             |
| --------------------------- | ------------------------------------------------------------ | ------------------------------------ |
<<<<<<< HEAD
| `Least Inflight Subscriber` | 未アック（未確認）メッセージが少ないサブスクライバーを優先 | 不均一な消費者間でのロードバランシング |
| `Round Robin`               | 各サブスクライバーに順番にメッセージを配信                   | 速度に関係なく公平に配信したい場合   |
| `Random` (デフォルト)       | ランダムに選ばれたサブスクライバーにメッセージを送信         | 予測不能な配信やデモ用途             |

`worker-a` と `worker-b` へのメッセージ配信の様子を見て、これらの挙動を確認できます。
=======
| `Least Inflight Subscriber` | 未アック（未確認）メッセージが少ないサブスクライバーを優先 | 不均一な消費者間の負荷分散             |
| `Round Robin`               | サブスクライバーに順番に交互にメッセージを配信               | 速度に関係なく公平に配信したい場合     |
| `Random` (デフォルト)       | ランダムに選ばれたサブスクライバーにメッセージを送信         | 予測不能な動作やデモシナリオ           |

`worker-a` と `worker-b` のメッセージ配信状況を観察して、これらの挙動を確認できます。
>>>>>>> origin/release-6.1

### ディスパッチ戦略の変更

戦略は動的に変更可能です。

<<<<<<< HEAD
1. ダッシュボードの **Queues** に移動します。
2. 対象のキューの **Edit** をクリックします。
3. 新しい **Dispatch Strategy** を選択して保存します。

ただし、サブスクライバーがオンラインの間は新しい戦略は適用されません。クライアントを切断して再接続する必要があります。

切り替え後に再度メッセージをパブリッシュし、サブスクライバー間の配信パターンの違いを観察してください。

## ラストバリューセマンティクスのテスト

このセクションでは、**ラストバリューセマンティクス**を有効にし、キーごとに最新のメッセージのみをキューに保持する方法を示します。これはデバイス設定の更新などに適しています。

### ステップ 1: 既存キューの削除

1. EMQX ダッシュボードの **Queues** に移動します。
=======
1. ダッシュボードの **Message Queue** に移動します。
2. 対象のキューの横にある **Edit** をクリックします。
3. 新しい **Dispatch Strategy** を選択し、保存します。

ただし、サブスクライバーがオンラインで接続中の場合は新しい戦略は適用されません。クライアントを切断し、再接続する必要があります。

切り替え後に再度メッセージをパブリッシュし、サブスクライバー間の配信パターンの違いを確認してください。

## Last-Value Semantics のテスト

このセクションでは、**Last-Value Semantics** を有効にする方法を示します。これは、キーごとに最新のメッセージのみをキューに保持し、デバイス設定の更新などに適した機能です。

### ステップ 1: 既存キューの削除

1. EMQX ダッシュボードの **Message Queue** に移動します。
>>>>>>> origin/release-6.1
2. トピックフィルターが `demo/topic` のキューを探します。
3. **Actions** 列の **Delete** をクリックします。
4. 確認ダイアログで削除を確定します。

これで以前のキューと保存されていたメッセージが削除されます。

<<<<<<< HEAD
### ステップ 2: ラストバリューセマンティクスを有効にしたキューの作成

1. **Queues** ページで **Create** をクリックします。
2. **Create Queue** ダイアログで以下を設定します。
   - **Name**: `my_queue`
   - **Topic Filter**: `device/config`
   - **Dispatch Strategy**: `Random`（または任意）
   - **Data Retention Period**: `7` 日
   - **Last Value Semantics**: 有効にする
   - **Queue Key Expression**: `message.from`（またはキーとして使う任意のフィールド名）
3. **Create** をクリックします。

「Queue Key Expression」は、EMQX が各メッセージからキーを抽出し、ラストバリューキューで重複排除に使う方法を定義します。このフィールドは [Variform 式](../configuration/configuration.md#variform-expressions)で設定可能です。

このクイックスタートでは `message.from` を使い、メッセージパブリッシャーのクライアントIDをキーとして抽出します。

> キューキー式の高度な使い方やカスタムキー、メッセージ構造の詳細は [Queue Key Expression](./message-queue-task.md#queue-key-expression) を参照してください。

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

4. 同じクライアントIDで内容を更新したメッセージを再度パブリッシュします。
=======
### ステップ 2: Last-Value Semantics を有効にしたキューの作成

1. **Message Queue** ページで **Create** をクリックします。
2. **Create Message Queue** ダイアログで以下を設定します。
   - **Topic Filter**: `device/config`
   - **Dispatch Strategy**: `Random`（または任意）
   - **Data Retention Period**: `1` 日
   - **Last Value Semantics**: 有効にする（トグルオン）
   - **Queue Key Expression**: `message.from`（またはキーとして使う任意のフィールド名）
3. **Create** をクリックします。

「Queue Key Expression」は、EMQX がメッセージからキーを抽出し、Last-Value Queue での重複排除に使う方法を定義します。このフィールドは [Variform expressions](../configuration/configuration.md#variform-expressions) による設定をサポートしています。

このクイックスタートでは `message.from` を使い、メッセージのパブリッシャーのクライアントIDをキーとして抽出しています。

> Queue Key Expression の高度な使い方やカスタムキー、メッセージ構造の例については、[Queue Key Expression](./message-queue-task.md#queue-key-expression) を参照してください。

### ステップ 3: メッセージのパブリッシュ

1. MQTTX を開き、クライアントを選択または作成します（例：`publisher`）。
2. EMQX に接続します（`mqtt://localhost:1883`）。
3. `device/config` にメッセージをパブリッシュします。

例：

| フィールド   | 値                   |
| ------------ | -------------------- |
| **Topic**    | `device/config`      |
| **QoS**      | 1                    |
| **Payload**  | `{"ssid": "wifi1"}`  |

4. 同じクライアント（同じクライアントID）で内容を更新したメッセージをパブリッシュします。
>>>>>>> origin/release-6.1

```json
Payload: {"ssid": "wifi2"}
```

<<<<<<< HEAD
**Queue Key Expression** が `message.from` に設定されているため、EMQX は各メッセージからクライアントIDを自動抽出し、同じクライアントからの未消費メッセージを上書きします。

### ステップ 4: キューへのサブスクライブ

1. 2つ目の MQTTX クライアント（例：`subscriber`）を作成し、EMQX に接続します。

2. キュートピックにサブスクライブします。

   ```json
   Topic: $queue/my_queue/device/config
   QoS: 1
   ```
=======
**Queue Key Expression** が `message.from` に設定されているため、EMQX は各メッセージからクライアントIDを自動的に抽出し、それをキューキーとして使用します。同じクライアントからのメッセージは、未消費の前のメッセージを上書きします。

### ステップ 4: キューへのサブスクライブ

1. 2つ目の MQTTX クライアントを作成します（例：`subscriber`）および EMQX に接続します。
2. キュートピックにサブスクライブします。

```json
Topic: $q/device/config
QoS: 1
```
>>>>>>> origin/release-6.1

**期待される動作**：  
最新のメッセージのみが配信されます。この場合、`{"ssid": "wifi2"}` のみが受信されます。
