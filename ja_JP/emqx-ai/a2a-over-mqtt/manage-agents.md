# エージェントの管理

このページでは、A2Aレジストリの有効化方法と、Dashboard UI、CLI、またはMQTTを使用してエージェントを登録、表示、削除する方法について説明します。

## 前提条件

- EMQX 6.2.0以降。
- DashboardまたはEMQXノードへの管理者アクセス権。

## A2Aレジストリの有効化

A2Aレジストリはデフォルトで無効になっています。エージェントを登録する前に有効化してください。

### Dashboardからの操作

1. 左側のナビゲーションパネルで **A2A Registry** をクリックします。
2. **Settings** をクリックします。
3. **Enable A2A Registry** をオンに切り替えます。
4. **Validate Schema** はデフォルトで有効です。有効にすると、EMQXは登録時にAgent CardのペイロードをA2Aスキーマに対して検証し、スキーマに準拠しないカードを拒否します。スキーマから逸脱したカードを受け入れる必要がある場合のみ無効にしてください。
5. **Save Changes** をクリックします。

### 設定ファイルからの操作

`emqx.conf` に以下を追加します。

```hocon
a2a_registry {
  enable = true
  validate_schema = true
}
```

設定オプションの詳細：

| パラメータ | 型 | デフォルト | 説明 |
|---|---|---|---|
| `enable` | Boolean | `false` | A2Aレジストリを有効にします。 |
| `validate_schema` | Boolean | `true` | 登録時にAgent CardのペイロードをA2Aスキーマに対して検証し、不正なカードは拒否します。 |
| `max_card_size` | Integer | `65536` | Agent Cardペイロードの最大サイズ（バイト単位）。 |
| `registration_rate_limit` | Integer | `10` | エージェントごとの1分あたりの登録更新の最大数。 |
| `require_security_metadata` | Boolean | `false` | 有効にすると、Agent Cardのセキュリティメタデータ拡張に`jwksUri`の含有を必須とします。 |
| `trusted_jkus` | 配列 | `[]` | 空でない場合、Agent Card内の`jwksUri`はリストにあるいずれかのプレフィックスと一致する必要があります。空リストはJKU検証を無効化（許容モード）します。 |
| `verify_jku_tls` | Boolean | `true` | JWKSエンドポイント取得時にTLS証明書を検証します。 |

## エージェントの登録

エージェントは自身のAgent CardをA2Aレジストリにパブリッシュして登録し、他のエージェントから検出可能にします。登録はDashboard、MQTT、またはCLIから行えます。

### Dashboardからの登録

1. **A2A Registry** -> **+ Register Agent** をクリックします。
2. 識別フィールドを入力します：

   - **Organization ID**：エージェントが属する組織または信頼ドメイン。例：`com.example`。デプロイ間での一意性を保つためにリバースDNS表記を使用します。
   - **Unit ID**：組織内の部署やデプロイ環境などの区分。例：`factory-a`。
   - **Agent ID**：組織およびユニット内で一意のエージェント識別子。例：`iot-ops-agent-001`。

   3つの値はすべて英数字、ハイフン、アンダースコア、ピリオドのみ（`^[A-Za-z0-9._-]+$`）を含み、`/`、`+`、`#`、空白は含められません。これらは `{org_id}/{unit_id}/{agent_id}` の形式でエージェントの完全なアドレスを形成します。

3. エディターにAgent CardのJSONを貼り付けます。必要なフィールドやテンプレートは **Help** ボタンで確認できます。
4. **Register Agent** をクリックします。

### MQTTXを使用した登録

エージェントは自身のAgent Cardを保持メッセージとして発見トピックにパブリッシュして登録します。要件は以下の通りです：

- MQTTプロトコルバージョン5。
- クライアントIDは `{org_id}/{unit_id}/{agent_id}` に設定。
- Retainフラグ有効、QoS 1。
- ペイロードは `name`、`description`、`version`、`url`、`skills` を含むAgent Card JSON。

**[MQTTX Desktop](https://mqttx.app/downloads) を使用する場合：**

1. MQTTXを開き、**New Connection** をクリックします。

2. 接続情報を入力：
   - **Name**：接続名（例：`IoT Operations Agent`）
   - **Host**：EMQXブローカーのアドレス
   - **Port**：`1883`（または適切なポート）
   - **Client ID**：`com.example/factory-a/iot-ops-agent-001`
   - **MQTT Version**：`5.0`
   
   <img src="./assets/register_agent_mqttx_connection.png" alt="MQTTX接続設定画面" style="zoom:67%;" />
   
3. **Connect** をクリック。

4. 画面下部のメッセージ作成エリアに以下を入力：
   - **Topic**：`$a2a/v1/discovery/com.example/factory-a/iot-ops-agent-001`
   - **QoS**：`1`
   - **Retain**：有効
   - **Payload**：Agent Card JSON（以下の例参照）
   
5. 送信ボタンをクリック。

```json
{
  "name": "IoT Operations Agent",
  "description": "工場のテレメトリを監視し、修復アクションを調整します。",
  "version": "1.2.3",
  "url": "mqtts://broker.example.com:8883",
  "skills": [
    {
      "id": "device-diagnostics",
      "name": "デバイス診断",
      "description": "テレメトリを分析し、デバイスの異常を検出します。"
    }
  ]
}
```

<img src="./assets/register_agent_mqttx_send.png" alt="MQTTXメッセージ送信画面" style="zoom:67%;" />

**[MQTTX CLI](https://mqttx.app/cli) を使用する場合：**

```bash
mqttx pub \
  -h localhost -p 1883 \
  -V 5 \
  -i "com.example/factory-a/iot-ops-agent-001" \
  -t '$a2a/v1/discovery/com.example/factory-a/iot-ops-agent-001' \
  -m '{"name":"IoT Operations Agent","description":"工場のテレメトリを監視し、修復アクションを調整します。","version":"1.2.3","url":"mqtts://broker.example.com:8883","skills":[{"id":"device-diagnostics","name":"デバイス診断","description":"テレメトリを分析し、デバイスの異常を検出します。"}]}' \
  -q 1 -r
```

**Validate Schema** が有効な場合、EMQXはペイロードを登録前に検証し、不正なカードはPUBACKの理由コードで拒否されます。

### CLIからの登録

```bash
emqx ctl a2a-registry register <path-to-agent-card.json>
```

JSONファイルにはAgent Cardのフィールドとルーティングに使われる識別フィールドを含める必要があります：

```json
{
  "org_id": "com.example",
  "unit_id": "factory-a",
  "agent_id": "iot-ops-agent-001",
  "name": "IoT Operations Agent",
  "description": "工場のテレメトリを監視し、修復アクションを調整します。",
  "version": "1.2.3",
  "url": "mqtts://broker.example.com:8883",
  "skills": [
    {
      "id": "device-diagnostics",
      "name": "デバイス診断",
      "description": "テレメトリを分析し、デバイスの異常を検出します。"
    }
  ]
}
```

## 登録済みエージェントの表示

登録済みのエージェントはDashboardで閲覧・確認でき、CLIからもクエリ可能です。

### Dashboardからの表示

**A2A Registry** ページに登録済みエージェントが一覧表示されます。各行には **Agent Card JSON** と **Delete** の操作ボタンがあります。

上部の **Organization ID**、**Unit ID**、**Agent ID** のフィルターで絞り込みが可能です。

任意の行の **Agent Card JSON** をクリックすると、Agent Cardの生JSONがコピー可能な形式で表示されます。

![Dashboardでのエージェント表示](./assets/view_agent_via_dashboard.png)

### CLIからの表示

```bash
# すべてのエージェントを一覧表示
emqx ctl a2a-registry list

# 組織とステータスでフィルター
emqx ctl a2a-registry list --org com.example --status online

# 特定エージェントのAgent Cardを取得
emqx ctl a2a-registry get com.example factory-a iot-ops-agent-001

# レジストリ統計を表示
emqx ctl a2a-registry stats
```

## エージェントの削除

エージェントを削除するとA2Aレジストリから登録が解除され、保持されたAgent Cardがクリアされて検出不可になります。

### Dashboardからの削除

エージェント一覧で削除したいエージェントの削除ボタンをクリックし、確認のために完全な `{org_id}/{unit_id}/{agent_id}` を入力します。

### MQTTからの削除

エージェントの発見トピックに空の保持メッセージをパブリッシュします。これにより保持されたカードがクリアされ、レジストリから削除されます。

**MQTTX Desktopを使用する場合：**

1. エージェントのクライアントID（例：`com.example/factory-a/iot-ops-agent-001`）で接続します。
2. トピックを `$a2a/v1/discovery/com.example/factory-a/iot-ops-agent-001`、QoS `1`、**Retain** 有効に設定し、ペイロードは空にします。
3. 送信ボタンをクリックします。

**MQTTX CLIを使用する場合：**

```bash
mqttx pub \
  -h localhost -p 1883 \
  -V 5 \
  -i "com.example/factory-a/iot-ops-agent-001" \
  -t '$a2a/v1/discovery/com.example/factory-a/iot-ops-agent-001' \
  -m '' \
  -q 1 -r
```

### CLIからの削除

```bash
emqx ctl a2a-registry delete com.example factory-a iot-ops-agent-001
```

## MQTTによるエージェントの検出

クライアントエージェントはワイルドカードを使って発見トピックをサブスクライブし、利用可能なエージェントを検出します。カードは保持されているため、サブスクライブ直後に即座に配信されます。

**MQTTX Desktopを使用する場合：**

1. EMQXブローカーに接続します。
2. **+ New Subscription** をクリックし、ワイルドカードトピック（例：`$a2a/v1/discovery/com.example/+/+`）を入力して組織内のすべてのエージェントを検出します。
3. **Confirm** をクリック。保持されたAgent Cardがメッセージペインに即座に表示されます。

**MQTTX CLIを使用する場合：**

```bash
# 組織内のすべてのエージェント
mqttx sub -h localhost -p 1883 -V 5 -t '$a2a/v1/discovery/com.example/+/+' -v

# 特定ユニット内のすべてのエージェント
mqttx sub -h localhost -p 1883 -V 5 -t '$a2a/v1/discovery/com.example/factory-a/+' -v

# 特定エージェント
mqttx sub -h localhost -p 1883 -V 5 -t '$a2a/v1/discovery/com.example/factory-a/iot-ops-agent-001' -v
```

`-v` フラグは受信したペイロードの前にトピック名を表示します。

受信メッセージのペイロードにはAgent Card JSONが含まれます。EMQXはMQTT v5のユーザープロパティとして以下を付加し、エージェントのライブネスを示します：

| ユーザープロパティ | 値 | 意味 |
|---|---|---|
| `a2a-status` | `online` | エージェントが現在接続中。 |
| `a2a-status` | `offline` | エージェントが切断済み。 |
| `a2a-status-source` | `broker` | EMQXが接続状態に基づき設定。 |
| `a2a-status-source` | `agent` | エージェント自身が積極的にパブリッシュ（例：正常なオフライン）。 |
| `a2a-status-source` | `lwt` | Last Will and Testamentによる異常切断を反映。 |
