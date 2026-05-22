# エージェントの管理

このページでは、A2Aレジストリの有効化方法、およびダッシュボードUI、CLI、MQTTを使用したエージェントの登録、表示、削除方法について説明します。

## 前提条件

- EMQX 6.2.0以降
- ダッシュボードまたはEMQXノードへの管理者アクセス権

## A2Aレジストリの有効化

A2Aレジストリはデフォルトで無効になっています。エージェントを登録する前に有効化してください。

### ダッシュボードからの有効化

1. 左側のナビゲーションパネルで **A2A Registry** をクリックします。
2. **Settings** をクリックします。
3. **Enable A2A Registry** をオンに切り替えます。
4. **Validate Schema** はデフォルトで有効です。有効にすると、EMQXは登録時にAgent CardのペイロードをA2Aスキーマに対して検証し、スキーマに準拠しないカードは拒否します。スキーマから逸脱したカードを受け入れる必要がある場合のみ無効にしてください。
5. **Save Changes** をクリックします。

### 設定ファイルからの有効化

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
| `enable` | Boolean | `false` | A2Aレジストリを有効化します。 |
| `validate_schema` | Boolean | `true` | 登録時にAgent CardのペイロードをA2Aスキーマに対して検証します。無効なカードは拒否されます。 |
| `max_card_size` | Integer | `65536` | Agent Cardペイロードの最大サイズ（バイト単位）。 |
| `registration_rate_limit` | Integer | `10` | エージェントごとの1分あたりの最大登録更新回数。 |
| `require_security_metadata` | Boolean | `false` | 有効にすると、Agent Cardのセキュリティメタデータ拡張に`jwksUri`を含める必要があります。 |
| `trusted_jkus` | Array | `[]` | 空でない場合、Agent Cardの`jwksUri`はリスト内のいずれかのプレフィックスと一致する必要があります。空リストはJKU検証を無効化（許容モード）します。 |
| `verify_jku_tls` | Boolean | `true` | JWKSエンドポイント取得時にTLS証明書を検証します。 |

## エージェントの登録

エージェントは自身のAgent CardをA2Aレジストリにパブリッシュすることで登録され、他のエージェントから検出可能になります。登録はダッシュボード、MQTT、CLIのいずれかで行えます。

### ダッシュボードからの登録

1. **A2A Registry** → **+ Register Agent** をクリックします。
2. 識別フィールドを入力します。

   - **Organization ID**：エージェントが所属する組織またはトラストドメイン。例：`com.example`。展開間の一意性を保つためにリバースDNS表記を推奨します。
   - **Unit ID**：組織内の部署や展開環境などの区分。例：`factory-a`。
   - **Agent ID**：組織・ユニット内で一意のエージェント識別子。例：`iot-ops-agent-001`。

   これら3つの値は英数字、ハイフン、アンダースコア、ピリオドのみを含み（`^[A-Za-z0-9._-]+$`）、`/`、`+`、`#`、空白は含められません。これらは `{org_id}/{unit_id}/{agent_id}` の形式でエージェントの完全なアドレスを構成します。

3. エディターにAgent CardのJSONを貼り付けます。必要なフィールドやテンプレートは **Help** ボタンで確認できます。
4. **Register Agent** をクリックします。

### MQTTXを使った登録

エージェントは自身のAgent Cardを保持メッセージとしてディスカバリートピックにパブリッシュして登録します。条件は以下の通りです。

- MQTTプロトコルバージョン5を使用
- クライアントIDは `{org_id}/{unit_id}/{agent_id}` に設定
- Retainフラグ有効、QoSは1
- ペイロードは少なくとも `name`、`description`、`version`、`url`、`skills` を含むAgent CardのJSON

**[MQTTX Desktop](https://mqttx.app/downloads) を使う場合：**

1. MQTTXを開き、**New Connection** をクリックします。
2. 接続情報を入力します。
   - **Name**：接続名（例：`IoT Operations Agent`）
   - **Host**：EMQXブローカーのアドレス
   - **Port**：`1883`（または適切なポート）
   - **Client ID**：`com.example/factory-a/iot-ops-agent-001`
   - **MQTT Version**：`5.0`
   
   <img src="./assets/register_agent_mqttx_connection.png" alt="register_agent_mqttx_connection" style="zoom:67%;" />
   
3. **Connect** をクリックします。
4. 画面下部のメッセージ作成エリアに以下を入力します。
   - **Topic**：`$a2a/v1/discovery/com.example/factory-a/iot-ops-agent-001`
   - **QoS**：`1`
   - **Retain**：有効
   - **Payload**：Agent CardのJSON（下記例参照）
5. 送信ボタンをクリックします。

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
      "description": "テレメトリを解析し、デバイスの異常を検出します。"
    }
  ]
}
```

<img src="./assets/register_agent_mqttx_send.png" alt="register_agent_mqttx_send" style="zoom:67%;" />

**[MQTTX CLI](https://mqttx.app/cli) を使う場合：**

```bash
mqttx pub \
  -h localhost -p 1883 \
  -V 5 \
  -i "com.example/factory-a/iot-ops-agent-001" \
  -t '$a2a/v1/discovery/com.example/factory-a/iot-ops-agent-001' \
  -m '{"name":"IoT Operations Agent","description":"工場のテレメトリを監視し、修復アクションを調整します。","version":"1.2.3","url":"mqtts://broker.example.com:8883","skills":[{"id":"device-diagnostics","name":"デバイス診断","description":"テレメトリを解析し、デバイスの異常を検出します。"}]}' \
  -q 1 -r
```

**Validate Schema** が有効な場合、EMQXはペイロードを登録前に検証し、不正なカードはPUBACKの理由コードで拒否されます。

### CLIを使った登録

```bash
emqx ctl a2a-registry register <path-to-agent-card.json>
```

JSONファイルにはAgent Cardのフィールドに加え、ルーティングに使用する識別フィールドを含める必要があります。

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
      "description": "テレメトリを解析し、デバイスの異常を検出します。"
    }
  ]
}
```

## 登録済みエージェントの表示

登録済みエージェントはダッシュボードで閲覧・検査でき、CLIからもクエリ可能です。

### ダッシュボードからの表示

**A2A Registry** ページに登録済みエージェントが一覧表示されます。各行には **Agent Card JSON** と **Delete** の2つの操作ボタンがあります。

画面上部の **Organization ID**、**Unit ID**、**Agent ID** のフィルターでリストを絞り込めます。

任意の行の **Agent Card JSON** をクリックすると、Agent Cardの生JSONがコピー用ボタン付きで表示されます。

![view_agent_via_dashboard](./assets/view_agent_via_dashboard.png)

### CLIからの表示

```bash
# 全エージェント一覧
emqx ctl a2a-registry list

# 組織とステータスで絞り込み
emqx ctl a2a-registry list --org com.example --status online

# 特定エージェントのAgent Cardを取得
emqx ctl a2a-registry get com.example factory-a iot-ops-agent-001

# レジストリ統計の表示
emqx ctl a2a-registry stats
```

## エージェントの削除

エージェントを削除するとA2Aレジストリから登録解除され、保持されたAgent Cardがクリアされるため、検出不可能になります。

### ダッシュボードからの削除

エージェント一覧で削除したいエージェントの削除操作をクリックし、確認画面で完全な `{org_id}/{unit_id}/{agent_id}` を入力して確定します。

### MQTTからの削除

エージェントのディスカバリートピックに空の保持メッセージをパブリッシュします。これにより保持カードがクリアされ、レジストリから削除されます。

**MQTTX Desktopを使う場合：**

1. エージェントのクライアントID（例：`com.example/factory-a/iot-ops-agent-001`）で接続します。
2. トピックに `$a2a/v1/discovery/com.example/factory-a/iot-ops-agent-001`、QoS `1`、Retain有効、ペイロードは空のまま設定します。
3. 送信ボタンをクリックします。

**MQTTX CLIを使う場合：**

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

## MQTTを使ったエージェントの検出

クライアントエージェントはワイルドカードを使ったディスカバリートピックをサブスクライブして利用可能なエージェントを検出します。カードは保持されているため、サブスクライブ時に即座に配信されます。

**MQTTX Desktopを使う場合：**

1. EMQXブローカーに接続します。
2. **+ New Subscription** をクリックし、ワイルドカードトピック（例：`$a2a/v1/discovery/com.example/+/+`）を入力して組織内の全エージェントを検出します。
3. **Confirm** をクリックします。保持されたAgent Cardがメッセージペインに即座に表示されます。

**MQTTX CLIを使う場合：**

```bash
# 組織内の全エージェント
mqttx sub -h localhost -p 1883 -V 5 -t '$a2a/v1/discovery/com.example/+/+' -v

# 特定ユニット内の全エージェント
mqttx sub -h localhost -p 1883 -V 5 -t '$a2a/v1/discovery/com.example/factory-a/+' -v

# 特定エージェント
mqttx sub -h localhost -p 1883 -V 5 -t '$a2a/v1/discovery/com.example/factory-a/iot-ops-agent-001' -v
```

`-v` フラグは受信したペイロードの前にトピック名を表示します。

受信メッセージのペイロードはAgent CardのJSONです。EMQXはMQTT v5のユーザープロパティとして以下を付与し、エージェントの生存状態を示します。

| ユーザープロパティ | 値 | 意味 |
|---|---|---|
| `a2a-status` | `online` | エージェントが現在接続中です。 |
| `a2a-status` | `offline` | エージェントが切断されました。 |
| `a2a-status-source` | `broker` | EMQXが接続状態に基づき設定したステータスです。 |
| `a2a-status-source` | `agent` | エージェント自身が積極的にパブリッシュしたステータス（例：正常なオフライン）です。 |
| `a2a-status-source` | `lwt` | Last Will and Testamentによる異常切断を反映したステータスです。 |
