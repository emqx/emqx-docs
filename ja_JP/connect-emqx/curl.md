# EMQXでcurlを使う

curlはデータ転送や自動化に広く使われるコマンドラインツールです。2020年以降、curlはMQTTプロトコルをサポートしており、curl 8.19.0（2026年3月予定）以降ではMQTTS（TLS上のMQTT）もサポートしています。

curlを使うことで、開発者は言語固有のMQTTクライアントSDKをインストールせずに、コマンドラインから直接EMQXに接続し、メッセージのパブリッシュやトピックのサブスクライブが可能です。これにより、迅速なテスト、スクリプト作成、IoTプロトタイピングに便利な選択肢となります。

本ページでは、curlを使ったEMQXとのMQTTおよびMQTTS通信の接続、パブリッシュ／サブスクライブ、認証、TLS設定、よくあるトラブルシューティングについて解説します。

## curlのバージョン要件

| 機能               | 最低バージョン | リリース時期       |
| ------------------ | -------------- | ------------------ |
| MQTT (`mqtt://`)   | 7.70.0         | 2020年4月          |
| MQTTS (`mqtts://`) | 8.19.0         | 2026年3月初旬予定  |

インストール済みのcurlバージョンを確認するには：

```bash
curl --version
```

**Protocols** リストに `mqtt`（および curl ≥ 8.19.0 では `mqtts`）が含まれていることを確認してください。

> curlのバージョンが古い場合は、パッケージマネージャーでアップグレードするか、https://curl.se/download から最新版をダウンロードしてください。  
> macOSでは `brew install curl` で最新バージョンをインストールできます。

## MQTTブローカーのセットアップ

接続先のMQTTブローカーが必要です。本ガイドではMQTTおよびMQTTSをサポートするEMQXを使用します。

### EMQXパブリックブローカー（テスト用）

自身でブローカーをデプロイせずに素早くテストしたい場合、EMQXのパブリックブローカーを利用できます。

| パラメーター | 値                 |
| ------------ | ------------------ |
| ブローカー   | `broker.emqx.io`   |
| MQTTポート   | `1883`             |
| MQTTSポート  | `8883`             |

パブリックブローカーはテストおよびデモ目的のみの利用を想定しています。

### EMQXエンタープライズデプロイメント

本番環境では、curlを自身のEMQXエンタープライズ環境に接続します。ブローカーのアドレス、ポート、認証情報、TLS設定は環境に応じて設定してください。

一般的な構成例：

- カスタムブローカーホスト名またはIPアドレス
- EMQXエンタープライズで有効化されたMQTTおよび/またはMQTTSリスナー
- ユーザー名／パスワード認証、トークン認証、または相互TLS認証
- トピックに適用されるアクセス制御ルール（ACL）

curlコマンドを作成する際は、EMQXエンタープライズのリスナー、認証、TLS設定を参照してください。

**注意**

- 自身の環境でテストする場合は、すべての例で `broker.emqx.io` をEMQXエンタープライズのブローカーアドレスに置き換えてください。  
- 接続前に対応するMQTTまたはMQTTSリスナーがEMQXエンタープライズで有効になっていることを確認してください。

> 自身で管理するEMQXエンタープライズのほか、完全マネージドMQTTサービスである[EMQX Cloud](https://docs.emqx.com/en/cloud/latest/overview.html)（サーバーレスまたは専用）にもcurlで接続可能です。  
> curlのMQTT/MQTTS利用方法は同じで、EMQX Cloudが提供するブローカーアドレス、ポート、認証情報を使用してください。

### curlでEMQXエンタープライズに接続する

MQTTでは、クライアントはトピックのサブスクライブやメッセージのパブリッシュなどの操作の一環としてブローカーに接続します。独立した「接続」コマンドはありません。

curlでEMQXエンタープライズを利用する場合、サブスクライブまたはパブリッシュコマンドを実行すると、自動的に接続が確立されます（認証やTLS設定があればそれも適用されます）。

例えば、以下のコマンドはEMQXエンタープライズに接続し、トピックをサブスクライブします。

```bash
curl -N mqtts://your-enterprise-broker.example.com/curl/test
```

> **注意**：MQTTS（`mqtts://`）はcurl 8.19.0以降が必要です。curl 7.70.0〜8.18.xでは代わりに `mqtt://` を使用してください。

## curlのMQTT URLスキームの理解

curlはMQTT操作にURLベースの構文を使用します：

```
mqtt[s]://[user:password@]broker[:port]/topic
```

各要素の意味：

- `mqtt[s]`：プロトコル（`mqtt` または `mqtts`）
- `[user:password@]`：省略可能な認証情報
- `broker`：ブローカーのホスト名またはIPアドレス
- `[:port]`：省略可能なポート番号。指定しない場合はデフォルトを使用  
  - `mqtt://` の場合は `1883`  
  - `mqtts://` の場合は `8883`
- `/topic`：MQTTトピックパス（例：`/sensor/temperature`）

## curl MQTTの出力フォーマット

トピックをサブスクライブすると、curlは以下の形式で生のMQTTメッセージデータを出力します：

```
[2バイト: トピック長（ビッグエンディアン）] [トピック文字列] [ペイロード]
```

例えば、トピック `curl/test` に `"hello"` というメッセージが届くと：

```
curl/testhello
```

これはトピック名とペイロードが連結されたバイナリ形式であり、パースしないと読みづらいです。

この出力はバイナリであり、デフォルトでは人間が読みやすい形式ではありません。以下の[MQTTメッセージのパース](#parse-mqtt-messages)を参照してください。

## トピックのサブスクライブ

サブスクライブは接続を維持し、受信したメッセージを標準出力に表示します。

### 基本的なサブスクライブ（暗号化なし）

```bash
curl -N mqtt://broker.emqx.io/curl/test --output messages.bin
```

`-N` オプションは出力バッファリングを無効化し、メッセージを即時に表示します。

### MQTTSによる安全なサブスクライブ（curl ≥ 8.19.0）

```bash
curl -N mqtts://broker.emqx.io/curl/test --output messages.bin
```

### 認証付きサブスクライブ

```bash
curl -N -u "username:password" \
  mqtts://your-broker.emqxsl.com/curl/test --output messages.bin
```

## MQTTメッセージのパース

curlのサブスクライブ出力は**バイナリ形式**であり、整形されたテキスト形式ではありません。

各メッセージは以下の構造です：

- 2バイト：トピック長（ビッグエンディアン）
- トピック文字列
- ペイロード（生バイト）

そのため、出力はトピックとペイロードが連結された形で、人間が読みやすい形式ではありません。

### Bashワンライナー例

サブスクライブ出力を読みやすくするには、curlの出力をシェルでパースします：

```bash
curl -sN mqtt://broker.emqx.io/curl/test | \
  while IFS= read -r -d $'\0' d; do
    [ -z "$d" ] && continue

    # curlのMQTTサブスクライブ出力は2バイトのトピック長（MSB,LSB）、トピック、ペイロード。
    # このループはNUL(0x00)を区切り文字に使い、LSBを先に見てMSB=0と仮定（トピック長0〜255で動作）。
    lsb=$(printf "%d" "'${d:0:1}")
    topic_len=$((lsb))
    echo "[${d:1:$topic_len}] ${d:$((1 + topic_len))}"
  done
```

出力例：

```
[curl/test] hello
```

このパーサーは簡易的な方法で、デモ用途に適しています。

動作概要：

1. ストリームをNULバイトで分割  
2. 2バイトのトピック長の下位バイト（LSB）から長さを取得。上位バイト（MSB）は0と仮定（256バイト未満のトピックに対応）  
3. トピック文字列とペイロードを長さ情報で抽出  
4. 各メッセージを `[トピック] ペイロード` 形式で表示

### 生出力をファイルに保存して確認

MQTTメッセージのバイナリ構造を理解するために、生のサブスクライブ出力をファイルに保存し、手動で確認できます。

保存例：

```bash
curl -sN mqtt://broker.emqx.io/curl/test > messages.bin
```

`hexdump`で確認：

```bash
hexdump -C messages.bin
```

トピック長のプレフィックス、トピックバイト列、ペイロードのレイアウトを明確に確認できます。

### 再利用可能なシェル関数

繰り返し使う場合は、以下のようにシェル関数にまとめると便利です：

```bash
mqtt_subscribe() {
  curl -sN "$1" | while IFS= read -r -d $'\0' d; do
    [ -z "$d" ] && continue

    # curlのMQTTサブスクライブ出力は2バイトのトピック長（MSB,LSB）、トピック、ペイロード。
    # このループはNUL(0x00)を区切り文字に使い、LSBを先に見てMSB=0と仮定（トピック長0〜255で動作）。
    lsb=$(printf "%d" "'${d:0:1}")
    topic_len=$((lsb))
    echo "[${d:1:$topic_len}] ${d:$((1 + topic_len))}"
  done
}
```

使用例：

```bash
mqtt_subscribe "mqtt://broker.emqx.io/curl/test"
```

> 本番環境や複雑なパースが必要な場合は、[MQTTX CLI](https://mqttx.app/cli)の利用を検討してください。  
> MQTT 5.0完全対応、QoS処理、ワイルドカードサブスクライブなどの機能を備えています。

## メッセージのパブリッシュ

パブリッシュはcurlの `-d`（データ）オプションでペイロードを指定します。

### 基本的なパブリッシュ（暗号化なし）

```bash
curl -d "Hello from curl" \
  mqtt://broker.emqx.io/curl/test
```

### MQTTSによる安全なパブリッシュ（curl ≥ 8.19.0）

```bash
curl -d "Secure message from curl" \
  mqtts://broker.emqx.io/curl/test
```

### JSONペイロードのパブリッシュ

```bash
curl -d '{"sensor_id":"temp-001","value":23.5}' \
  mqtt://broker.emqx.io/sensors/temperature
```

### 認証付きパブリッシュ

```bash
curl -u "username:password" \
  -d '{"status":"online"}' \
  mqtts://your-broker.example.com/devices/status
```

## 主要なcurlオプション

本ドキュメントで使用するcurlコマンドの主なオプション一覧：

| オプション      | 説明                                         | 主な用途        |
| -------------- | -------------------------------------------- | --------------- |
| `-N`           | 出力バッファリングを無効化（サブスクライブに必須） | サブスクライブ  |
| `-d`           | パブリッシュするメッセージペイロード         | パブリッシュ    |
| `-u user:pass` | ユーザー名とパスワードによる認証             | 認証            |
| `-v`           | 詳細出力（MQTTハンドシェイクの確認に有用）   | トラブルシューティング |
| `-s`           | サイレントモード（進捗表示を抑制）           | スクリプト      |
| `--cacert`     | TLS検証用のCA証明書                           | MQTTS           |
| `--cert`       | 相互TLS用クライアント証明書                   | MQTTS           |
| `--key`        | 相互TLS用クライアント秘密鍵                     | MQTTS           |
| `-k`           | TLS検証をスキップ（テスト用のみ推奨）         | トラブルシューティング |

> curlの全オプションについては公式ドキュメントを参照してください。

## TLS設定（MQTTS）

### CA証明書による検証

```bash
curl --cacert /path/to/ca.crt \
  -d "TLS verified message" \
  mqtts://your-broker.example.com/secure/topic
```

### 相互TLS（mTLS）

```bash
curl --cacert /path/to/ca.crt \
  --cert /path/to/client.crt \
  --key /path/to/client.key \
  -d "mTLS message" \
  mqtts://your-broker.example.com/secure/topic
```

> 証明書検証をスキップするにはテスト目的のみで `-k` を使用してください。

## よくあるユースケース

### ブローカー接続テスト

```bash
curl -v mqtt://broker.emqx.io/curl/test
```

DNS解決、TCP接続、MQTTハンドシェイクを検証します。

**接続成功の目安**

詳細出力に以下が含まれます：

- ブローカーのホスト名がIPアドレスに解決される  
- ポート `1883`（MQTT）または `8883`（MQTTS）へのTCP接続成功  
- MQTTハンドシェイクがエラーなく完了

エラーがなくコマンドが正常終了すれば、EMQXブローカーへの接続が確立されています。

### シェルスクリプトやIoTプロトタイピング

例：5秒ごとに温度センサーのデータをパブリッシュするシミュレーション

```bash
#!/bin/bash
BROKER="mqtt://broker.emqx.io"
TOPIC="sensors/room1/temperature"

while true; do
  TEMP=$(awk -v min=20 -v max=30 'BEGIN{srand(); print min+rand()*(max-min)}')
  PAYLOAD="{\"temperature\": $TEMP, \"timestamp\": $(date +%s)}"
  curl -s -d "$PAYLOAD" "$BROKER/$TOPIC"
  sleep 5
done
```

## curlのMQTTにおける制限事項

curlはテストやスクリプト用途に便利ですが、以下の制限があります：

| 制限事項               | 説明                                 |
| ---------------------- | ------------------------------------ |
| QoS 0のみ対応          | QoS 1または2はサポートしない         |
| バイナリ出力           | サブスクライブ出力が整形されていない |
| ワイルドカード非対応   | `+` や `#` を使ったサブスクライブ不可 |
| 単一トピックのみ       | 1コマンドにつき1トピックのみ         |
| 永続セッションなし     | ステートレス接続                      |

高度なMQTT機能が必要な場合は、[MQTTX CLI](https://mqttx.app/cli)やEMQXクライアントSDKの利用を推奨します。

## curlでMQTTサポートを確認する

```bash
curl --version | grep -i mqtt
```

`mqtt`（およびcurl ≥ 8.19.0では `mqtts`）が出力に含まれていれば、MQTTサポート付きのcurlです。含まれていない場合は：

- curlを7.70.0以上にアップグレード  
- MQTT対応ビルドをインストール  
- `--enable-mqtt` オプション付きでソースからビルド

を検討してください。

## トラブルシューティング

curlとEMQXの利用でよくある問題と対処法を紹介します。

### 接続拒否またはタイムアウト

**症状**

- `Connection refused`  
- `Failed to connect to broker`  
- 接続がハングしてタイムアウト

**原因候補**

- ブローカーアドレスやポートの誤り  
- ネットワークファイアウォールがMQTT/MQTTSポートをブロック  
- ブローカーが起動していない、または指定ポートでリスニングしていない

**対処**

- ブローカーアドレスとポートを確認  
  - MQTT: `1883`  
  - MQTTS: `8883`  
- 詳細モードで接続確認：

```bash
curl -v mqtt://broker.emqx.io/curl/test
```

### curlがMQTTやMQTTSをサポートしていない

**症状**

- `Protocol "mqtt" not supported`  
- `Unknown protocol`

**原因候補**

- MQTTサポートなしでビルドされたcurl  
- curlのバージョンが古い

**対処**

- プロトコルサポートを確認：

```bash
curl --version
```

`mqtt`（およびTLS用の `mqtts`）が **Protocols** に含まれているか確認。

- curlをアップグレード、またはMQTT対応ビルドをインストール。

### TLSハンドシェイクや証明書エラー（MQTTS）

**症状**

- `SSL certificate problem`  
- `TLS handshake failed`  
- `Unable to get local issuer certificate`

**原因候補**

- CA証明書が不足または誤っている  
- ブローカーがプライベートまたは自己署名証明書を使用

**対処**

- CA証明書を明示的に指定：

```bash
curl --cacert /path/to/ca.crt \
  mqtts://your-broker.example.com/topic
```

- テスト目的で検証をスキップ（本番非推奨）：

```bash
curl -k mqtts://your-broker.example.com/topic
```

### サブスクライブしてもメッセージが受信できない

**症状**

- サブスクライブコマンドは実行されるが出力がない

**原因候補**

- 出力バッファリングが有効  
- トピックにメッセージがパブリッシュされていない  
- トピック名の不一致

**対処**

- サブスクライブ時は必ず `-N` を付ける：

```bash
curl -N mqtt://broker.emqx.io/curl/test
```

- メッセージが同じトピックにパブリッシュされているか確認。

### 認証失敗

**症状**

- 接続が即座に切断される  
- ブローカーのログに認証や認可エラー

**原因候補**

- ユーザー名またはパスワードの誤り  
- トピックに対するACL制限

**対処**

- 認証情報を確認：

```bash
curl -u "username:password" \
  mqtts://your-broker.example.com/topic
```

- EMQXの認証設定とACLを確認。

## さらに詳しく

curlを使ったMQTTおよびMQTTSの利用方法を、背景説明や追加例、利用上の注意点とともに詳しく解説したブログ記事はこちら：  
[Using curl for MQTT: Connect, Publish, and Subscribe with Secure IoT Communication](https://www.emqx.com/en/blog/using-curl-for-mqtt)

本ブログは本エンタープライズ向けガイドを補完し、より詳細な説明と拡張例を提供しています。
