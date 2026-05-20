# EMQXでcurlを使う

curlは広く使われているコマンドラインツールで、データ転送や自動化に利用されます。2020年以降、curlはMQTTプロトコルをサポートしており、curl 8.19.0（2026年3月予定）以降ではMQTTS（TLS上のMQTT）にも対応しています。

curlを使うことで、開発者は言語固有のMQTTクライアントSDKをインストールせずに、コマンドラインから直接EMQXに接続し、メッセージのパブリッシュやトピックのサブスクライブが可能です。これにより、迅速なテスト、スクリプト作成、IoTプロトタイピングに便利な選択肢となります。

本ページでは、EMQXとcurlを用いたMQTTおよびMQTTS通信の接続、パブリッシュ／サブスクライブ、認証、TLS設定、よくあるトラブルシューティングについて説明します。

## curlのバージョン要件

| 機能               | 最低バージョン | リリース時期       |
| ------------------ | -------------- | ------------------ |
| MQTT (`mqtt://`)   | 7.70.0         | 2020年4月          |
| MQTTS (`mqtts://`) | 8.19.0         | 2026年3月初旬予定  |

インストール済みのcurlバージョンを確認するには：

```bash
curl --version
```

**Protocols** リストに `mqtt`（およびcurl ≥ 8.19.0の場合は `mqtts`）が含まれていることを確認してください。

> curlのバージョンが古い場合は、パッケージマネージャーでアップグレードするか、https://curl.se/download から最新版をダウンロードしてください。
>
> macOSの場合は、`brew install curl` で最新バージョンをインストールできます。

## MQTTブローカーの準備

接続先となるMQTTブローカーが必要です。本ガイドではMQTTとMQTTSの両方をサポートするEMQXを使用します。

### EMQXパブリックブローカー（テスト用）

独自にブローカーを構築せずに手軽にテストしたい場合は、EMQXのパブリックブローカーを利用できます。

| パラメーター | 値                |
| ------------ | ----------------- |
| ブローカー   | `broker.emqx.io`  |
| MQTTポート   | `1883`            |
| MQTTSポート  | `8883`            |

パブリックブローカーはテストおよびデモ目的のみの利用を想定しています。

### EMQX Enterpriseのデプロイ環境

本番環境では、自身のEMQX Enterpriseデプロイメントにcurlを接続します。ブローカーのアドレス、ポート、認証情報、TLS設定は環境に応じて指定してください。

一般的な構成例：

- カスタムのブローカーホスト名またはIPアドレス
- EMQX Enterpriseで有効化されたMQTTおよび／またはMQTTSリスナー
- ユーザー名／パスワード認証、トークン認証、または相互TLS認証
- トピックに対するアクセス制御ルール（ACL）

curlコマンドを構築する際は、EMQX Enterpriseのリスナー設定、認証設定、TLS設定を参照してください。

**注意事項**

- すべての例で `broker.emqx.io` を自身のEMQX Enterpriseブローカーアドレスに置き換えてください。
- 接続前にEMQX Enterpriseで該当するMQTTまたはMQTTSリスナーが有効になっていることを確認してください。

> 自己管理型のEMQX Enterpriseに加え、完全マネージドMQTTサービスである[EMQX Cloud](https://docs.emqx.com/en/cloud/latest/)（ServerlessまたはDedicated）にもcurlで接続可能です。
>
> curlのMQTT／MQTTS利用方法は同様で、EMQX Cloudから提供されるブローカーアドレス、ポート、認証情報を使用してください。

### curlでEMQX Enterpriseに接続する

MQTTでは、クライアントはトピックのサブスクライブやメッセージのパブリッシュなどの操作の一環としてブローカーに接続します。独立した「接続」コマンドはありません。

curlでEMQX Enterpriseを利用する場合、サブスクライブやパブリッシュコマンドを実行すると、自動的に接続が確立されます。接続にはEnterpriseブローカーのアドレスや認証・TLS設定を指定します。

例として、以下のコマンドはEMQX Enterpriseに接続し、トピックをサブスクライブします。

```bash
curl -N mqtts://your-enterprise-broker.example.com/curl/test
```

> **注意**：MQTTS（`mqtts://`）はcurl 8.19.0以降が必要です。curl 7.70.0～8.18.xの場合は `mqtt://` を使用してください。

## curlのMQTT URLスキームの理解

curlはMQTT操作にURLベースの構文を使用します：

```
mqtt[s]://[user:password@]broker[:port]/topic
```

各要素の意味：

- `mqtt[s]`：プロトコル（`mqtt` または `mqtts`）
- `[user:password@]`：省略可能な認証情報
- `broker`：ブローカーのホスト名またはIPアドレス
- `[:port]`：省略可能なポート番号。指定しない場合はデフォルトが使用されます。
  - `mqtt://` の場合は `1883`
  - `mqtts://` の場合は `8883`
- `/topic`：MQTTのトピックパス（例：`/sensor/temperature`）

## curl MQTTの出力形式

トピックをサブスクライブすると、curlは生のMQTTメッセージデータを以下の形式で出力します：

```
[2バイト：トピック長（ビッグエンディアン）] [トピック文字列] [ペイロード]
```

例えば、トピック `curl/test` に `"hello"` というメッセージが届くと、

```
curl/testhello
```

のように表示されます。

この出力はバイナリ形式で、トピック名とペイロードが連結されているため、そのままでは読み取りにくいです。

読みやすく変換する方法は後述の[Parse MQTT Messages](#parse-mqtt-messages)を参照してください。

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

## MQTTメッセージの解析

curlのサブスクライブ出力は**バイナリ形式**であり、整形されたテキストではありません。

各メッセージは以下の構造です：

- 2バイト：トピック長（ビッグエンディアン）
- トピック文字列
- ペイロード（生データ）

そのため、出力はトピックとペイロードが連結された形となり、そのままでは人間が読み取れません。

### Bashワンライナー

サブスクライブ出力を読みやすくするには、curlの出力をシェルスクリプトでパースします：

```bash
curl -sN mqtt://broker.emqx.io/curl/test | \
  while IFS= read -r -d $'\0' d; do
    [ -z "$d" ] && continue

    # curlのMQTTサブスクライブ出力は、2バイトのトピック長（MSB,LSB）、トピック、ペイロード。
    # このループはNUL(0x00)を区切り文字に使い、LSBを先に読み、MSB=0と仮定（トピック長0～255に対応）。
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

1. ストリームをヌルバイトで分割
2. 2バイトのトピック長の下位バイトを取得（上位バイトは0と仮定）
3. トピック文字列とペイロードを長さに基づき抽出
4. `[トピック] ペイロード` の形式で表示

### 生データをファイルに保存して確認

MQTTメッセージのバイナリ構造を理解するために、生のサブスクライブ出力をファイルに保存し、手動で確認できます。

保存例：

```bash
curl -sN mqtt://broker.emqx.io/curl/test > messages.bin
```

`hexdump`で確認：

```bash
hexdump -C messages.bin
```

これにより、トピック長のプレフィックス、トピックバイト列、ペイロードの配置を明確に把握できます。

### 再利用可能なシェル関数

繰り返し使う場合は、パーサーをシェル関数にまとめられます：

```bash
mqtt_subscribe() {
  curl -sN "$1" | while IFS= read -r -d $'\0' d; do
    [ -z "$d" ] && continue

    # curlのMQTTサブスクライブ出力は、2バイトのトピック長（MSB,LSB）、トピック、ペイロード。
    # このループはNUL(0x00)を区切り文字に使い、LSBを先に読み、MSB=0と仮定（トピック長0～255に対応）。
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

> 本番利用や複雑な解析が必要な場合は、[MQTTX CLI](https://mqttx.app/cli)の利用を検討してください。MQTT 5.0完全対応、QoS処理、ワイルドカードサブスクライブなどが可能で、整形済みの出力を提供します。

## メッセージのパブリッシュ

メッセージをパブリッシュするには、curlの `-d`（データ）オプションにペイロードを指定します。

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

以下は本ドキュメントで使用しているcurlコマンドラインオプションのまとめです。

| オプション       | 説明                                         | 主な用途       |
| ---------------- | -------------------------------------------- | -------------- |
| `-N`             | 出力バッファリングを無効化（サブスクライブ時必須） | サブスクライブ |
| `-d`             | パブリッシュするメッセージペイロード         | パブリッシュ   |
| `-u user:pass`   | ユーザー名とパスワードによる認証             | 認証           |
| `-v`             | 詳細出力（MQTTハンドシェイクの確認に有用）   | トラブルシューティング |
| `-s`             | サイレントモード（進捗表示を抑制）           | スクリプト     |
| `--cacert`       | TLS検証用のCA証明書                           | MQTTS          |
| `--cert`         | 相互TLS用のクライアント証明書                 | MQTTS          |
| `--key`          | 相互TLS用のクライアント秘密鍵                 | MQTTS          |
| `-k`             | TLS検証をスキップ（テスト用途のみ推奨）       | トラブルシューティング |

> curlの全オプションについては、公式curlドキュメントを参照してください。

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

> 証明書検証をスキップするには `-k` を使用しますが、本番環境では推奨されません。

## よくある利用ケース

### ブローカー接続確認

```bash
curl -v mqtt://broker.emqx.io/curl/test
```

DNS解決、TCP接続、MQTTハンドシェイクの確認に使います。

**接続成功の目安**

成功すると、詳細出力に以下が表示されます：

- ブローカーのホスト名がIPアドレスに解決される
- ポート `1883`（MQTT）または `8883`（MQTTS）へのTCP接続が成功
- MQTTハンドシェイクがエラーなく完了

エラーが表示されず正常終了すれば、EMQXブローカーへの接続が確立しています。

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

## curlのMQTT利用における制限

curlはテストやスクリプト用途に便利ですが、以下の制限があります：

| 制限事項               | 説明                                  |
| ---------------------- | ------------------------------------- |
| QoS 0のみ対応          | QoS 1や2には対応していません          |
| バイナリ出力           | サブスクライブ出力は整形されていません |
| ワイルドカード非対応   | `+` や `#` を使ったサブスクライブ不可 |
| 単一トピックのみ対応  | 1コマンドで複数トピックの指定不可    |
| 永続セッション非対応  | ステートレスな接続のみ                 |

高度なMQTT機能が必要な場合は、[MQTTX CLI](https://mqttx.app/cli)やEMQXクライアントSDKの利用を推奨します。

## curlでMQTTサポートを確認する

```bash
curl --version | grep -i mqtt
```

`mqtt`（およびcurl ≥ 8.19.0の場合は `mqtts`）が表示されれば、MQTTサポート付きのcurlビルドです。表示されない場合は：

- curlを7.70.0以上にアップグレード
- MQTT対応ビルドをインストール
- ソースから `--enable-mqtt` オプション付きでコンパイル

を検討してください。

## トラブルシューティング

curlとEMQXを使う際のよくある問題と対処法を紹介します。

### 接続拒否やタイムアウト

**症状**

- `Connection refused`
- `Failed to connect to broker`
- 接続がハングしてタイムアウト

**原因候補**

- ブローカーアドレスやポートの誤り
- ネットワークファイアウォールによるMQTT/MQTTSポートの遮断
- ブローカーが起動していない、または指定ポートで待機していない

**対処**

- ブローカーアドレスとポートを確認
  - MQTT: `1883`
  - MQTTS: `8883`
- 詳細モードでネットワーク接続を確認：

```bash
curl -v mqtt://broker.emqx.io/curl/test
```

### curlでMQTTやMQTTSがサポートされていない

**症状**

- `Protocol "mqtt" not supported`
- `Unknown protocol`

**原因候補**

- MQTTサポートなしでビルドされたcurl
- 古いcurlバージョン

**対処**

- プロトコルサポートを確認：

```bash
curl --version
```

**Protocols** に `mqtt`（およびTLS用の `mqtts`）があるか確認。

- curlをアップグレード、またはMQTT対応ビルドをインストール。

### TLSハンドシェイクや証明書エラー（MQTTS）

**症状**

- `SSL certificate problem`
- `TLS handshake failed`
- `Unable to get local issuer certificate`

**原因候補**

- CA証明書の指定漏れや誤り
- ブローカーがプライベートまたは自己署名証明書を使用

**対処**

- CA証明書を明示的に指定：

```bash
curl --cacert /path/to/ca.crt \
  mqtts://your-broker.example.com/topic
```

- テストのみで証明書検証をスキップ（本番環境では非推奨）：

```bash
curl -k mqtts://your-broker.example.com/topic
```

### サブスクライブしてもメッセージが受信できない

**症状**

- サブスクライブコマンドは実行されるが出力なし

**原因候補**

- 出力バッファリングが有効
- トピックにメッセージがパブリッシュされていない
- トピック名の不一致

**対処**

- サブスクライブ時は必ず `-N` を使用：

```bash
curl -N mqtt://broker.emqx.io/curl/test
```

- メッセージが同じトピックにパブリッシュされているか確認。

### 認証失敗

**症状**

- 接続直後に切断される
- ブローカーのログに認証／認可エラー

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

curlを使ったMQTTおよびMQTTSの利用方法をステップバイステップで解説し、背景説明や追加例、利用上の注意点を含むブログ記事はこちら：

[Using curl for MQTT: Connect, Publish, and Subscribe with Secure IoT Communication](https://www.emqx.com/en/blog/using-curl-for-mqtt)

本ブログは本ガイド（Enterprise向け）を補完し、より詳細な解説と拡張例を提供しています。
