# EMQXでcurlを使う

curlはデータ転送や自動化に広く使われているコマンドラインツールです。2020年以降、curlはMQTTプロトコルをサポートしており、curl 8.19.0（2026年3月予定）以降ではMQTTS（TLS上のMQTT）もサポートされます。

curlを使うことで、開発者は言語固有のMQTTクライアントSDKをインストールすることなく、コマンドラインから直接EMQXに接続し、メッセージのパブリッシュやトピックのサブスクライブが可能です。これにより、簡単なテスト、スクリプト作成、IoTプロトタイピングに便利な選択肢となります。

本ページでは、EMQXとcurlを使ったMQTTおよびMQTTS通信の接続、パブリッシュ／サブスクライブ、認証、TLS設定、よくあるトラブルシューティングについて解説します。

## curlのバージョン要件

| 機能               | 最低バージョン | リリース時期        |
| ------------------ | -------------- | ------------------- |
| MQTT (`mqtt://`)   | 7.70.0         | 2020年4月           |
| MQTTS (`mqtts://`) | 8.19.0         | 2026年3月初旬予定   |

インストール済みのcurlバージョンを確認するには：

```bash
curl --version
```

**Protocols**リストに`mqtt`（curl ≥ 8.19.0の場合は`mqtts`も）と表示されていることを確認してください。

> curlのバージョンが古い場合は、パッケージマネージャーでアップグレードするか、https://curl.se/download から最新版をダウンロードしてください。  
> macOSでは`brew install curl`で最新バージョンをインストールできます。

## MQTTブローカーのセットアップ

接続先となるMQTTブローカーが必要です。本ガイドではMQTTとMQTTSの両方をサポートするEMQXを使用します。

### EMQXパブリックブローカー（テスト用）

自分でブローカーをデプロイせずに手軽にテストしたい場合は、EMQXのパブリックブローカーを利用できます。

| パラメータ | 値                 |
| ---------- | ------------------ |
| ブローカー | `broker.emqx.io`   |
| MQTTポート | `1883`             |
| MQTTSポート| `8883`             |

パブリックブローカーはテストおよびデモ目的のみの利用を想定しています。

### EMQX Enterpriseデプロイメント

本番環境では、curlを自分のEMQX Enterpriseデプロイメントに接続します。ブローカーのアドレス、ポート、認証情報、TLS設定は環境に応じて設定してください。

一般的な構成例：

- 独自のブローカーホスト名またはIPアドレス
- EMQX Enterpriseで有効化されたMQTTおよび/またはMQTTSリスナー
- ユーザー名／パスワード認証、トークン認証、相互TLS認証
- トピックに対するアクセス制御ルール（ACL）

curlコマンドを作成する際は、EMQX Enterpriseのリスナー、認証、TLS設定を参照してください。

**注意**

- 例示の`broker.emqx.io`は自分のEMQX Enterpriseブローカーアドレスに置き換えてください。
- 接続前にEMQX Enterpriseで対応するMQTTまたはMQTTSリスナーが有効になっていることを確認してください。

> 自己管理のEMQX Enterpriseに加え、完全マネージドMQTTサービスである[EMQX Cloud](https://docs.emqx.com/en/cloud/latest/)（ServerlessまたはDedicated）にもcurlで接続可能です。  
> curlのMQTT/MQTTS利用方法は同じで、EMQX Cloudが提供するブローカーアドレス、ポート、認証情報を使用してください。

### curlでEMQX Enterpriseに接続する

MQTTでは、クライアントはトピックのサブスクライブやメッセージのパブリッシュなどの操作の一環としてブローカーに接続します。独立した「接続」コマンドはありません。

curlでEMQX Enterpriseを利用する場合、サブスクライブやパブリッシュコマンドを実行すると、指定したEnterpriseブローカーアドレス（認証やTLS設定があればそれも含む）に自動的に接続されます。

例えば、以下のコマンドはEMQX Enterpriseに接続し、トピックをサブスクライブします：

```bash
curl -N mqtts://your-enterprise-broker.example.com/curl/test
```

> **注意**：MQTTS（`mqtts://`）はcurl 8.19.0以降が必要です。curl 7.70.0〜8.18.xの場合は`mqtt://`を使用してください。

## curlのMQTT URLスキームの理解

curlはMQTT操作にURLベースの構文を用います：

```
mqtt[s]://[user:password@]broker[:port]/topic
```

各要素の意味：

- `mqtt[s]`：プロトコル（`mqtt`または`mqtts`）
- `[user:password@]`：省略可能な認証情報
- `broker`：ブローカーのホスト名またはIPアドレス
- `[:port]`：省略可能なポート番号。指定がない場合はデフォルトを使用：
  - `1883`（`mqtt://`の場合）
  - `8883`（`mqtts://`の場合）
- `/topic`：MQTTトピックパス（例：`/sensor/temperature`）

## curlのMQTT出力フォーマット

トピックをサブスクライブすると、curlは次の形式で生のMQTTメッセージデータを出力します：

```
[2バイト：トピック長（ビッグエンディアン）] [トピック文字列] [ペイロード]
```

例えば、トピック`curl/test`にメッセージ `"hello"` が届くと、以下のように表示されます：

```
curl/testhello
```

この出力はトピック名とペイロードが連結されたバイナリ形式であり、パースしないと読みづらいです。

詳しくは後述の[MQTTメッセージのパース](#parse-mqtt-messages)をご覧ください。

## トピックのサブスクライブ

サブスクライブは接続を維持し、受信したメッセージを`stdout`に出力します。

### 基本的なサブスクライブ（暗号化なし）

```bash
curl -N mqtt://broker.emqx.io/curl/test --output messages.bin
```

`-N`オプションは出力のバッファリングを無効化し、メッセージを即時に表示します。

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

curlはサブスクライブ時にMQTTメッセージを**バイナリ形式**で出力します。整形されたテキスト形式ではありません。

各メッセージは以下の構造です：

- 2バイト：トピック長（ビッグエンディアン）
- トピック文字列
- ペイロード（生バイト）

そのため、出力はトピックとペイロードが連結された形で、人間が読みやすい形式ではありません。

### Bashワンライナー

サブスクライブ出力を読みやすくするには、curlの出力をシェルスクリプトでパースします：

```bash
curl -sN mqtt://broker.emqx.io/curl/test | \
  while IFS= read -r -d $'\0' d; do
    [ -z "$d" ] && continue

    # curlのMQTTサブスクライブ出力は、2バイトのトピック長（MSB,LSB）、トピック、ペイロード。
    # このループはNUL(0x00)を区切り文字として使うため、LSBを先に見ており、
    # MSB=0と暗黙的に仮定（トピック長0〜255で動作）。
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
3. トピック文字列とペイロードを長さ指定で抽出
4. `[トピック] ペイロード`形式で表示

### 生データを保存して確認

MQTTメッセージのバイナリ構造を理解するため、生のサブスクライブ出力をファイルに保存し、手動で確認できます。

保存例：

```bash
curl -sN mqtt://broker.emqx.io/curl/test > messages.bin
```

`hexdump`で内容を確認：

```bash
hexdump -C messages.bin
```

トピック長のプレフィックス、トピックバイト、ペイロードの配置が明確に見えます。

### 再利用可能なシェル関数

繰り返し使う場合は、以下のように関数化できます：

```bash
mqtt_subscribe() {
  curl -sN "$1" | while IFS= read -r -d $'\0' d; do
    [ -z "$d" ] && continue

    # curlのMQTTサブスクライブ出力は、2バイトのトピック長（MSB,LSB）、トピック、ペイロード。
    # このループはNUL(0x00)を区切り文字として使うため、LSBを先に見ており、
    # MSB=0と暗黙的に仮定（トピック長0〜255で動作）。
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

> 本番利用や複雑なパースが必要な場合は、[MQTTX CLI](https://mqttx.app/cli)の利用を検討してください。  
> こちらは整形済み出力、MQTT 5.0完全対応、QoS処理、ワイルドカードサブスクライブをサポートしています。

## メッセージのパブリッシュ

パブリッシュはcurlの`-d`（データ）オプションでペイロードを指定します。

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

以下は本ドキュメントで使用するcurlコマンドラインオプションのまとめです。

| オプション       | 説明                                         | 主な用途           |
| ---------------- | -------------------------------------------- | ------------------ |
| `-N`             | 出力バッファリングを無効化（サブスクライブ時必須） | サブスクライブ     |
| `-d`             | パブリッシュするメッセージペイロード         | パブリッシュ       |
| `-u user:pass`   | ユーザー名とパスワードによる認証             | 認証               |
| `-v`             | 詳細出力（MQTTハンドシェイク表示）           | トラブルシューティング |
| `-s`             | サイレントモード（進捗表示抑制）             | スクリプト         |
| `--cacert`       | TLS検証用のCA証明書                           | MQTTS              |
| `--cert`         | 相互TLS用のクライアント証明書                 | MQTTS              |
| `--key`          | 相互TLS用のクライアント秘密鍵                 | MQTTS              |
| `-k`             | TLS検証をスキップ（テスト用のみ推奨）         | トラブルシューティング |

> curlの全オプションについては公式ドキュメントをご参照ください。

## TLS設定（MQTTS）

### CA証明書検証

```bash
curl --cacert /path/to/ca.crt \
  -d "TLS verified message" \
  mqtts://your-broker.example.com/secure/topic
```

### 相互TLS認証（mTLS）

```bash
curl --cacert /path/to/ca.crt \
  --cert /path/to/client.crt \
  --key /path/to/client.key \
  -d "mTLS message" \
  mqtts://your-broker.example.com/secure/topic
```

> 証明書検証をスキップする場合は`-k`を使用しますが、本番環境では推奨されません。

## よくあるユースケース

### ブローカー接続テスト

```bash
curl -v mqtt://broker.emqx.io/curl/test
```

DNS解決、TCP接続、MQTTハンドシェイクを検証します。

**接続成功の目安**

- ブローカーのホスト名が1つ以上のIPアドレスに解決される
- ポート`1883`（MQTT）または`8883`（MQTTS）へのTCP接続が成功する
- MQTTハンドシェイクがエラーなく完了する

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
| QoS 0のみ対応          | QoS 1やQoS 2はサポートしていない      |
| バイナリ出力           | サブスクライブ出力は整形されていない  |
| ワイルドカード非対応   | `+`や`#`によるサブスクライブ不可      |
| 単一トピックのみ       | 1コマンドで1トピックのみ対応          |
| 永続セッションなし     | ステートレスな接続                    |

高度なMQTT機能が必要な場合は、[MQTTX CLI](https://mqttx.app/cli)やEMQXクライアントSDKの利用を推奨します。

## curlでMQTTサポートを確認する

```bash
curl --version | grep -i mqtt
```

`mqtt`（curl ≥ 8.19.0では`mqtts`も）が表示されれば、MQTT対応ビルドです。表示されない場合は：

- curlを7.70.0以上にアップグレード
- MQTT対応ビルドをインストール
- ソースから`--enable-mqtt`オプション付きでビルド

を検討してください。

## トラブルシューティング

curlとEMQXを使う際のよくある問題と対処法を紹介します。

### 接続拒否またはタイムアウト

**症状**

- `Connection refused`
- `Failed to connect to broker`
- 接続がハングしてタイムアウトする

**原因の可能性**

- ブローカーアドレスやポートの誤り
- ネットワークファイアウォールがMQTT/MQTTSポートをブロック
- ブローカーが起動していない、または指定ポートでリスニングしていない

**対処法**

- ブローカーアドレスとポートを確認：
  - MQTT：`1883`
  - MQTTS：`8883`
- 詳細モードで接続確認：

```bash
curl -v mqtt://broker.emqx.io/curl/test
```

### curlがMQTT/MQTTSをサポートしていない

**症状**

- `Protocol "mqtt" not supported`
- `Unknown protocol`

**原因の可能性**

- MQTT対応なしでビルドされたcurl
- 古いcurlバージョンの使用

**対処法**

- プロトコルサポートを確認：

```bash
curl --version
```

`mqtt`（およびTLS対応なら`mqtts`）が**Protocols**にあるか確認。

- curlをアップグレードまたはMQTT対応版をインストール。

### TLSハンドシェイクや証明書エラー（MQTTS）

**症状**

- `SSL certificate problem`
- `TLS handshake failed`
- `Unable to get local issuer certificate`

**原因の可能性**

- CA証明書が不足または誤っている
- ブローカーがプライベートまたは自己署名証明書を使用

**対処法**

- CA証明書を明示的に指定：

```bash
curl --cacert /path/to/ca.crt \
  mqtts://your-broker.example.com/topic
```

- テスト用に検証をスキップ（本番非推奨）：

```bash
curl -k mqtts://your-broker.example.com/topic
```

### サブスクライブ時にメッセージが受信できない

**症状**

- サブスクライブコマンドは実行されるが出力がない

**原因の可能性**

- 出力バッファリングが有効
- トピックにメッセージがパブリッシュされていない
- トピック名の不一致

**対処法**

- サブスクライブ時は必ず`-N`を使用：

```bash
curl -N mqtt://broker.emqx.io/curl/test
```

- 同じトピックにメッセージがパブリッシュされているか確認。

### 認証失敗

**症状**

- 接続直後に切断される
- ブローカーのログに認証・認可エラー

**原因の可能性**

- ユーザー名またはパスワードの誤り
- トピックに対するACL制限

**対処法**

- 認証情報を確認：

```bash
curl -u "username:password" \
  mqtts://your-broker.example.com/topic
```

- EMQXの認証設定とACLを確認。

## さらに詳しく

curlを使ったMQTT/MQTTSの詳細な使い方、背景説明、追加例、利用上の注意点については、以下のブログ記事をご覧ください：  
[Using curl for MQTT: Connect, Publish, and Subscribe with Secure IoT Communication](https://www.emqx.com/en/blog/using-curl-for-mqtt)

本ブログは本Enterprise向けガイドを補完し、より深い解説と拡張例を提供しています。
