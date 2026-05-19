# EMQXでcurlを使う

curlは、データ転送や自動化で広く使われているコマンドラインツールです。2020年以降、curlはMQTTプロトコルをサポートしており、curl 8.19.0（2026年3月頃予定）以降はMQTTS（TLS上のMQTT）もサポートします。

curlを使うことで、開発者は言語固有のMQTTクライアントSDKをインストールせずに、コマンドラインから直接EMQXに接続し、メッセージのパブリッシュやトピックのサブスクライブが可能です。これにより、簡単なテストやスクリプト作成、IoTプロトタイピングに便利な選択肢となります。

本ページでは、EMQXとcurlを使ったMQTTおよびMQTTS通信の接続、パブリッシュ／サブスクライブ、認証、TLS設定、よくあるトラブルシューティングについて説明します。

## curlのバージョン要件

| 機能               | 最低バージョン | リリース時期       |
| ------------------ | -------------- | ------------------ |
| MQTT (`mqtt://`)   | 7.70.0         | 2020年4月          |
| MQTTS (`mqtts://`) | 8.19.0         | 2026年3月初旬予定  |

インストール済みのcurlバージョンを確認するには：

```bash
curl --version
```

**Protocols**リストに`mqtt`（curl ≥ 8.19.0の場合は`mqtts`も）が含まれていることを確認してください。

> curlのバージョンが古い場合は、パッケージマネージャーでアップグレードするか、https://curl.se/download からダウンロードしてください。
>
> macOSでは、`brew install curl`で最新バージョンをインストールできます。

## MQTTブローカーのセットアップ

接続先としてMQTTブローカーが必要です。本ガイドではMQTTとMQTTSの両方をサポートするEMQXを使用します。

### EMQXパブリックブローカー（テスト用）

独自のブローカーをデプロイせずにすぐにテストしたい場合は、EMQXのパブリックブローカーを利用できます。

| パラメータ | 値                 |
| ---------- | ------------------ |
| ブローカー | `broker.emqx.io`   |
| MQTTポート | `1883`             |
| MQTTSポート| `8883`             |

パブリックブローカーはテストおよびデモ目的のみの利用を想定しています。

### EMQX Enterpriseデプロイメント

本番環境では、EMQX Enterpriseのブローカーアドレス、ポート、認証情報、TLS設定を用いてcurlを接続します。

一般的な構成例：

- カスタムブローカーホスト名またはIPアドレス
- EMQX Enterpriseで有効化されたMQTTおよび／またはMQTTSリスナー
- ユーザー名／パスワード認証、トークン認証、または相互TLS認証
- トピックに適用されるアクセス制御ルール（ACL）

curlコマンドを作成する際は、EMQX Enterpriseのリスナー設定、認証設定、TLS設定を参照してください。

**注意**

- すべての例で`broker.emqx.io`を、自身のEMQX Enterpriseブローカーアドレスに置き換えてください。
- 接続前にEMQX Enterpriseで対応するMQTTまたはMQTTSリスナーが有効になっていることを確認してください。

> 自己管理型のEMQX Enterpriseに加え、完全マネージドMQTTサービスである[EMQX Cloud](https://docs.emqx.com/en/cloud/latest/)（ServerlessまたはDedicated）にもcurlで接続可能です。
>
> curlのMQTT/MQTTS利用方法は同じです。EMQX Cloudが提供するブローカーアドレス、ポート、認証情報を使用してください。

### curlでEMQX Enterpriseに接続する

MQTTでは、クライアントはトピックのサブスクライブやメッセージのパブリッシュなどの操作の一環としてブローカーに接続します。独立した「接続」コマンドはありません。

curlをEMQX Enterpriseで使う場合、サブスクライブやパブリッシュコマンドを実行すると、Enterpriseブローカーアドレス（および認証やTLS設定がある場合はそれら）を用いて自動的に接続が確立されます。

例えば、以下のコマンドはEMQX Enterpriseに接続し、トピックをサブスクライブする一連の操作を一度に行います。

```bash
curl -N mqtts://your-enterprise-broker.example.com/curl/test
```

> **注意**：MQTTS（`mqtts://`）はcurl 8.19.0以降が必要です。curl 7.70.0～8.18.xでは`mqtt://`を使用してください。

## curlのMQTT URLスキームの理解

curlはMQTT操作にURLベースの構文を使用します：

```
mqtt[s]://[user:password@]broker[:port]/topic
```

各要素の意味：

- `mqtt[s]`：プロトコル（`mqtt`または`mqtts`）
- `[user:password@]`：省略可能な認証情報
- `broker`：ブローカーのホスト名またはIPアドレス
- `[:port]`：省略可能なポート番号。指定がない場合はデフォルトを使用
  - `mqtt://`は`1883`
  - `mqtts://`は`8883`
- `/topic`：MQTTトピックのパス（例：`/sensor/temperature`）

## curlのMQTT出力フォーマット

トピックをサブスクライブすると、curlは以下の形式で生のMQTTメッセージデータを出力します：

```
[2バイト: トピック長（ビッグエンディアン）] [トピック文字列] [ペイロード]
```

例えば、トピック`curl/test`にメッセージ`"hello"`が届くと、以下のように表示されます：

```
curl/testhello
```

この出力はトピック名とペイロードが連結されたバイナリ形式のため、解析しないと読み取りが困難です。

この出力はバイナリであり、デフォルトでは人間が読みやすい形式ではありません。以下の[MQTTメッセージの解析](#parse-mqtt-messages)を参照してください。

## トピックのサブスクライブ

サブスクライブは接続を維持し、受信したメッセージを`stdout`に出力します。

### 基本的なサブスクライブ（暗号化なし）

```bash
curl -N mqtt://broker.emqx.io/curl/test --output messages.bin
```

`-N`オプションは出力バッファリングを無効化し、メッセージを即時に表示します。

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

トピックをサブスクライブすると、curlはMQTTメッセージを**バイナリ形式**で出力します。整形済みのテキスト形式ではありません。

各メッセージの構造は以下の通りです：

- 2バイト：トピック長（ビッグエンディアン）
- トピック文字列
- ペイロード（生データ）

そのため、出力はトピックとペイロードが連結された形で表示され、人間には読みづらいです。

### Bashワンライナー

サブスクライブ出力を読みやすくするため、curlの出力をシェルスクリプトで解析します：

```bash
curl -sN mqtt://broker.emqx.io/curl/test | \
  while IFS= read -r -d $'\0' d; do
    [ -z "$d" ] && continue

    # curlのMQTTサブスクライブ出力は2バイトのトピック長（MSB,LSB）、トピック、ペイロード。
    # このループは区切り文字にNUL(0x00)を使い、LSBを先に読み取り、
    # MSB=0を暗黙的に仮定（トピック長0～255バイトに対応）。
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

1. ストリームをヌル文字で分割
2. 2バイトのトピック長の下位バイトから長さを取得（上位バイトは0と仮定）
3. トピック文字列とペイロードを長さに基づき抽出
4. 各メッセージを`[トピック] ペイロード`形式で表示

### 生データを保存して確認

MQTTメッセージのバイナリ構造を詳細に調べるには、生のサブスクライブ出力をファイルに保存し、`hexdump`などで確認します。

保存例：

```bash
curl -sN mqtt://broker.emqx.io/curl/test > messages.bin
```

`hexdump`で確認：

```bash
hexdump -C messages.bin
```

トピック長のプレフィックス、トピックバイト列、ペイロードの配置が明確に見えます。

### 再利用可能なシェル関数

繰り返し使う場合は、以下のように関数化できます：

```bash
mqtt_subscribe() {
  curl -sN "$1" | while IFS= read -r -d $'\0' d; do
    [ -z "$d" ] && continue

    # curlのMQTTサブスクライブ出力は2バイトのトピック長（MSB,LSB）、トピック、ペイロード。
    # このループはNUL(0x00)区切りでLSBを先に読み取り、
    # MSB=0を仮定（トピック長0～255バイト対応）。
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

> 本番環境や複雑な解析が必要な場合は、[MQTTX CLI](https://mqttx.app/cli)の利用を検討してください。MQTT 5.0完全対応、QoS処理、ワイルドカードサブスクライブなどをサポートし、整形済みの出力を提供します。

## メッセージのパブリッシュ

メッセージをパブリッシュするには、curlの`-d`（データ）オプションでペイロードを指定します。

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

本ドキュメントで使用されるcurlコマンドラインオプションの概要です。

| オプション         | 説明                                         | 主な用途        |
| ------------------ | -------------------------------------------- | --------------- |
| `-N`               | 出力バッファリングを無効化（サブスクライブに必須） | サブスクライブ  |
| `-d`               | パブリッシュするメッセージペイロード          | パブリッシュ    |
| `-u user:pass`     | ユーザー名とパスワードによる認証               | 認証            |
| `-v`               | 詳細出力（MQTTハンドシェイクの表示）           | トラブルシューティング |
| `-s`               | サイレントモード（進捗表示を抑制）             | スクリプト      |
| `--cacert`         | TLS検証用のCA証明書                            | MQTTS           |
| `--cert`           | 相互TLS用のクライアント証明書                   | MQTTS           |
| `--key`            | 相互TLS用のクライアント秘密鍵                   | MQTTS           |
| `-k`               | TLS検証をスキップ（テスト用のみ推奨）           | トラブルシューティング |

> curlの全オプションについては、公式curlドキュメントを参照してください。

## TLS設定（MQTTS）

### CA証明書検証

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

> 証明書検証をスキップするには`-k`を使いますが、テスト用のみ推奨します。

## よくあるユースケース

### ブローカー接続テスト

```bash
curl -v mqtt://broker.emqx.io/curl/test
```

DNS解決、TCP接続、MQTTハンドシェイクの確認に使います。

**接続成功の指標**

成功時は詳細出力に以下が含まれます：

- ブローカーのホスト名がIPアドレスに解決される
- ポート`1883`（MQTT）または`8883`（MQTTS）へのTCP接続が成功
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

## curlのMQTTにおける制限事項

curlはテストやスクリプト用途に便利ですが、以下の制限があります：

| 制限事項               | 説明                                |
| ---------------------- | ----------------------------------- |
| QoS 0のみ対応          | QoS 1や2はサポートしていない        |
| バイナリ出力           | サブスクライブ出力は整形されていない |
| ワイルドカード非対応   | `+`や`#`を使ったサブスクライブ不可  |
| 単一トピックのみ       | 1コマンドで1トピックのみ対応        |
| 永続セッションなし     | ステートレスな接続のみ               |

高度なMQTT機能が必要な場合は、[MQTTX CLI](https://mqttx.app/cli)やEMQXクライアントSDKの利用を推奨します。

## curlでMQTTサポートを確認する

```bash
curl --version | grep -i mqtt
```

`mqtt`（curl ≥ 8.19.0の場合は`mqtts`も）が出力されれば、MQTT対応ビルドです。表示されない場合は：

- curlを7.70.0以上にアップグレード
- MQTT対応ビルドをインストール
- ソースから`--enable-mqtt`オプション付きでビルド

を検討してください。

## トラブルシューティング

curlとEMQXを使う際のよくある問題と対処法を示します。

### 接続拒否またはタイムアウト

**症状**

- `Connection refused`
- `Failed to connect to broker`
- 接続がハングしてタイムアウトする

**原因候補**

- ブローカーアドレスやポートの誤り
- ネットワークファイアウォールがMQTT/MQTTSポートをブロック
- ブローカーが起動していない、または指定ポートで待ち受けていない

**対処**

- ブローカーアドレスとポートを確認
  - MQTT：`1883`
  - MQTTS：`8883`
- 詳細モードでネットワーク接続を確認：

```bash
curl -v mqtt://broker.emqx.io/curl/test
```

### curlでMQTTやMQTTSがサポートされていない

**症状**

- `Protocol "mqtt" not supported`
- `Unknown protocol`

**原因候補**

- curlがMQTT対応でビルドされていない
- curlのバージョンが古い

**対処**

- プロトコルサポートを確認：

```bash
curl --version
```

`mqtt`（およびTLS対応なら`mqtts`）が**Protocols**にあるか確認。

- curlをアップグレード、またはMQTT対応ビルドをインストール。

### TLSハンドシェイクや証明書エラー（MQTTS）

**症状**

- `SSL certificate problem`
- `TLS handshake failed`
- `Unable to get local issuer certificate`

**原因候補**

- CA証明書がない、または誤っている
- ブローカーがプライベートまたは自己署名証明書を使用

**対処**

- CA証明書を明示的に指定：

```bash
curl --cacert /path/to/ca.crt \
  mqtts://your-broker.example.com/topic
```

- テスト用に検証をスキップ（本番環境では推奨しません）：

```bash
curl -k mqtts://your-broker.example.com/topic
```

### サブスクライブ時にメッセージが受信できない

**症状**

- サブスクライブコマンドは実行されるが出力がない

**原因候補**

- 出力バッファリングが有効
- トピックにメッセージがパブリッシュされていない
- トピック名の不一致

**対処**

- サブスクライブ時は必ず`-N`を使用：

```bash
curl -N mqtt://broker.emqx.io/curl/test
```

- メッセージが同じトピックにパブリッシュされているか確認。

### 認証失敗

**症状**

- 接続が即座に切断される
- ブローカーのログに認証や認可エラーが記録される

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

curlでのMQTTおよびMQTTS利用の詳細な手順、背景説明、追加例、利用上の注意点については、以下のブログ記事をご覧ください：

[Using curl for MQTT: Connect, Publish, and Subscribe with Secure IoT Communication](https://www.emqx.com/en/blog/using-curl-for-mqtt)

本ブログは本Enterprise向けガイドを補完し、より深い解説と拡張例を提供しています。
