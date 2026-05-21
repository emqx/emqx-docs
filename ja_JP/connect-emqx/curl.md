# EMQXでcurlを使う

<<<<<<< HEAD
curlは広く使われているコマンドラインツールで、データ転送や自動化に利用されます。2020年以降、curlはMQTTプロトコルをサポートしており、curl 8.19.0（2026年3月予定）以降ではMQTTS（TLS上のMQTT）もサポートしています。

curlを使うことで、開発者は言語固有のMQTTクライアントSDKをインストールせずに、コマンドラインから直接EMQXに接続し、メッセージのパブリッシュやトピックのサブスクライブが可能です。これにより、簡単なテストやスクリプト作成、IoTプロトタイピングに便利な選択肢となります。
=======
curlは広く使われているコマンドラインツールで、データ転送や自動化に利用されます。2020年以降、curlはMQTTプロトコルをサポートしており、curl 8.19.0（2026年3月予定）以降はMQTTS（TLS上のMQTT）もサポートします。

curlを使うことで、開発者は言語固有のMQTTクライアントSDKをインストールせずに、コマンドラインから直接EMQXに接続し、メッセージのパブリッシュやトピックのサブスクライブが可能です。これにより、迅速なテスト、スクリプト作成、IoTプロトタイピングに便利なツールとなっています。
>>>>>>> origin/release-5.10

本ページでは、EMQXとcurlを使ったMQTTおよびMQTTS通信の接続、パブリッシュ／サブスクライブ、認証、TLS設定、よくあるトラブルシューティングについて解説します。

## curlのバージョン要件

<<<<<<< HEAD
| 機能               | 最低バージョン | リリース時期         |
| ------------------ | -------------- | -------------------- |
| MQTT (`mqtt://`)   | 7.70.0         | 2020年4月            |
| MQTTS (`mqtts://`) | 8.19.0         | 2026年3月初旬予定    |
=======
| 機能               | 最低バージョン | リリース時期       |
| ------------------ | -------------- | ------------------ |
| MQTT (`mqtt://`)   | 7.70.0         | 2020年4月          |
| MQTTS (`mqtts://`) | 8.19.0         | 2026年3月初旬予定  |
>>>>>>> origin/release-5.10

インストール済みのcurlバージョンを確認するには：

```bash
curl --version
```

<<<<<<< HEAD
**Protocols**リストに`mqtt`（curl ≥ 8.19.0では`mqtts`も）が含まれていることを確認してください。

> curlのバージョンが古い場合は、パッケージマネージャーでアップグレードするか、https://curl.se/download から最新版をダウンロードしてください。  
> macOSの場合は`brew install curl`で最新バージョンをインストールできます。

## MQTTブローカーの準備

接続先のMQTTブローカーが必要です。本ガイドではMQTTおよびMQTTSをサポートするEMQXを使用します。

### EMQXパブリックブローカー（テスト用）

独自にブローカーを用意せずに素早くテストしたい場合は、EMQXのパブリックブローカーを利用できます。

| パラメータ  | 値                 |
| ----------- | ------------------ |
| ブローカー | `broker.emqx.io`   |
| MQTTポート  | `1883`             |
| MQTTSポート | `8883`             |

パブリックブローカーはテストおよびデモ目的のみの利用を想定しています。

### EMQX Enterpriseデプロイメント

本番環境では、独自のEMQX Enterpriseデプロイメントにcurlを接続します。ブローカーのアドレス、ポート、認証情報、TLS設定は環境に応じて指定してください。
=======
**Protocols**リストに`mqtt`（curl ≥ 8.19.0の場合は`mqtts`も）が含まれていることを確認してください。

> curlのバージョンが古い場合は、パッケージマネージャーでアップグレードするか、https://curl.se/download からダウンロードしてください。
>
> macOSの場合は、`brew install curl`で最新バージョンをインストールできます。

## MQTTブローカーの準備

接続先としてMQTTブローカーが必要です。本ガイドではMQTTとMQTTSの両方をサポートするEMQXを使用します。

### EMQXパブリックブローカー（テスト用）

独自にブローカーを構築せずに手軽にテストしたい場合は、EMQXのパブリックブローカーを利用できます。

| パラメーター | 値                  |
| ------------ | ------------------- |
| ブローカー   | `broker.emqx.io`    |
| MQTTポート   | `1883`              |
| MQTTSポート  | `8883`              |

パブリックブローカーはテストおよびデモ目的のみの利用を想定しています。

### EMQX Enterpriseのデプロイ環境

本番環境では、curlを自社のEMQX Enterpriseデプロイメントに接続します。ブローカーのアドレス、ポート、認証情報、TLS設定は環境に応じて設定してください。
>>>>>>> origin/release-5.10

一般的な構成例：

- カスタムブローカーのホスト名またはIPアドレス
- EMQX Enterpriseで有効化されたMQTTおよび／またはMQTTSリスナー
- ユーザー名／パスワード認証、トークン認証、または相互TLS認証
<<<<<<< HEAD
- トピックに対するアクセス制御ルール（ACL）
=======
- トピックに適用されるアクセス制御ルール（ACL）
>>>>>>> origin/release-5.10

curlコマンドを作成する際は、EMQX Enterpriseのリスナー設定、認証設定、TLS設定を参照してください。

**注意事項**

<<<<<<< HEAD
- 例にある`broker.emqx.io`は、ご自身のEMQX Enterpriseブローカーのアドレスに置き換えてください。
- 接続前にEMQX Enterpriseで該当のMQTTまたはMQTTSリスナーが有効になっていることを確認してください。

> 自己管理型のEMQX Enterpriseに加え、完全マネージドMQTTサービスである[EMQX Cloud](https://docs.emqx.com/en/cloud/latest/)（ServerlessまたはDedicated）にもcurlで接続可能です。  
> curlのMQTT/MQTTS利用方法は同じで、EMQX Cloudが提供するブローカーアドレス、ポート、認証情報を使用してください。

### curlでEMQX Enterpriseに接続する

MQTTでは、クライアントはトピックのサブスクライブやメッセージのパブリッシュなどの操作の一環としてブローカーに接続します。独立した「接続」コマンドはありません。

curlでEMQX Enterpriseを利用する場合は、サブスクライブやパブリッシュコマンドを実行すると、指定したEnterpriseブローカーアドレス（および認証やTLS設定があればそれら）を使って自動的に接続が確立されます。
=======
- 例示の`broker.emqx.io`は、自社のEMQX Enterpriseブローカーアドレスに置き換えてください。
- 接続前にEMQX Enterpriseで対応するMQTTまたはMQTTSリスナーが有効になっていることを確認してください。

> 自己管理型のEMQX Enterpriseに加え、完全マネージドMQTTサービスである[EMQX Cloud](https://docs.emqx.com/en/cloud/latest/overview.html)（ServerlessまたはDedicated）への接続にもcurlを利用可能です。
>
> curlのMQTT/MQTTS利用方法は同じで、EMQX Cloudから提供されるブローカーアドレス、ポート、認証情報を使用してください。

### curlでEMQX Enterpriseに接続する

MQTTでは、クライアントはトピックのサブスクライブやメッセージのパブリッシュなどの操作時にブローカーへ接続を確立します。独立した「接続」コマンドはありません。

curlでEMQX Enterpriseを利用する場合、サブスクライブまたはパブリッシュコマンドを実行すると、自動的に接続が確立されます（Enterpriseブローカーアドレスや認証・TLS設定がある場合はそれに従います）。
>>>>>>> origin/release-5.10

例えば、以下のコマンドはEMQX Enterpriseに接続し、トピックをサブスクライブします。

```bash
curl -N mqtts://your-enterprise-broker.example.com/curl/test
```

<<<<<<< HEAD
> **注意**：MQTTS（`mqtts://`）はcurl 8.19.0以降が必要です。curl 7.70.0〜8.18.xでは`mqtt://`を使用してください。
=======
> **注意**：MQTTS（`mqtts://`）はcurl 8.19.0以降が必要です。curl 7.70.0〜8.18.xの場合は`mqtt://`を使用してください。
>>>>>>> origin/release-5.10

## curlのMQTT URLスキームの理解

curlはMQTT操作にURLベースの構文を使用します：

```
mqtt[s]://[user:password@]broker[:port]/topic
```

各要素の意味：

- `mqtt[s]`：プロトコル（`mqtt`または`mqtts`）
<<<<<<< HEAD
- `[user:password@]`：省略可能な認証情報
=======
- `[user:password@]`：認証情報（省略可能）
>>>>>>> origin/release-5.10
- `broker`：ブローカーのホスト名またはIPアドレス
- `[:port]`：省略可能なポート番号。指定しない場合はデフォルトを使用：
  - `mqtt://`は`1883`
  - `mqtts://`は`8883`
<<<<<<< HEAD
- `/topic`：MQTTのトピックパス（例：`/sensor/temperature`）

## curlのMQTT出力フォーマット

トピックをサブスクライブすると、curlは以下の形式で生のMQTTメッセージデータを出力します：

```
[2バイト：トピック長（ビッグエンディアン）] [トピック文字列] [ペイロード]
```

例えば、トピック`curl/test`にメッセージ`"hello"`が届くと：
=======
- `/topic`：MQTTトピックパス（例：`/sensor/temperature`）

## curlのMQTT出力フォーマット

トピックをサブスクライブすると、curlはMQTTメッセージを以下の形式で生データとして出力します：

```
[2バイト: トピック長（ビッグエンディアン）] [トピック文字列] [ペイロード]
```

例えば、トピック`curl/test`にメッセージ`"hello"`が届くと、出力は次のようになります：
>>>>>>> origin/release-5.10

```
curl/testhello
```

<<<<<<< HEAD
この出力はトピック名とペイロードが連結されたバイナリ形式であり、解析しないと読みづらいです。

この出力はバイナリであり、デフォルトでは人間が読みやすい形式ではありません。以下の[MQTTメッセージの解析](#parse-mqtt-messages)を参照してください。
=======
この出力はトピック名とペイロードが連結されたバイナリ形式であり、そのままでは読みづらいです。

読みやすく変換する方法は後述の[MQTTメッセージの解析](#parse-mqtt-messages)を参照してください。
>>>>>>> origin/release-5.10

## トピックのサブスクライブ

サブスクライブは接続を維持し、受信したメッセージを`stdout`に出力します。

### 基本的なサブスクライブ（暗号化なし）

```bash
curl -N mqtt://broker.emqx.io/curl/test --output messages.bin
```

<<<<<<< HEAD
`-N`オプションは出力のバッファリングを無効化し、メッセージを即時に表示します。

### MQTTSによる安全なサブスクライブ（curl ≥ 8.19.0）
=======
`-N`オプションは出力バッファリングを無効化し、メッセージを即時表示します。

### MQTTSによるセキュアサブスクライブ（curl ≥ 8.19.0）
>>>>>>> origin/release-5.10

```bash
curl -N mqtts://broker.emqx.io/curl/test --output messages.bin
```

### 認証付きサブスクライブ

```bash
curl -N -u "username:password" \
  mqtts://your-broker.emqxsl.com/curl/test --output messages.bin
```

## MQTTメッセージの解析

<<<<<<< HEAD
curlのサブスクライブ出力は**バイナリ形式**であり、整形されたテキストではありません。
=======
curlのサブスクライブ出力は**バイナリ形式**であり、整形されたテキスト表示ではありません。
>>>>>>> origin/release-5.10

各メッセージは以下の構造です：

- 2バイト：トピック長（ビッグエンディアン）
- トピック文字列
<<<<<<< HEAD
- ペイロード（生バイト）
=======
- ペイロード（生バイト列）
>>>>>>> origin/release-5.10

そのため、出力はトピックとペイロードが連結された形で表示され、人間には読みづらいです。

### Bashワンライナー

<<<<<<< HEAD
サブスクライブ出力を読みやすくするため、curlの出力をシェルでパースします：
=======
curlの出力をシェルでパースし、読みやすくする例：
>>>>>>> origin/release-5.10

```bash
curl -sN mqtt://broker.emqx.io/curl/test | \
  while IFS= read -r -d $'\0' d; do
    [ -z "$d" ] && continue

<<<<<<< HEAD
    # curlのMQTTサブスクライブ出力は、2バイトのトピック長（MSB,LSB）、トピック、ペイロード。
    # このループはNUL(0x00)を区切り文字に使い、LSBを先に見てMSB=0と仮定（トピック長0〜255で動作）。
=======
    # curlのMQTTサブスクライブ出力は2バイトのトピック長（MSB,LSB）、トピック、ペイロード。
    # このループはNUL(0x00)を区切りにしているためLSBが先に見えており、
    # MSB=0と暗黙的に仮定（トピック長0〜255バイトに対応）。
>>>>>>> origin/release-5.10
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
<<<<<<< HEAD
2. 2バイトのトピック長の下位バイトから長さを取得（256バイト未満のトピックに対応）
3. トピック文字列とペイロードを長さ指定で抽出
4. 各メッセージを`[トピック] ペイロード`形式で表示

### 生データをファイルに保存して確認

MQTTメッセージのバイナリ構造を詳しく調べるには、サブスクライブ出力をファイルに保存し、`hexdump`などで確認します。
=======
2. トピック長の下位バイトを取得（上位バイトは0と仮定）
3. トピック文字列とペイロードを長さに基づき抽出
4. `[トピック] ペイロード`形式で表示

### 生データをファイルに保存して確認

MQTTメッセージのバイナリ構造を理解するため、サブスクライブ出力をファイルに保存し、手動で確認できます。
>>>>>>> origin/release-5.10

保存例：

```bash
curl -sN mqtt://broker.emqx.io/curl/test > messages.bin
```

<<<<<<< HEAD
`hexdump`で確認：
=======
`hexdump`で内容を確認：
>>>>>>> origin/release-5.10

```bash
hexdump -C messages.bin
```

<<<<<<< HEAD
これにより、トピック長のプレフィックス、トピックバイト列、ペイロードのレイアウトが明確に見えます。

### 再利用可能なシェル関数

繰り返し使う場合は、パーサーをシェル関数にまとめられます：
=======
トピック長のプレフィックスやトピック名、ペイロードの配置が明確に見えます。

### 再利用可能なシェル関数

繰り返し使う場合は、以下のように関数化できます：
>>>>>>> origin/release-5.10

```bash
mqtt_subscribe() {
  curl -sN "$1" | while IFS= read -r -d $'\0' d; do
    [ -z "$d" ] && continue

    # curlのMQTTサブスクライブ出力は2バイトのトピック長（MSB,LSB）、トピック、ペイロード。
<<<<<<< HEAD
    # NUL(0x00)を区切り文字に使い、LSBを先に見てMSB=0と仮定（トピック長0〜255で動作）。
=======
    # このループはNUL(0x00)区切りでLSBが先に見え、MSB=0と仮定（トピック長0〜255バイト対応）。
>>>>>>> origin/release-5.10
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

<<<<<<< HEAD
> 本番用途や複雑な解析が必要な場合は、[MQTTX CLI](https://mqttx.app/cli)の利用を検討してください。  
> MQTT 5.0対応、QoS処理、ワイルドカードサブスクライブなど、整形された出力を提供します。

## メッセージのパブリッシュ

パブリッシュはcurlの`-d`（データ）オプションでペイロードを指定します。
=======
> 本格的な利用や複雑な解析には、[MQTTX CLI](https://mqttx.app/cli)の利用を推奨します。MQTT 5.0対応、QoS制御、ワイルドカードサブスクライブなどが可能です。

## メッセージのパブリッシュ

メッセージをパブリッシュするには、curlの`-d`（データ）オプションでペイロードを指定します。
>>>>>>> origin/release-5.10

### 基本的なパブリッシュ（暗号化なし）

```bash
curl -d "Hello from curl" \
  mqtt://broker.emqx.io/curl/test
```

<<<<<<< HEAD
### MQTTSによる安全なパブリッシュ（curl ≥ 8.19.0）
=======
### MQTTSによるセキュアパブリッシュ（curl ≥ 8.19.0）
>>>>>>> origin/release-5.10

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

<<<<<<< HEAD
以下は本ドキュメントで使用されるcurlコマンドラインオプションの概要です。

| オプション      | 説明                                         | 典型的な用途    |
| --------------- | -------------------------------------------- | --------------- |
| `-N`            | 出力のバッファリングを無効化（サブスクライブ時必須） | サブスクライブ  |
| `-d`            | パブリッシュするメッセージペイロード          | パブリッシュ    |
| `-u user:pass`  | ユーザー名とパスワードによる認証               | 認証            |
| `-v`            | 詳細出力（MQTTハンドシェイクの表示）           | トラブルシューティング |
| `-s`            | サイレントモード（進捗表示を抑制）              | スクリプト      |
| `--cacert`      | TLS検証用のCA証明書                            | MQTTS           |
| `--cert`        | 相互TLS用のクライアント証明書                    | MQTTS           |
| `--key`         | 相互TLS用のクライアント秘密鍵                    | MQTTS           |
| `-k`            | TLS検証をスキップ（テスト用途のみ）               | トラブルシューティング |
=======
本ドキュメントで使用するcurlコマンドラインオプションの概要：

| オプション       | 説明                                         | 主な用途         |
| ---------------- | -------------------------------------------- | ---------------- |
| `-N`             | 出力バッファリングを無効化（サブスクライブ時必須） | サブスクライブ   |
| `-d`             | パブリッシュするメッセージペイロード         | パブリッシュ     |
| `-u user:pass`   | ユーザー名とパスワードによる認証             | 認証             |
| `-v`             | 詳細出力（MQTTハンドシェイク表示）           | トラブルシューティング |
| `-s`             | サイレントモード（進捗表示抑制）             | スクリプト       |
| `--cacert`       | TLS検証用のCA証明書指定                       | MQTTS            |
| `--cert`         | 相互TLS用のクライアント証明書                 | MQTTS            |
| `--key`          | 相互TLS用のクライアント秘密鍵                 | MQTTS            |
| `-k`             | TLS検証をスキップ（テスト目的のみ）           | トラブルシューティング |
>>>>>>> origin/release-5.10

> curlの全オプション一覧は公式ドキュメントを参照してください。

## TLS設定（MQTTS）

<<<<<<< HEAD
### CA証明書による検証
=======
### CA証明書検証
>>>>>>> origin/release-5.10

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

<<<<<<< HEAD
> 証明書検証をスキップする場合は`-k`を使用しますが、本番環境では推奨しません。
=======
> 証明書検証をスキップするには`-k`を使用しますが、本番環境では推奨しません。
>>>>>>> origin/release-5.10

## よくあるユースケース

### ブローカー接続テスト

```bash
curl -v mqtt://broker.emqx.io/curl/test
```

<<<<<<< HEAD
DNS解決、TCP接続、MQTTハンドシェイクを検証します。

**接続成功の指標**

成功時は詳細出力に以下が含まれます：

- ブローカーのホスト名が1つ以上のIPアドレスに解決される
- ポート`1883`（MQTT）または`8883`（MQTTS）へのTCP接続が成功
=======
DNS解決、TCP接続、MQTTハンドシェイクの確認に使います。

**接続成功の目安**

成功時は詳細出力に以下が含まれます：

- ブローカーのホスト名がIPアドレスに解決される
- ポート`1883`（MQTT）または`8883`（MQTTS）へのTCP接続成功
>>>>>>> origin/release-5.10
- MQTTハンドシェイクがエラーなく完了

エラーが表示されず正常終了すれば、EMQXブローカーへの接続が確立されています。

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

<<<<<<< HEAD
## curlのMQTTにおける制限事項

curlはテストやスクリプト用途に便利ですが、以下の制限があります：

| 制限事項              | 説明                                 |
| --------------------- | ------------------------------------ |
| QoS 0のみ対応         | QoS 1や2はサポートされていません      |
| バイナリ出力          | サブスクライブ出力は整形されていません  |
| ワイルドカード非対応  | `+`や`#`によるサブスクライブ不可       |
| 単一トピックのみ対応  | 1コマンドで複数トピックは不可          |
| 永続セッションなし    | ステートレスな接続のみ                 |
=======
## curlのMQTTにおける制限

curlはテストやスクリプト用途に便利ですが、以下の制限があります：

| 制限事項               | 説明                                  |
| ---------------------- | ------------------------------------- |
| QoS 0のみ対応          | QoS 1や2はサポートされていません      |
| バイナリ出力           | サブスクライブ出力は整形されていません |
| ワイルドカード非対応   | `+`や`#`によるサブスクライブ不可       |
| 単一トピックのみ対応   | 1コマンドで複数トピック不可            |
| 永続セッション非対応   | ステートレス接続のみ                   |
>>>>>>> origin/release-5.10

高度なMQTT機能が必要な場合は、[MQTTX CLI](https://mqttx.app/cli)やEMQXクライアントSDKの利用を推奨します。

## curlでMQTTサポートを確認する

```bash
curl --version | grep -i mqtt
```

<<<<<<< HEAD
出力に`mqtt`（curl ≥ 8.19.0では`mqtts`も）が含まれていれば、MQTT対応ビルドです。含まれていない場合は：

- curlを7.70.0以降にアップグレード
- MQTT対応ビルドをインストール
- ソースから`--enable-mqtt`オプションでビルド
=======
`mqtt`（curl ≥ 8.19.0なら`mqtts`も）が表示されれば、MQTTサポート付きのcurlビルドです。表示されない場合は：

- curlを7.70.0以降にアップグレード
- MQTT対応ビルドをインストール
- ソースから`--enable-mqtt`オプション付きでビルド
>>>>>>> origin/release-5.10

を検討してください。

## トラブルシューティング

<<<<<<< HEAD
curlとEMQX利用時によくある問題と対処法を示します。

### 接続拒否またはタイムアウト
=======
curlとEMQXを使う際のよくある問題と対処法を紹介します。

### 接続拒否やタイムアウト
>>>>>>> origin/release-5.10

**症状**

- `Connection refused`
- `Failed to connect to broker`
- 接続がハングしタイムアウトする

<<<<<<< HEAD
**原因例**

- ブローカーアドレスやポートの誤り
- ネットワークファイアウォールがMQTT/MQTTSポートを遮断
- ブローカーが起動していない、または指定ポートで待機していない
=======
**原因候補**

- ブローカーアドレスやポートが誤っている
- ネットワークファイアウォールがMQTT/MQTTSポートを遮断
- ブローカーが起動していない、または指定ポートで待ち受けていない
>>>>>>> origin/release-5.10

**対処法**

- ブローカーアドレスとポートを確認：
  - MQTT：`1883`
  - MQTTS：`8883`
- 詳細モードでネットワーク接続を確認：

```bash
curl -v mqtt://broker.emqx.io/curl/test
```

<<<<<<< HEAD
### curlがMQTTやMQTTSをサポートしていない
=======
### curlでMQTTやMQTTSがサポートされていない
>>>>>>> origin/release-5.10

**症状**

- `Protocol "mqtt" not supported`  
- `Unknown protocol`

<<<<<<< HEAD
**原因例**

- curlがMQTTサポートなしでビルドされている
- curlバージョンが古い
=======
**原因候補**

- MQTTサポートなしでビルドされたcurl
- curlのバージョンが古い
>>>>>>> origin/release-5.10

**対処法**

- プロトコルサポートを確認：

```bash
curl --version
```

`mqtt`（およびTLS対応なら`mqtts`）が**Protocols**に含まれているか確認。

<<<<<<< HEAD
- curlをアップグレード、またはMQTT対応ビルドをインストール。
=======
- curlをアップグレードまたはMQTT対応ビルドをインストール。
>>>>>>> origin/release-5.10

### TLSハンドシェイクや証明書エラー（MQTTS）

**症状**

- `SSL certificate problem`  
- `TLS handshake failed`  
- `Unable to get local issuer certificate`

<<<<<<< HEAD
**原因例**
=======
**原因候補**
>>>>>>> origin/release-5.10

- CA証明書が不足または誤っている
- ブローカーがプライベートまたは自己署名証明書を使用

**対処法**

- CA証明書を明示的に指定：

```bash
curl --cacert /path/to/ca.crt \
  mqtts://your-broker.example.com/topic
```

<<<<<<< HEAD
- テスト目的で検証をスキップ（本番では推奨しません）：
=======
- テスト目的で検証をスキップ（本番非推奨）：
>>>>>>> origin/release-5.10

```bash
curl -k mqtts://your-broker.example.com/topic
```

<<<<<<< HEAD
### サブスクライブしてもメッセージが受信できない
=======
### サブスクライブ時にメッセージが受信できない
>>>>>>> origin/release-5.10

**症状**

- サブスクライブコマンドは動作するが出力がない

<<<<<<< HEAD
**原因例**
=======
**原因候補**
>>>>>>> origin/release-5.10

- 出力バッファリングが有効
- トピックにメッセージがパブリッシュされていない
- トピック名の不一致

**対処法**

- サブスクライブ時は必ず`-N`を使用：

```bash
curl -N mqtt://broker.emqx.io/curl/test
```

- メッセージが同じトピックにパブリッシュされているか確認。

### 認証失敗

**症状**

<<<<<<< HEAD
- 接続がすぐ切断される
- ブローカーのログに認証／認可エラー

**原因例**
=======
- 接続が即座に切断される
- ブローカーのログに認証・認可エラー

**原因候補**
>>>>>>> origin/release-5.10

- ユーザー名またはパスワードの誤り
- トピックに対するACL制限

**対処法**

- 認証情報を確認：

```bash
curl -u "username:password" \
  mqtts://your-broker.example.com/topic
```

<<<<<<< HEAD
- EMQXの認証設定およびACL設定を確認。

## さらに詳しく

curlを使ったMQTTおよびMQTTSの利用方法をステップバイステップで詳しく解説し、背景説明や追加例、利用上の注意点を含むブログ記事はこちら：  
[Using curl for MQTT: Connect, Publish, and Subscribe with Secure IoT Communication](https://www.emqx.com/en/blog/using-curl-for-mqtt)

本ブログは本Enterprise向けガイドを補完し、より詳細な解説と拡張例を提供しています。
=======
- EMQXの認証設定とACLを確認。

## さらに詳しく

curlを使ったMQTTおよびMQTTSの接続、パブリッシュ、サブスクライブの詳細な手順や背景解説、追加例、利用上の注意点は、以下のブログ記事を参照してください。

[Using curl for MQTT: Connect, Publish, and Subscribe with Secure IoT Communication](https://www.emqx.com/en/blog/using-curl-for-mqtt)

本ブログは本Enterprise向けガイドを補完し、より深い解説と拡張例を提供しています。
>>>>>>> origin/release-5.10
