# eMQTT-Benchによるパフォーマンステスト

EMQXをシングルモードまたはクラスターとしてデプロイした後、システムの容量や負荷時の挙動を把握するためにパフォーマンス評価を行うことができます。本セクションでは、パフォーマンステストを実施するための[eMQTT-Bench](https://www.emqx.com/en/try?product=emqtt-bench)のインストール方法と使用方法について説明します。

eMQTT-BenchはErlangで書かれた軽量かつ強力なMQTTベンチマークツールで、多数のクライアントをシミュレートし、スループットやレイテンシなどの主要なパフォーマンス指標を測定できます。大規模なテストシナリオや高度でカスタマイズされたベンチマーク要件については、sales@emqx.ioまでお問い合わせください。

## eMQTT-Benchのインストール

eMQTT-Benchのインストール方法は以下の3つのオプションがあります。

- Dockerイメージの実行
- バイナリパッケージのダウンロードとインストール
- ソースコードからのビルド

### Dockerイメージ

[hub.docker.com](https://hub.docker.com/r/emqx/emqtt-bench/tags)に公開されている`emqtt-bench`のDockerイメージを実行してベンチマークツールをインストールできます。`:latest`タグは新しいバージョンごとに更新されます。

```bash
docker run -it emqx/emqtt-bench:latest
Usage: emqtt_bench pub | sub | conn [--help]
```

Dockerイメージ名はハイフン`-`を使用していますが、バイナリスクリプト名はアンダースコア`_`を使用している点にご注意ください。

### バイナリパッケージ

以下のプラットフォーム向けにリリースされたバイナリパッケージをダウンロードし、`emqtt-bench`をインストールできます。

- Amazon Linux 2
- Amazon Linux 2023
- CentOS 7
- Rocky Linux 8
- Rocky Linux 9
- Debian 9
- Debian 10
- Debian 11
- Debian 12
- Ubuntu 16.04
- Ubuntu 18.04
- Ubuntu 20.04
- Ubuntu 22.04
- MacOS 11 (Intel)
- MacOS 12 (Intel)
- MacOS 12 (Apple Silicon)

各リリースの詳細については[Releases](https://github.com/emqx/emqtt-bench/releases)をご参照ください。

例えば、Ubuntu 20.04に`emqtt-bench`をインストールする手順は以下の通りです。

```bash
mkdir emqtt-bench && cd emqtt-bench
wget https://github.com/emqx/emqtt-bench/releases/download/0.4.12/emqtt-bench-0.4.12-ubuntu20.04-amd64.tar.gz
tar xfz emqtt-bench-0.4.12-ubuntu20.04-amd64.tar.gz
rm emqtt-bench-0.4.12-ubuntu20.04-amd64.tar.gz

./emqtt_bench
Usage: emqtt_bench pub | sub | conn [--help]
```

### ソースからのビルド

eMQTT-BenchはErlangで書かれており、ビルドには[Erlang/OTP](https://www.erlang.org/) 22.3以上が必要です。Erlang/OTPのインストール方法はここでは省略します。詳細はオンラインのインストールチュートリアルをご参照ください。

Erlang環境をインストール後、`emqtt-bench`の最新コードをダウンロードしてコンパイルします。

```bash
git clone https://github.com/emqx/emqtt-bench
cd emqtt-bench

make
```

コンパイル後、カレントディレクトリに`emqtt_bench`という実行可能スクリプトが生成されます。以下のコマンドで正常に動作することを確認してください。

```bash
./emqtt_bench
Usage: emqtt_bench pub | sub | conn [--help]
```

上記の出力が表示されれば、ホストに`emqtt-bench`が正しくインストールされたことを示します。

## eMQTT-Benchの使い方

`emqtt_bench`には以下の3つのサブコマンドがあります。

1. `pub`：多数のクライアントを作成し、メッセージのパブリッシュ操作を行います。
2. `sub`：多数のクライアントを作成し、トピックをサブスクライブしてメッセージを受信します。
3. `conn`：多数の接続を作成します。

### パブリッシュ

`./emqtt_bench pub --help`を実行すると利用可能なパラメータが表示されます。

| パラメータ         | 省略形 | オプション値    | デフォルト値  | 説明                                                                                  |
| ----------------- | ------ | -------------- | ------------ | ------------------------------------------------------------------------------------- |
| --host            | -h     | -              | localhost    | 接続するMQTTサーバーのアドレス                                                      |
| --port            | -p     | -              | 1883         | MQTTサービスのポート番号                                                             |
| --version         | -V     | 3<br />4<br />5 | 5            | 使用するMQTTプロトコルバージョン                                                    |
| --count           | -c     | -              | 200          | クライアントの総数                                                                   |
| --startnumber     | -n     | -              | 0            | クライアントの開始番号                                                               |
| --interval        | -i     | -              | 10           | クライアント作成間隔（単位：ms）                                                    |
| --interval_of_msg | -I     | -              | 1000         | メッセージパブリッシュ間隔                                                           |
| --username        | -u     | -              | なし（任意） | クライアントのユーザー名                                                             |
| --password        | -P     | -              | なし（任意） | クライアントのパスワード                                                             |
| --topic           | -t     | -              | なし（必須） | パブリッシュするトピック。プレースホルダー対応：<br />`%c`: ClientId<br />`%u`: Username<br />`%i`: クライアントの連番 |
| --size            | -s     | -              | 256          | メッセージペイロードのサイズ（バイト単位）                                         |
| --qos             | -q     | -              | 0            | QoSレベル                                                                            |
| --retain          | -r     | true<br />false | false        | メッセージのRetainフラグ設定の有無                                                 |
| --keepalive       | -k     | -              | 300          | クライアントのキープアライブ時間                                                    |
| --clean           | -C     | true<br />false | true         | セッションをクリアして接続を確立するかどうか                                       |
| --ssl             | -S     | true<br />false | false        | SSLを有効にするかどうか                                                             |
| --certfile        | -      | -              | なし         | クライアントのSSL証明書ファイル                                                     |
| --keyfile         | -      | -              | なし         | クライアントのSSLキー ファイル                                                      |
| --ws              | -      | true<br />false | false        | WebSocket経由で接続を確立するかどうか                                              |
| --ifaddr          | -      | -              | なし         | クライアント接続に使用するローカルネットワークインターフェースを指定               |

例えば、10接続を開始し、1秒間に100件のQoS0メッセージをトピック`t`にペイロードサイズ16バイトで送信する場合は以下のように実行します。

```bash
./emqtt_bench pub -t t -h emqx-server -s 16 -q 0 -c 10 -I 10
```

### サブスクライブ

`./emqtt_bench sub --help`を実行すると、このサブコマンドで利用可能なパラメータ一覧が表示されます。説明は上記の表に含まれているためここでは省略します。

例えば、500接続を開始し、それぞれがトピック`t`をQoS0でサブスクライブする場合は以下のように実行します。

```bash
./emqtt_bench sub -t t -h emqx-server -c 500
```

### コネクト

`./emqtt_bench conn --help`を実行すると、このサブコマンドで利用可能なパラメータ一覧が表示されます。説明は上記の表に含まれているためここでは省略します。

例えば、1000接続を開始する場合は以下のように実行します。

```bash
./emqtt_bench conn -h emqx-server -c 1000
```

### SSL接続

`emqtt-bench`はSSLによる安全な接続の確立とテストをサポートしています。

片方向認証の場合：

```bash
./emqtt_bench sub -c 100 -i 10 -t bench/%i -p 8883 -S
./emqtt_bench pub -c 100 -I 10 -t bench/%i -p 8883 -s 256 -S
```

双方向認証の場合：

```bash
./emqtt_bench sub -c 100 -i 10 -t bench/%i -p 8883 --certfile path/to/client-cert.pem --keyfile path/to/client-key.pem
./emqtt_bench pub -c 100 -i 10 -t bench/%i -s 256 -p 8883 --certfile path/to/client-cert.pem --keyfile path/to/client-key.pem
```

## ストレステストの実施

本セクションでは、接続数とスループットの2つの典型的なシナリオにおけるストレステストの実施方法を説明します。

### 代表的なストレステストシナリオ

ツールの使用例として以下の2つの典型的なシナリオを検証します。

1. 接続数：`emqtt-bench`を使用してEMQXに数百万の接続を作成する。
2. スループット：`emqtt-bench`を使用してEMQXで`100k/s Qos 0`のメッセージスループットを生成する。

### デバイスおよびデプロイトポロジー

8コア16GBメモリのサーバーを3台用意し、1台をEMQX用、2台をクライアントプレッシャー用に使用します。

- **システム**：`CentOS Linux release 7.7.1908 (Core)`
- **CPU**：`Intel Xeon Processor (Skylake)` メインクロック：`2693.670 MHZ`
- **サーバー**：`emqx-centos7-v4.0.2.zip`
- **プレッシャー**：`emqtt-bench v0.3.1`、各プレッシャーは10個のネットワークカードを設定し、接続テストで多数のMQTTクライアント接続を確立

トポロジー構成は以下の通りです。

![ベンチマークトポロジー](./assets/benchmark_topology.png)

### チューニング

クライアントプレッシャーとサーバーの両方でシステムパラメータのチューニングが必要です。詳細は[Tuning guide](../performance/tune.md)を参照してください。

### 接続テスト

システムチューニング後、EMQXを起動し、`bench1`の各ネットワークカードで5万接続ずつ開始します。合計で50万接続となります。

```bash
./emqtt_bench -h 192.168.0.99 -c 50000 --ifaddr 192.168.0.100
./emqtt_bench -h 192.168.0.99 -c 50000 --ifaddr 192.168.0.101
./emqtt_bench -h 192.168.0.99 -c 50000 --ifaddr 192.168.0.102
./emqtt_bench -h 192.168.0.99 -c 50000 --ifaddr 192.168.0.103
./emqtt_bench -h 192.168.0.99 -c 50000 --ifaddr 192.168.0.104
./emqtt_bench -h 192.168.0.99 -c 50000 --ifaddr 192.168.0.105
./emqtt_bench -h 192.168.0.99 -c 50000 --ifaddr 192.168.0.106
./emqtt_bench -h 192.168.0.99 -c 50000 --ifaddr 192.168.0.107
./emqtt_bench -h 192.168.0.99 -c 50000 --ifaddr 192.168.0.108
./emqtt_bench -h 192.168.0.99 -c 50000 --ifaddr 192.168.0.109
```

同様の操作を`bench2`でも実施します。

すべての接続が確立したら、`./bin/emqx ctl listeners`を実行し、EMQXの接続数情報を確認します。

```bash
listener on mqtt:tcp:0.0.0.0:1883
  acceptors       : 8
  max_conns       : 1024000
  current_conn    : 1000000
  shutdown_count  : []
```

### スループットテスト

同様に、まずEMQXを起動し、`bench1`で500のサブスクライブクライアントを開始します。

```bash
./emqtt_bench sub -t t -h 192.168.0.99 -c 500
```

次に、`bench2`で20のパブリッシャーを起動し、1秒あたり10メッセージをパブリッシュします。

```bash
./emqtt_bench pub -t t -h 192.168.0.99 -c 20 -I 100
```

`bench1`のサブスクライブクライアントに戻ると、現在のメッセージ受信レートを確認できます。

```bash
recv(28006): total=2102563, rate=99725(msg/sec)
```
