# パフォーマンスチューニング（Linux）

IoTアプリケーションでは通常、多数のデバイスと大量のデータが存在するため、EMQXはMQTTサーバーとして膨大な数のデバイスから生成されるメッセージの処理と配信を担います。このような状況では、EMQXのシステムパフォーマンスの最適化が特に重要となります。

最適化の目的は、以下のパフォーマンス面を最大化することです。

- **メッセージ処理能力**：EMQXがメッセージを迅速かつ効率的に処理し、デバイスからのメッセージを速やかに受信、処理、転送できる能力を向上させること。
- **スループット**：システムがデバイスからのメッセージをタイムリーに処理・配信できるようスループットを増加させること。
- **安定性**：高負荷時のレイテンシを低減し、システムの応答性を向上させ、クラッシュや障害のリスクを減らすこと。

本ページでは、ベンチマークおよびデプロイメントに向けた一般的なチューニングの提案を紹介します。

## スワップの無効化

Linuxのスワップパーティションは、Erlang仮想マシンに対して非決定的なメモリレイテンシを引き起こし、システムの安定性に大きく影響します。スワップは恒久的に無効化することを推奨します。

- すぐにスワップを無効化するには、以下のコマンドを実行してください。  
  `sudo swapoff -a`

- 恒久的に無効化するには、`/etc/fstab`内のスワップ行をコメントアウトし、ホストを再起動してください。

## Linuxカーネルのチューニング

システム全体の最大オープンファイルハンドル数の設定：

```bash
# システム全体で200万
sysctl -w fs.file-max=2097152
sysctl -w fs.nr_open=2097152
echo 2097152 > /proc/sys/fs/nr_open
```

現在のセッションでのオープンファイルハンドル数の制限：

```bash
ulimit -n 2097152
```

### `/etc/sysctl.conf`

`fs.file-max`の設定を永続化するため、`/etc/sysctl.conf`に以下を追加してください。

```bash
fs.file-max = 2097152
```

サービスの最大ファイルハンドル数を`/etc/systemd/system.conf`で設定：

```bash
DefaultLimitNOFILE=2097152
```

### `emqx.service`

Linuxディストリビューションに応じて以下のいずれかのパスにある`emqx.service`ファイルで、emqxサービスの最大ファイルハンドル数を設定します。

- `/usr/lib/systemd/system/emqx.service`
- `/lib/systemd/system/emqx.service`

```bash
LimitNOFILE=2097152
```

### `/etc/security/limits.conf`

ユーザーの最大オープンファイルハンドル数を永続化するため、`/etc/security/limits.conf`に以下を追加してください。

```bash
*      soft   nofile      2097152
*      hard   nofile      2097152
```

## TCPネットワークのチューニング

受け入れ可能な接続のバックログ数を増やします。

```bash
sysctl -w net.core.somaxconn=32768
sysctl -w net.ipv4.tcp_max_syn_backlog=16384
sysctl -w net.core.netdev_max_backlog=16384
```

ローカルポート範囲の設定：

```bash
sysctl -w net.ipv4.ip_local_port_range='1024 65535'
```

TCPソケットの読み書きバッファ：

```bash
sysctl -w net.core.rmem_default=262144
sysctl -w net.core.wmem_default=262144
sysctl -w net.core.rmem_max=16777216
sysctl -w net.core.wmem_max=16777216
sysctl -w net.core.optmem_max=16777216

#sysctl -w net.ipv4.tcp_mem='16777216 16777216 16777216'
sysctl -w net.ipv4.tcp_rmem='1024 4096 16777216'
sysctl -w net.ipv4.tcp_wmem='1024 4096 16777216'
```

TCP接続トラッキング：

```bash
sysctl -w net.nf_conntrack_max=1000000
sysctl -w net.netfilter.nf_conntrack_max=1000000
sysctl -w net.netfilter.nf_conntrack_tcp_timeout_time_wait=30
```

TIME-WAITバケットプールのサイズ、リサイクル、および再利用：

```bash
sysctl -w net.ipv4.tcp_max_tw_buckets=1048576

# 以下のオプションの有効化は推奨されません。NAT環境下で接続リセットを引き起こす可能性があります。
# sysctl -w net.ipv4.tcp_tw_recycle=1
# sysctl -w net.ipv4.tcp_tw_reuse=1
```

FIN-WAIT-2ソケットのタイムアウト：

```bash
sysctl -w net.ipv4.tcp_fin_timeout=15
```

## Erlang VMのチューニング

`etc/emqx.conf`ファイルでErlang VMのチューニングと最適化を行います。

```bash
## システムで同時に存在可能な最大ポート数を設定
node.max_ports = 2097152
```

## EMQXのチューニング

### リスナーアクセプター

`etc/base.hocon`でアクセプタープールサイズと`max_connections`制限を調整します。

接続処理を最適化するために、`etc/emqx.conf`の設定ファイルでアクセプタープールサイズと`max_connections`制限を調整できます。

例：TCPリスナーの設定

```bash
## TCPリスナー
listeners.tcp.$name.acceptors = 64
listeners.tcp.$name.max_connections = 1024000
```

- `acceptors`：着信接続を処理するアクセプタープロセスの数
- `max_connections`：許可される同時接続の最大数

### ディストリビューションポートのバッファサイズ

多数のレプリカノードを持つ大規模クラスターでは、コアノードで`node.dist_buffer_size`パラメータを設定し、ディストリビューションポートのバッファサイズを調整することを推奨します。

```bash
# バッファサイズ（KB単位）。以下は最大約2GBに設定しています。
node.dist_buffer_size=2097151
```

この調整により、コアノードは大量のクライアント再接続によるトラフィックの急増をより適切に処理できます。

また、以下のような警告ログが出力される場合は、このバッファサイズを増やすことで問題を軽減できます。

```
[warning] msg: busy_dist_port ...
```

## クライアントマシンのチューニング

EMQXのベンチマークを行うクライアントマシンのチューニング例：

```bash
sysctl -w net.ipv4.ip_local_port_range="500 65535"
echo 1000000 > /proc/sys/fs/nr_open
ulimit -n 100000
```

### MQTTベンチマーク

同時接続数のテストツール：[emqtt_bench](https://github.com/emqx/emqtt_bench)をご利用ください。
