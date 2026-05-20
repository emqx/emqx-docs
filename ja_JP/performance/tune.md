# パフォーマンスチューニング（Linux）

IoTアプリケーションでは通常、多数のデバイスと大量のデータが存在するため、MQTTサーバーであるEMQXは膨大な数のデバイスから生成されるメッセージの処理と配信を担います。このような状況では、EMQXシステムのパフォーマンス最適化が特に重要となります。

最適化の目的は、以下のパフォーマンスを最大化することです。

- **メッセージ処理能力**：EMQXがメッセージを迅速かつ効率的に処理し、デバイスから生成されるメッセージを速やかに受信、処理、転送できる能力を向上させます。
- **スループット**：システムがデバイスからのメッセージをタイムリーに処理・配信できるようスループットを向上させます。
- **安定性**：高負荷時のレイテンシを低減し、システムの応答性を改善するとともに、システムのクラッシュや障害のリスクを減らします。

本ページでは、ベンチマークおよびデプロイメントにおける一般的なチューニングの提案を示します。

## スワップの無効化

Linuxのスワップパーティションは、Erlang仮想マシンに対して非決定的なメモリレイテンシを引き起こし、システムの安定性に大きく影響を与える可能性があります。スワップは恒久的に無効化することを推奨します。

- 即時にスワップを無効化するには、以下のコマンドを実行してください。

  ```bash
  sudo swapoff -a
  ```

- 恒久的にスワップを無効化するには、`/etc/fstab`の`swap`行をコメントアウトし、ホストを再起動してください。

## Linuxカーネルのチューニング

システム全体で開ける最大ファイルハンドル数の制限：

```bash
# システム全体で200万
sysctl -w fs.file-max=2097152
sysctl -w fs.nr_open=2097152
echo 2097152 > /proc/sys/fs/nr_open
```

現在のセッションで開けるファイルハンドル数の制限：

```bash
ulimit -n 2097152
```

### `/etc/sysctl.conf`

`fs.file-max`の設定を永続化するには、`/etc/sysctl.conf`に以下を追加します。

```bash
fs.file-max = 2097152
```

サービスの最大ファイルハンドル数を`/etc/systemd/system.conf`に設定：

```bash
DefaultLimitNOFILE=2097152
```

### `emqx.service`

使用しているLinuxディストリビューションに応じて、以下のいずれかのパスにある`emqx.service`ファイルでemqxサービスの最大ファイルハンドル数を設定します。

- `/usr/lib/systemd/system/emqx.service`
- `/lib/systemd/system/emqx.service`

```bash
LimitNOFILE=2097152
```

### `/etc/security/limits.conf`

ユーザーの最大開放ファイルハンドル数を`/etc/security/limits.conf`に永続化します。

```bash
*      soft   nofile      2097152
*      hard   nofile      2097152
```

## TCPネットワークのチューニング

受け入れ待ち接続のバックログ数を増やします。

```bash
sysctl -w net.core.somaxconn=32768
sysctl -w net.ipv4.tcp_max_syn_backlog=16384
sysctl -w net.core.netdev_max_backlog=16384
```

ローカルポートの範囲設定：

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

TCPコネクション追跡：

```bash
sysctl -w net.nf_conntrack_max=1000000
sysctl -w net.netfilter.nf_conntrack_max=1000000
sysctl -w net.netfilter.nf_conntrack_tcp_timeout_time_wait=30
```

TIME-WAITバケットプールの設定、リサイクルおよび再利用：

```bash
sysctl -w net.ipv4.tcp_max_tw_buckets=1048576

# 以下の設定は推奨されません。NAT環境下で接続リセットを引き起こす可能性があります。
# sysctl -w net.ipv4.tcp_tw_recycle=1
# sysctl -w net.ipv4.tcp_tw_reuse=1
```

FIN-WAIT-2ソケットのタイムアウト設定：

```bash
sysctl -w net.ipv4.tcp_fin_timeout=15
```

## Erlang VMのチューニング

`etc/emqx.conf`ファイルでErlang VMのチューニングを行います。

```bash
## このシステムで同時に存在可能な最大ポート数を設定
node.max_ports = 2097152
```

## EMQXのチューニング

### リスナーアクセプター

`etc/base.hocon`にてアクセプタープールサイズおよび`max_connections`制限を調整します。

接続処理を最適化するために、`etc/emqx.conf`設定ファイルでアクセプタープールサイズと`max_connections`制限を調整可能です。

TCPリスナーの設定例：

```bash
## TCPリスナー
listeners.tcp.$name.acceptors = 64
listeners.tcp.$name.max_connections = 1024000
```

`acceptors`：受信接続を処理するアクセプタープロセスの数。

`max_connections`：許可される同時接続の最大数。

### ディストリビューションポートバッファサイズ

多数のレプリカノードを持つ大規模クラスターでは、コアノードの`node.dist_buffer_size`パラメータを調整してディストリビューションポートのバッファサイズをチューニングすることを推奨します。

```bash
# バッファサイズ（KB単位）。以下は最大約2GBに設定。
node.dist_buffer_size=2097151
```

この調整により、コアノードは大量のクライアント再接続によるトラフィックの急増をより良く処理できます。

また、以下のような警告ログが出る場合は、このバッファサイズを増やすことで問題を緩和できます。

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
