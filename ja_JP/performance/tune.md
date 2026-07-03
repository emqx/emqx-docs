# パフォーマンスチューニング（Linux）

IoTアプリケーションでは通常、多数のデバイスと大量のデータが存在するため、EMQXはMQTTサーバーとして膨大な数のデバイスから生成されるメッセージの処理と配信を担います。このような状況では、EMQXシステムのパフォーマンス最適化が特に重要となります。

最適化の目的は、以下のパフォーマンス面を最大化することです。

- **メッセージ処理能力**：EMQXがメッセージを迅速かつ効率的に処理し、デバイスからのメッセージを素早く受信、処理、転送できる能力を向上させること。
- **スループット**：システムがデバイスからのメッセージをタイムリーに処理・配信できるようにスループットを向上させること。
- **安定性**：高負荷時のレイテンシを低減し、システムの応答性を改善するとともに、クラッシュや障害のリスクを下げること。

本ページでは、ベンチマークやデプロイメントにおける一般的なチューニングの提案を紹介します。

## スワップの無効化

Linuxのスワップ領域はErlang仮想マシンに対して非決定的なメモリレイテンシを引き起こし、システムの安定性に大きく影響します。スワップは恒久的に無効化することを推奨します。

- すぐにスワップを無効化するには、以下のコマンドを実行します。  
  `sudo swapoff -a`

- 恒久的にスワップを無効化するには、`/etc/fstab`内のスワップ行をコメントアウトし、ホストを再起動してください。

## Linuxカーネルチューニング

システム全体の最大オープンファイルハンドル数の制限設定：

```bash
# システム全体で200万件
sysctl -w fs.file-max=2097152
sysctl -w fs.nr_open=2097152
echo 2097152 > /proc/sys/fs/nr_open
```

現在のセッションにおけるオープンファイルハンドル数の制限：

```bash
ulimit -n 2097152
```

### `/etc/sysctl.conf`

`fs.file-max`の設定を永続化するため、`/etc/sysctl.conf`に以下を追記します。

```bash
fs.file-max = 2097152
```

サービスの最大ファイルハンドル数を`/etc/systemd/system.conf`に設定：

```bash
DefaultLimitNOFILE=2097152
```

### `emqx.service`

Linuxディストリビューションにより異なりますが、以下のいずれかのパスにある`emqx.service`ファイルに最大ファイルハンドル数を設定します。

- `/usr/lib/systemd/system/emqx.service`
- `/lib/systemd/system/emqx.service`

```bash
LimitNOFILE=2097152
```

### `/etc/security/limits.conf`

ユーザーの最大オープンファイルハンドル数を永続化するため、`/etc/security/limits.conf`に以下を追記します。

```bash
*      soft   nofile      2097152
*      hard   nofile      2097152
```

### Transparent HugePages（THP）の無効化

EMQXは組み込みのデータベースワークロードを含みます。他のデータベースシステムと同様に、EMQX起動前にTransparent HugePages（THP）を無効化することを強く推奨します。

```bash
echo never > /sys/kernel/mm/transparent_hugepage/enabled
echo never > /sys/kernel/mm/transparent_hugepage/defrag
```

メモリ容量が16GB以上の高メモリマシンで長時間EMQXを稼働させた際に以下の症状が発生する場合は、THPを無効化してTHP関連の問題を除外してください。

- メッセージレイテンシの不安定化
- 予期しないメモリ使用量の急増
- EMQXの`long_schedule`警告ログ
- EMQXの`runq_overload`アラーム

クラスター運用中の場合は、比較のためにまず一部のノードでTHPを無効化してください。なお、ワークロードによってはTHPを有効にした方が良い場合もあります。

これらの設定を再起動後も維持するには、OSのドキュメントを参照して適切な方法で設定してください。

## TCPネットワークチューニング

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

TCPコネクショントラッキング：

```bash
sysctl -w net.nf_conntrack_max=1000000
sysctl -w net.netfilter.nf_conntrack_max=1000000
sysctl -w net.netfilter.nf_conntrack_tcp_timeout_time_wait=30
```

TIME-WAITバケットプール、リサイクル、再利用：

```bash
sysctl -w net.ipv4.tcp_max_tw_buckets=1048576

# 以下の設定は推奨されません。NAT環境で接続リセットを引き起こす可能性があります。
# sysctl -w net.ipv4.tcp_tw_recycle=1
# sysctl -w net.ipv4.tcp_tw_reuse=1
```

FIN-WAIT-2ソケットのタイムアウト：

```bash
sysctl -w net.ipv4.tcp_fin_timeout=15
```

## Erlang VMチューニング

`etc/emqx.conf`ファイルでErlang VMのチューニングを行います。

```bash
## システムで同時に存在可能な最大ポート数を設定
node.max_ports = 2097152
```

## EMQXチューニング

### リスナーアクセプター

`etc/base.hocon`にてアクセプタープールサイズと`max_connections`の制限を調整します。

接続処理を最適化するため、`etc/emqx.conf`の設定ファイルでアクセプタープールサイズと`max_connections`制限を調整可能です。

TCPリスナーの例：

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
# KB単位のバッファサイズ。以下は最大約2GBに設定。
node.dist_buffer_size=2097151
```

この調整により、コアノードは大量のクライアント再接続によるトラフィックスパイクをより適切に処理できます。

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

同時接続テスト用ツール：[emqtt_bench](https://github.com/emqx/emqtt_bench)をご利用ください。
