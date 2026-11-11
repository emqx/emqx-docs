# パフォーマンスチューニング（Linux）

IoTアプリケーションでは通常、多数のデバイスと大量のデータが存在するため、MQTTサーバーであるEMQXは膨大な数のデバイスから生成されるメッセージの処理と配信を担います。このような状況では、EMQXのシステムパフォーマンスの最適化が特に重要となります。

最適化の目的は、以下のパフォーマンス面を最大化することです。

- **メッセージ処理能力**：EMQXがメッセージを迅速かつ効率的に処理し、デバイスから生成されたメッセージを速やかに受信・処理・転送できる能力を向上させること。
- **スループット**：システムがデバイスからのメッセージをタイムリーに処理・配信できるスループットを向上させること。
- **安定性**：高負荷時のレイテンシを低減し、システムの応答性を改善するとともに、システムクラッシュや障害のリスクを低減すること。

本ページでは、ベンチマークおよびデプロイメントに向けた一般的なチューニングの提案を紹介します。

## スワップの無効化

LinuxのスワップパーティションはErlang仮想マシンに対して非決定的なメモリレイテンシを引き起こし、システムの安定性に大きな影響を与える可能性があります。スワップは恒久的に無効化することを推奨します。

- 即時にスワップを無効化するには、以下のコマンドを実行してください。

  ```bash
  sudo swapoff -a
  ```

- 恒久的にスワップを無効化するには、`/etc/fstab`のスワップ行をコメントアウトし、ホストを再起動してください。

## Linuxカーネルのチューニング

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

`fs.file-max`の設定を永続化するために、`/etc/sysctl.conf`に以下を追加します。

```bash
fs.file-max = 2097152
```

サービスの最大ファイルハンドル数を設定するには、`/etc/systemd/system.conf`に以下を設定します。

```bash
DefaultLimitNOFILE=2097152
```

### `emqx.service`

Linuxディストリビューションにより異なる以下のいずれかのパスにある`emqx.service`ファイルで、emqxサービスの最大ファイルハンドル数を設定します。

- `/usr/lib/systemd/system/emqx.service`
- `/lib/systemd/system/emqx.service`

```bash
LimitNOFILE=2097152
```

### `/etc/security/limits.conf`

ユーザーの最大オープンファイルハンドル数を永続化するため、`/etc/security/limits.conf`に以下を追加します。

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

16GB以上のメモリを搭載したマシンで長時間EMQXを稼働させた際に以下の症状が発生する場合は、THPを無効化してTHP関連の問題を除外してください。

- メッセージレイテンシの不安定化
- 予期しないメモリ使用量の急増
- EMQXの`long_schedule`警告ログ
- EMQXの`runq_overload`アラーム

クラスターを運用している場合は、まず一部のノードでTHPを無効化して比較検証してください。なお、一部のワークロードではTHPを有効にしたほうが性能が向上する場合もあります。

これらの設定を再起動後も持続させるには、OSのドキュメントを参照して適切な方法を確認してください。

## TCPネットワークのチューニング

受け入れ可能な接続のバックログ数を増加させます。

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

TIME-WAITバケットプール、リサイクルおよび再利用：

```bash
sysctl -w net.ipv4.tcp_max_tw_buckets=1048576

# 以下のオプションは推奨されません。NAT環境下で接続リセットを引き起こす可能性があります。
# sysctl -w net.ipv4.tcp_tw_recycle=1
# sysctl -w net.ipv4.tcp_tw_reuse=1
```

FIN-WAIT-2ソケットのタイムアウト：

```bash
sysctl -w net.ipv4.tcp_fin_timeout=15
```

TCPパケットの再送回数を削減：

```bash
sysctl -w net.ipv4.tcp_retries2=5
```

## Erlang VMのチューニング

EMQX 6.3.0以降、EMQXはノードに割り当てられたCPUリソースに基づき、Erlang VMのリソース制限を自動設定します。`etc/emqx.conf`で以下の設定を行い、ノード再起動後に反映されます。

### ポートおよびプロセス制限

`node.max_ports`はErlang VMが同時に開けるファイルやソケットの最大数を制御します。デフォルトは`auto`で、Erlang VMのポート制限（`+Q`）は以下のように設定されます。

- 論理CPUが1〜8個のノードでは、CPUあたり65,536ポート。
- 論理CPUが8個を超えるノードでは、合計1,048,576ポート。

::: warning 重要なお知らせ
以前のEMQXバージョンからアップグレードする場合、論理CPUが8個以下のノードはより低いポート制限で起動します。自動計算された制限が接続数要件を満たさない場合は、アップグレード前に明示的に`node.max_ports`を設定し、ノードを再起動してください。
:::

EMQXはErlangプロセス制限（`+P`）を、解決された`node.max_ports`の2倍に設定します。`node.process_limit`を明示的に設定した場合は、計算されたプロセス制限より大きい値のみ有効となります。

高並列ワークロードで自動計算されたポート制限が不足する場合は、`node.max_ports`を明示的に設定してください。例：

```hocon
node.max_ports = 2097152
```

`node.max_ports`を増やす前に、OSのファイルディスクリプタ制限や利用可能メモリが設定値をサポートできることを確認してください。EMQXダッシュボードのノード監視ページで有効なポートおよびプロセス制限を確認できます。

### Erlangスケジューラ

`node.schedulers`はErlang VMの`+S`フラグを通じてスケジューラ数を制御します。デフォルトは`auto`で、Erlang VMが利用可能な論理プロセッサ数（コンテナのCPUリソースも含む）を使用します。

同一ホスト上の他のワークロードのためにCPUリソースを確保したい場合など、検出された値を上書きする必要がある場合にのみ、正の整数で`node.schedulers`を設定してください。

## EMQXのチューニング

### リスナーのアクセプター

`etc/base.hocon`でアクセプタープールサイズおよび`max_connections`制限を調整します。

接続処理を最適化するため、`etc/emqx.conf`でアクセプタープールサイズと`max_connections`制限を調整可能です。

TCPリスナーの例：

```bash
## TCPリスナー
listeners.tcp.$name.acceptors = 64
listeners.tcp.$name.max_connections = 1024000
```

- `acceptors`：着信接続を処理するアクセプタープロセスの数。
- `max_connections`：許可される同時接続の最大数。

### クラスター間通信ポートのバッファサイズ

多数のレプリカノードを持つ大規模クラスターでは、コアノードの`node.dist_buffer_size`パラメータを調整して分散ポートのバッファサイズをチューニングすることを推奨します。

```bash
# KB単位のバッファサイズ。以下は最大約2GBに設定。
node.dist_buffer_size=2097151
```

この調整により、大量のクライアント再接続によるトラフィック急増をコアノードがより適切に処理できます。

また、以下のような警告ログが出る場合は、このバッファサイズを増やすことで問題を緩和できます。

```
[warning] msg: busy_dist_port ...
```

### クラスターRPCトラフィックの圧縮 <Badge type="tip" text="6.3.3+" />

EMQXはノード間のリモートプロシージャコール（RPC）接続で送信されるデータ（転送されるMQTTメッセージを含む）を圧縮できます。圧縮によりノード間のRPCトラフィックが削減されますが、送信側・受信側の両ノードでCPU負荷が増加します。ノード間の帯域幅が限られている場合や、可用性ゾーンを跨ぐクラスターなどでノード間データ転送に課金が発生する場合に圧縮を有効化してください。

`etc/emqx.conf`でRPC圧縮を設定します。

```hocon
rpc {
  compress_level = 1
  compress_threshold = 1KB
}
```

`rpc`ルートは読み取り専用です。RPC圧縮を有効にしたい各ノードで設定し、ノードを再起動して反映させてください。

- `rpc.compress_level`は`0`から`9`までの整数を受け付けます。
  - `0`（デフォルト）は圧縮無効。既存のデプロイメントはアップグレード後も同様の動作を維持します。
  - `1`は最も高速な圧縮レベルで推奨の開始点です。
  - `9`は最小の出力を生成しますが、大きなメッセージでCPU負荷が大幅に増加します。通常はわずかなサイズ削減に留まります。

- `rpc.compress_threshold`は圧縮を開始するRPCパケットの最小サイズを設定します。デフォルトは`1KB`で、これより小さいパケットは圧縮されません。`0`に設定するとすべてのパケットを圧縮します。この設定は`rpc.compress_level`が`0`より大きい場合にのみ有効です。

閾値は個々のMQTTメッセージではなく、完全なRPCパケットに適用されます。RPCは同一ノード宛の複数メッセージをバッチ送信するため、各メッセージが閾値未満でもパケット全体は閾値を超える場合があります。

すでに圧縮または暗号化されたペイロードはRPC圧縮によってサイズが小さくなることはなく、圧縮するとCPU使用率が増加するだけです。

::: tip
圧縮レベルは`1`、閾値はデフォルトの`1KB`から開始し、代表的なワークロードでCPU使用率とノード間RPCトラフィックを計測してから、圧縮レベルの引き上げや閾値の引き下げを検討してください。
:::

## クライアントマシンのチューニング

EMQXのベンチマーク用にクライアントマシンをチューニングします。

```bash
sysctl -w net.ipv4.ip_local_port_range="500 65535"
echo 1000000 > /proc/sys/fs/nr_open
ulimit -n 100000
```

### MQTTベンチマーク

同時接続数のテストツール：[emqtt_bench](https://github.com/emqx/emqtt_bench)をご利用ください。
