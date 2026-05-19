# ノード退避とクラスター負荷再分散

MQTTはステートフルな長時間接続アクセスプロトコルであり、一度確立された接続は簡単には切断されません。そのため、クラスターのノードのアップグレード、メンテナンス、スケーリングはより困難になります。EMQXは、ユーザーのクラスター運用・保守を支援するために、ノード退避およびクラスター負荷再分散機能を提供しています。

## ノード退避

クラスター内のノードをメンテナンスやアップグレードする必要がある場合、ノードを直接シャットダウンすると接続やセッションが失われ、データ損失が発生します。さらに、この操作により多数のデバイスが一時的にオフラインになり再接続が発生してサーバー負荷が増加し、全体のビジネスに影響を与える可能性があります。

そこでEMQXは、ノード退避機能を提供し、ノードをシャットダウンする前にそのノードのすべての接続およびセッションデータをクラスター内の他のノードに移行することで、全体のビジネスへの影響を軽減します。

### 動作の仕組み

ノード退避は以下の順序で動作します：

1. 退避対象のノードは新規接続の受け入れを停止します。
2. 退避対象のノードは、設定されたレート（`conn-evict-rate`で指定）で現在のクライアントを徐々に切断します。切断されたクライアントは再接続機構を利用してクラスター内の他のノード（ターゲットノード）に接続します。再接続機構はプロトコルバージョンにより異なります：
   - MQTT v3.1/v3.1.1クライアント：ロードバランシング戦略で指定され、クライアント側で再接続機構を有効にする必要があります；
   - MQTT v5.0クライアント：`redirect-to`パラメータで指定します。
3. ターゲットノードがクライアントとの再接続を完了し、セッションを引き継ぐのを待ちます（`wait-takeover`で指定）。
4. 再接続待機時間経過後、退避対象ノードに残っている未引き継ぎのセッションをターゲットノードに移行します：
   - セッション移行先のノードは`migrate-to`で指定します；
   - セッション移行速度は`sess-evict-rate`で指定します。

退避はいつでも停止可能です。退避中に退避対象ノードがシャットダウンされた場合、ノード再起動後に退避処理が再開されます。

### CLIによるノード退避の開始と停止

CLIコマンドでノード退避の開始、退避状況の取得、退避の停止が可能です。

#### ノード退避の開始

以下のCLIコマンドでノード退避を開始できます。`--evacuation`パラメータは退避操作であることを示します：

```bash
./bin/emqx ctl rebalance start --evacuation \
    [--wait-health-check Secs] \
    [--redirect-to "Host1:Port1 Host2:Port2 ..."] \
    [--conn-evict-rate CountPerSec] \
    [--migrate-to "node1@host1 node2@host2 ..."] \
    [--wait-takeover Secs] \
    [--sess-evict-rate CountPerSec]
```

| パラメータ             | 型               | 説明                                                                                   |
| --------------------- | ---------------- | -------------------------------------------------------------------------------------- |
| `--wait-health-check` | 正の整数         | ノードがロードバランサー（LB）からアクティブなバックエンドノードリストから除外されるのを待つ時間（秒、デフォルト60秒）。この待機時間経過後に退避処理が開始され、ソースノードは新規接続を拒否し始めます。 |
| `--redirect-to`       | 文字列           | MQTT 5.0クライアント向けの再接続時のリダイレクト先サーバーアドレス。詳細は[MQTT 5.0仕様 - サーバーリダイレクション](https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901255)を参照してください。 |
| `--conn-evict-rate`   | 正の整数         | クライアント切断レート（接続数/秒）。デフォルトは1秒あたり500接続。                      |
| `--migrate-to`        | 文字列           | セッションを退避させるノードのスペースまたはカンマ区切りリスト。                          |
| `--wait-takeover`     | 正の整数         | セッション退避開始までの待機時間（秒）。単位は秒、デフォルトは60秒。                      |
| `--sess-evict-rate`   | 正の整数         | セッション退避レート（セッション数/秒）。デフォルトは1秒あたり500セッション。              |

**コード例**

ノード `emqx@127.0.0.1` 上のクライアントを `emqx2@127.0.0.1` と `emqx3@127.0.0.1` に移行したい場合、`emqx@127.0.0.1` ノードで以下のコマンドを実行します：

```bash
./bin/emqx ctl rebalance start --evacuation \
	--wait-health-check 60 \
	--wait-takeover 200 \
	--conn-evict-rate 30 \
	--sess-evict-rate 30 \
	--migrate-to "emqx2@127.0.0.1 emqx3@127.0.0.1"
Rebalance(evacuation) started
```

このコマンドは既存のクライアントを1秒あたり30接続の速度で切断します。すべての接続が切断された後、200秒間待機し、その間にクライアントセッションが再接続されたノードに移行されます。その後、残りのセッションを1秒あたり30セッションの速度で `emqx2@127.0.0.1` と `emqx3@127.0.0.1` ノードに移行します。

#### 退避状況の取得

退避状況は以下のCLIコマンドで取得できます：

```bash
./bin/emqx ctl rebalance status
```

返される結果の例：

```bash
./bin/emqx ctl rebalance status
--------------------------------------------------------------------
Node 'emqx@node1.emqx.io': evacuation
Rebalance state: evicting_conns
Connection eviction rate: 3 connections/second
Session eviction rate: 3 sessions/second
Connection goal: 0
Session goal: 0
Session recipient nodes: ['emqx@node2.emqx.io','emqx@node3.emqx.io']
Channel statistics:
  current_connected: 9
  current_sessions: 30
  initial_connected: 30
  initial_sessions: 30
```

#### ノード退避の停止

退避を停止するには以下のCLIコマンドを使用します：

```bash
./bin/emqx ctl rebalance stop
```

返される結果の例：

```bash
./bin/emqx ctl rebalance stop
Rebalance(evacuation) stopped
```

### HTTP APIによるノード退避の開始／停止

HTTP APIでもノード退避の開始・停止が可能で、退避対象ノードをパラメータで指定する必要があります。詳細は[APIドキュメント](https://docs.emqx.com/en/enterprise/v5.1/admin/api-docs.html)を参照してください。

## 負荷再分散

MQTTがステートフルな長時間接続プロトコルであるため、接続は確立後に簡単には切断されません。ノードをスケールアウトしても、既存の接続は自動的に新規追加ノードに移動しません。そのため、新規クライアント接続が少ない場合、追加ノードは長期間十分に活用されない可能性があります。このような場合、高負荷ノードから低負荷ノードへ手動で接続を移行し、クラスターの負荷バランスを取る必要があります。

<img src="./assets/rebalancing.png" alt="負荷再分散" style="zoom:50%;" />

### 動作の仕組み

負荷再分散は複数ノードが関与するため、より複雑なプロセスです。

任意のノードでクラスター負荷再分散タスクを開始できます。EMQXは各ノードの現在の接続負荷に基づいて必要な接続移行計画を自動計算し、高負荷ノードから低負荷ノードへ対応する数の接続およびセッションを移行してノード間の負荷バランスを実現します。ワークフローは以下の通りです：

1. 移行計画を計算し、再分散に関与するノード（`--nodes`で指定）をソースノードとターゲットノードに分割：
   - ソースノード：高負荷ノード
   - ターゲットノード：低負荷ノード
2. ソースノードで新規接続の受け入れを停止。
3. 一定時間（`wait-health-check`で指定）待機し、ロードバランサー（LB）がソースノードをアクティブなバックエンドノードリストから除外するのを待つ。
4. ソースノード上の接続クライアントを徐々に切断し、平均接続数がターゲットノードと同等になるまで続ける。
5. ターゲットノードがクライアントと再接続し、セッションを引き継ぐのを待つ（`wait-takeover`で指定）。
6. 再接続待機時間経過後、ソースノードは残っている未引き継ぎセッションをターゲットノードに、`sess-evict-rate`で指定された速度で移行する。

これで負荷再分散タスクは完了し、ソースノードは通常状態に戻ります。

::: tip

負荷再分散は一時的な処理です。参加ノードのいずれかがクラッシュすると、すべてのノードで処理が中断されます。

:::

### CLIによる負荷再分散の開始と停止

CLIコマンドで負荷再分散の開始、状況取得、停止が可能です。

#### 負荷再分散の開始

負荷再分散開始コマンドは以下のフィールドを含みます：

```bash
rebalance start \
    [--nodes "node1@host1 node2@host2"] \
    [--wait-health-check Secs] \
    [--conn-evict-rate ConnPerSec] \
    [--abs-conn-threshold Count] \
    [--rel-conn-threshold Fraction] \
    [--conn-evict-rate ConnPerSec] \
    [--wait-takeover Secs] \
    [--sess-evict-rate CountPerSec] \
    [--abs-sess-threshold Count] \
    [--rel-sess-threshold Fraction]
```

| フィールド               | 型               | 説明                                                                                   |
| ---------------------- | ---------------- | -------------------------------------------------------------------------------------- |
| `--nodes`              | 文字列           | 負荷再分散に参加するノードのスペースまたはカンマ区切りリスト。コマンド実行ノード（コーディネーター）を含む場合も含まない場合もあります。 |
| `--wait-health-check`  | 正の整数         | ノードがロードバランサー（LB）からアクティブなバックエンドノードリストから除外されるのを待つ時間（秒、デフォルト60秒）。この待機時間経過後に負荷再分散処理が開始されます。 |
| `--conn-evict-rate`    | 正の整数         | ソースノードでのクライアント切断レート。デフォルトは1秒あたり500接続。               |
| `--abs-conn-threshold` | 正の整数         | 接続バランスチェックの絶対閾値。デフォルトは1000。                                   |
| `--rel-conn-threshold` | 数値<br /> > 1.0 | 接続バランスチェックの相対閾値。デフォルトは1.1。                                   |
| `--wait-takeover`      | 正の整数         | すべての接続切断後、クライアントが再接続しセッションを引き継ぐまでの待機時間（秒、デフォルト60秒）。 |
| `--sess-evict-rate`    | 正の整数         | ソースノードでのセッション退避レート。デフォルトは1秒あたり500セッション。            |
| `--abs-sess-threshold` | 正の整数         | セッションバランスチェックの絶対閾値。デフォルトは1000。                             |
| `--rel-sess-threshold` | 数値<br /> > 1.0 | セッションバランスチェックの相対閾値。デフォルトは1.1。                             |

**セッションバランスのチェック**

接続は以下の条件を満たすとバランスが取れているとみなされます：

```bash
avg(DonorConns) < avg(RecipientConns) + abs_conn_threshold
OR
avg(DonorConns) < avg(RecipientConns) * rel_conn_threshold
```

切断されたセッションにも同様のルールが適用されます。

**例**

3つのノード `emqx@127.0.0.1`、`emqx2@127.0.0.1`、`emqx3@127.0.0.1` 間で負荷再分散を行う場合、以下のコマンドを使用します：

```bash
./bin/emqx ctl rebalance start \
	--wait-health-check 10 \
	--wait-takeover 60  \
	--conn-evict-rate 5 \
	--sess-evict-rate 5 \
	--abs-conn-threshold 30 \
	--abs-sess-threshold 30 \
	--nodes "emqx1@127.0.0.1 emqx2@127.0.0.1 emqx3@127.0.0.1"
Rebalance started
```

#### 負荷再分散状況の取得

負荷再分散状況は以下のCLIコマンドで取得できます：

```bash
./bin/emqx ctl rebalance status
```

**例**

```bash
./bin/emqx ctl rebalance status
--------------------------------------------------------------------
Node 'emqx1@127.0.0.1': rebalance coordinator
Rebalance state: evicting_conns
Coordinator node: 'emqx1@127.0.0.1'
Donor nodes: ['emqx2@127.0.0.1','emqx3@127.0.0.1']
Recipient nodes: ['emqx1@127.0.0.1']
Connection eviction rate: 5 connections/second
Session eviction rate: 5 sessions/second
Connection goal: 0.0
Current average donor node connection count: 300.0
```

#### 負荷再分散の停止

負荷再分散を停止するCLIコマンドは以下の通りです：

```bash
emqx ctl rebalance stop
```

返される結果の例：

```bash
./bin/emqx ctl rebalance stop
Rebalance stopped
```

### HTTP APIによる負荷再分散の開始／停止

CLIで可能な操作はすべてAPIでも可能です。開始・停止コマンドはノードをパラメータとして指定する必要があります。詳細は[APIドキュメント](https://docs.emqx.com/en/enterprise/v5.1/admin/api-docs.html)を参照してください。

## ロードバランサーの統合

ユーザーはロードバランサーを統合して退避／負荷再分散を実行できます。切断されたクライアントが再接続を試みる際、ロードバランサーはバックエンドノードの現在の状態に基づき、受け入れノードへリダイレクトします。ユーザーはロードバランサー統合のためにヘルスチェックパラメータを設定する必要があります。設定が不十分だと切断が過剰に発生する可能性があります。これを支援するため、EMQXはヘルスチェック用REST APIを提供しています：

`GET /api/v5/load_rebalance/availability_check`

ヘルスチェックは、ドナーまたは退避中のノードに対してHTTPコード503を返し、正常に稼働し接続を受け入れているノードに対してはHTTPコード200を返します。

例えば、3ノードのEMQXクラスターでHAProxyを使用し、MQTTリスナーがポート3001、3002、3003、REST APIがポート5001、5002、5003で稼働している場合、以下のような設定が可能です：

```bash
defaults
  timeout connect 5s
  timeout client 60m
  timeout server 60m

listen mqtt
  bind *:1883
  mode tcp
  maxconn 50000
  timeout client 6000s
  default_backend emqx_cluster

backend emqx_cluster
  mode tcp
  balance leastconn
  option httpchk
  http-check send meth GET uri /api/v5/load_rebalance/availability_check hdr Authorization "Basic xxxxxx"
  server emqx1 127.0.0.1:3001 check port 5001 inter 1000 fall 2 rise 5 weight 1 maxconn 1000
  server emqx2 127.0.0.1:3002 check port 5002 inter 1000 fall 2 rise 5 weight 1 maxconn 1000
  server emqx3 127.0.0.1:3003 check port 5003 inter 1000 fall 2 rise 5 weight 1 maxconn 1000
```
