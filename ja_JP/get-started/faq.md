# Usage FAQs

## What happens when my License expires?

EMQX Enterpriseユーザーの場合、ライセンスの有効期限が切れると、ノード起動時に有効期限切れを通知する警告が表示されます。ライセンスタイプに応じて、以下の追加制限が適用される場合があります。

- **「小規模」顧客向けまたはトライアルライセンスの場合：** ライセンスで指定された接続数の上限に達していなくても、新規のMQTT接続は許可されません。既存の接続は切断されませんが、切断された場合は再接続できません。
- **「小規模」顧客向けまたはトライアル以外のライセンスの場合：** 総接続数が最大上限を超えない限り、新規のMQTT接続は引き続き許可されます。

ご自身のライセンスタイプが不明な場合は、担当アカウントマネージャーにご確認ください。

## How do I update my License?

以下のコマンドでEMQX Enterpriseライセンスを更新できます。

```bash
./bin/emqx ctl

    license info             # ライセンス情報を表示
    license update <License> # 文字列で指定したライセンスを更新
```

また、ダッシュボードからもライセンスを更新可能です。ライセンスの申請およびダッシュボード経由での更新方法については、[Work with EMQX Enterprise License](./deploy/license.md)をご参照ください。

## Why can't I receive retained messages when using shared subscriptions?

MQTTプロトコルにより、クライアントが共有サブスクリプションを使用している場合、サーバーはそのクライアントに対してリテインドメッセージを送信できません。

## Why do messages sometimes get lost when using shared subscriptions?

共有サブスクライバーの接続が切断されてもセッションがアクティブな場合、サーバーはメッセージの配信を続け、メッセージはセッション内に一時的に保存されます。そのため、他のアクティブな共有サブスクライバーはすべてのメッセージを消費していないように見えることがあります。また、共有サブスクライバーが再接続時に新しいセッションを作成すると、古いセッションにキャッシュされたメッセージは永久に失われます。

上記の状況がないにもかかわらずメッセージロスが発生する場合は、[Log Trace](../guides/observability/tracer.md)を使用してさらに調査してください。

## How to troubleshoot the cause of SSL/TLS connection failure?

通常、SSL/TLS接続のハンドシェイクに失敗すると、EMQXは[ログ](../guides/observability/log.md)に失敗理由を出力します。以下はログに現れる一般的なキーワードとその意味です。

- certificate_expired

   `certificate_expired`がログに表示された場合、証明書の有効期限が切れているため、速やかに更新してください。

- no_suitable_cipher

   `no_suitable_cipher`がログに表示された場合、ハンドシェイク中に適切な暗号スイートが見つからなかったことを示します。証明書の種類が暗号スイートと合わない、サーバーとクライアント双方がサポートする暗号スイートが存在しないなどが考えられます。

- handshake_failure

   `handshake_failure`がログに表示された場合、原因は多岐にわたります。クライアントのエラーと合わせて分析してください。例えば、クライアントが接続先サーバーのアドレスとサーバー証明書のドメイン名が一致しないことを検知する場合があります。

- unknown_ca

   `unknown_ca`がログに表示された場合、証明書の検証に失敗しています。中間CA証明書の省略、ルートCA証明書の未指定、誤ったルートCA証明書の指定などが一般的な原因です。双方向認証の場合は、ログの他の情報からサーバーまたはクライアントの証明書設定の誤りを判断できます。サーバー証明書の問題の場合、通常は以下のようなエラーログが出ます。

   ```
   {ssl_error,{tls_alert,{unknown_ca,"TLS server: In state certify received CLIENT ALERT: Fatal - Unknown CA\n"}}}
   ```

   `CLIENT ALERT`はクライアントからの警告メッセージであり、サーバー証明書がクライアントの検証に失敗したことを意味します。

   クライアント証明書の問題の場合、通常は以下のようなエラーログが出ます。

   ```
   {ssl_error,{tls_alert,{unknown_ca,"TLS server: In state certify at ssl_handshake.erl:1887 generated SERVER ALERT: Fatal - Unknown CA\n"}}}
   ```

   `SERVER ALERT`はサーバーがクライアント証明書の認証に失敗し、クライアントに警告メッセージを送信したことを意味します。

- protocol_version

   `protocol_version`がログに表示された場合、クライアントとサーバーのTLSプロトコルバージョンが一致していません。

## How to troubleshoot abnormal disconnections on MQTT clients?

Shellで`emqx ctl listeners`を実行すると、各リスナーの切断統計が返されます。切断理由と切断回数が含まれます。例：

```
$ . /bin/emqx ctl listeners
tcp:default
  listen_on : 0.0.0.0:1883
  acceptors : 16
  proxy_protocol : false
  running : true
  current_conn : 9
  max_conns : infinity
  shutdown_count : [{keepalive_timeout,1},{idle_timeout,2}]
```

よくある切断理由は以下の通りです。

- `keepalive_timeout`: クライアントのハートビートがタイムアウトし、EMQXが接続を切断。
- `tcp_closed`: クライアントがDISCONNECTパケットを送信せずにTCP接続を直接閉じた。
- `frame_too_large`: クライアントが送信したパケットサイズが最大制限を超え、EMQXが接続を切断。
- `connect_packet_too_large`: クライアントのCONNECTパケットが`mqtt.max_connect_packet_size`を超え、EMQXが接続を切断。
- `protocol_error`: クライアントの非準拠動作（例：同一接続内で複数のCONNECTパケット送信）によりEMQXが接続を切断。
- `idle_timeout`: TCP接続確立後15秒以内にクライアントからCONNECTパケットを受信しなかったため、EMQXが接続を切断。

また、[Log Trace](../guides/observability/tracer.md)を使い、指定したクライアントID、IP、トピックに関連するログを追跡し、切断原因を分析できます。

## When I was executing the stress test, the connection number and throughput were lower than expected. How can I tune the system to make full use of it?

ストレステスト実行時は、必要なハードウェアリソースの確保に加え、OSおよびErlang VMのチューニングが重要です。主なチューニング項目は、ファイルハンドルのグローバル制限およびユーザー制限、TCPのバックログとバッファ、Erlang VMのプロセス数制限などです。クライアント側もすべてのサブスクライブおよびパブリッシュを処理できる能力とリソースを持つよう調整が必要です。

ユースケースにより最適なチューニングは異なります。一般的なチューニング方法については[Performance Tuning](../guides/performance/tune.md)をご参照ください。

## When I encounter problems related to client connection, publishing, and subscription, such as failure to connect, abnormal disconnection, etc., how should I troubleshoot?

EMQXのデバッグログにはすべての動作や現象が記録されています。デバッグログを確認することで、クライアントが接続を開始した時刻、接続時のパラメータ、接続の成功・拒否の有無および拒否理由などを特定できます。ただし、デバッグモードのログは膨大でリソースを消費し、個別のクライアントやトピックの分析が困難になる場合があります。

このため、EMQXは[Log Trace](../guides/observability/tracer.md)機能を提供しています。トレース対象のクライアントやトピックを指定すると、それらに関連するすべてのデバッグログを指定ログファイルに出力します。これにより自己分析やコミュニティへの問い合わせが容易になります。

なお、クライアントがネットワーク障害によりEMQXに接続できない場合、EMQXはメッセージを受信しないためログトレースは役に立ちません。このような場合は、ファイアウォールやセキュリティグループによるサーバーポートの閉塞などネットワーク設定の問題が多く、クラウド環境でのEMQXデプロイ時に特に多く見られます。したがって、ログトレースに加え、ポートの占有状況、リッスン状態、ネットワーク設定の確認も必要です。

## Why are there client IDs like "CENSYS" or other unfamiliar clients?

CENSYSはインターネットのスキャンおよび偵察ツールで、IPv4アドレス空間を定期的にスキャンし、HTTP、SSH、MQTTなどの各種プロトコルのデフォルトポートを特定します。したがって、MQTTブローカーに「CENSYS」などのクライアントIDや見慣れないクライアントがアクセスしている場合は、セキュリティ保護レベルが比較的低いことを示しています。効果的な対策として以下を検討してください。

1. EMQXのHTTP APIアクセス権限検証に使われるAppIDやAppSecretなど、デフォルト設定の使用を避ける。
2. パスワード認証やJWT認証などの認証機構を有効化し、IPアドレスの知識だけでログインできないようにする。
3. TLS相互認証を有効化し、有効な証明書を持つクライアントのみアクセスを許可する。
4. 適切な認可機構を有効化し、権限のないデバイスによる機密データへのアクセスを制限する。
5. 不要なポートは可能な限りファイアウォールで閉じる。

## How to reset the Dashboard login password if I forget it?

ダッシュボードの初期ユーザー名とパスワードはそれぞれ`admin`と`public`です。セキュリティ上の理由から、初回ログイン時にパスワード変更が強制されます。以前設定したパスワードを忘れた場合は、以下のコマンドで旧パスワード不要で新しいパスワードを設定できます。

```bash
./bin/emqx ctl admins passwd <Username> <Password>
```

## Why does EMQX memory usage stay high after a load spike?

接続数やメッセージ数、キューイングされた作業負荷が急増した後、負荷が収まってもOSが報告するEMQXのメモリ使用量が高いままになることがあります。これは通常、メモリリークではありません。

EMQXはErlang VM上で動作し、独自のアロケータでメモリを管理しています。アロケータはOSから大きなメモリ領域（キャリア）を取得し、プロセス、メッセージ、バイナリ、ETSテーブル用に分割します。EMQXがデータを解放しても、対応するメモリブロックはアロケータに戻され、OSには返却されません。すべてのブロックが空いている場合にのみ、アロケータはキャリアをOSに返却します。リテインドメッセージやセッションデータなどの長寿命データが少量でも存在すると、ほぼ空のキャリア内のブロックを占有し、キャリアの返却を妨げることがあります。

アロケータはこの空き領域を再利用できるため、後の同様の負荷増大時にはOSからのメモリ要求が減る可能性があります。OSレベルのメモリ測定はこのアロケータ保持の空き領域も含みますが、測定範囲は異なります。

- プロセスの常駐セットサイズ（RSS）はEMQXプロセスの常駐メモリを測定します。
- コンテナのメモリ使用量はEMQX以外のメモリも含め、コンテナのcgroupに課金されるメモリを含みます。
- ダッシュボードのノードメモリや`emqx_vm_used_memory` Prometheusメトリックはノードまたはコンテナのシステムメモリを示し、EMQXプロセスだけではありません。

### How to tell retained memory from a leak

Erlang VMアロケータが予約しているメモリと、Erlangデータが占有しているメモリを比較します。各コマンドはバイト単位の値を返します。

```bash
./bin/emqx eval 'recon_alloc:memory(allocated).'
./bin/emqx eval 'recon_alloc:memory(used).'
```

使用中のメモリの内訳（プロセス、バイナリ、ETSテーブルなど）を確認するには、`./bin/emqx ctl vm memory`を実行してください。

- `allocated`が`used`より大幅に大きく、`used`が安定または減少している場合、差分はアロケータが保持する空き領域であり、リークではありません。
- `used`が負荷が安定しているにもかかわらず増加し続ける場合、データが蓄積しています。例えば、遅いサブスクライバーが長いメッセージキューを作成したり、切断されたクライアントのセッションが蓄積したりします。さらに調査するか、[技術サポート](./support/technical-support.md)にお問い合わせください。

### How to release the memory

メモリを解放する必要はありません。アロケータは再利用します。OSが報告するメモリ使用量を減らしたい場合は、ノードを再起動してください。クラスター環境では、ノードを1台ずつ順番に再起動してください。
