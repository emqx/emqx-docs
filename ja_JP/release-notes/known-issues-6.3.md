# EMQX 6.3 の既知の問題

## 6.3.1

| 対象バージョン | 問題                                                         | 回避策                                                        | ステータス |
| --------------- | ------------------------------------------------------------ | ------------------------------------------------------------- | ---------- |
| 6.0.0           | **`tcp_backend = socket` かつ小さい `tcp_options.recbuf` を設定した TCP リスナーでの受信トラフィックのスロットリング**<br />`tcp_backend = socket`（6.3.0 以降の Linux および macOS でのデフォルト）かつ OS のデフォルト受信バッファより小さい `tcp_options.recbuf`（例：`4KB`）を設定した TCP リスナーは、メッセージサイズに関わらず受信トラフィックが非常に遅くなります。`recbuf = 4KB` の場合、各接続は約 20 KB/s の速度で受信するため、256 KB のメッセージ到着に数秒かかります。`tcp_backend = gen_tcp` のリスナーや、`recbuf` を設定していない `socket` リスナーは影響を受けません。 | `tcp_backend = socket` を使用するリスナーから `tcp_options.recbuf` を削除するか、リスナーを `tcp_backend = gen_tcp` に切り替えてください。 | 6.3.2 で修正済み ([emqx/emqx#19351](https://github.com/emqx/emqx/pull/19351)) |
| 6.3.0           | **`tcp_backend = socket` の TCP リスナーでサブスクライバーが遅延すると CPU 使用率が高止まりし、配信が遅くなる場合がある**<br />6.3.0 以降の Linux および macOS で TCP リスナーのデフォルトとなった `tcp_backend = socket` では、サブスクライバーがメッセージ到着より遅く読み込むと（例：QoS 1 メッセージの大量バースト時）、未配信メッセージが接続内にキューイングされます。この接続はガベージコレクションに多くの時間を費やし、受信より少ないメッセージしか配信できず、バックログが減少しません。CPU は高負荷のままで、パブリッシュ停止後も配信速度は秒間数十メッセージに低下します。`tcp_backend = gen_tcp` の TCP リスナーや SSL、WebSocket、QUIC リスナーは影響を受けません。 | TCP リスナーの `tcp_backend` を `gen_tcp` に設定してください。例：`listeners.tcp.default.tcp_backend = gen_tcp`。 | 6.3.2 で修正済み ([emqx/emqx#19298](https://github.com/emqx/emqx/pull/19298)) |
