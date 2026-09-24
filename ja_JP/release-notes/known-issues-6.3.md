# EMQX 6.3 の既知の問題

## 6.3.1

| バージョン | 問題                                                         | 回避策                                                        | ステータス |
| ----------- | ------------------------------------------------------------ | ------------------------------------------------------------ | ---------- |
| 6.0.0       | **`tcp_backend = socket` かつ小さい `tcp_options.recbuf` を設定した TCP リスナーでの受信トラフィックのスロットリング**<br />`tcp_backend = socket`（6.3.0 以降の Linux と macOS のデフォルト）かつ OS のデフォルト受信バッファより小さい `tcp_options.recbuf`（例：`4KB`）を設定した TCP リスナーは、メッセージサイズに関わらず受信トラフィックが非常に遅くなります。`recbuf = 4KB` の場合、各接続の受信速度は約 20 KB/s となり、256 KB のメッセージの到着に数秒かかります。`tcp_backend = gen_tcp` のリスナーや `recbuf` を設定していない `socket` リスナーは影響を受けません。 | `tcp_backend = socket` を使用するリスナーから `tcp_options.recbuf` を削除するか、リスナーを `tcp_backend = gen_tcp` に切り替えてください。 | 6.3.2 で修正予定 ([emqx/emqx#19190](https://github.com/emqx/emqx/pull/19190)) |
| 6.3.0       | **`tcp_backend = socket` の TCP リスナーでサブスクライバーが遅延すると CPU 使用率が高止まりし、配信が遅くなる場合がある**<br />6.3.0 以降の Linux と macOS で TCP リスナーのデフォルトとなった `tcp_backend = socket` で、サブスクライバーがメッセージ到着より遅く読み込む（例：QoS 1 メッセージの大量バースト時）と、未配信メッセージが接続にキューイングされます。接続はガベージコレクションに多くの時間を費やし、受信より少ないメッセージしか配信できず、バックログが減少しません。CPU は限界まで高止まりし、パブリッシュ停止後も配信は秒間数十メッセージに低下します。`tcp_backend = gen_tcp` の TCP リスナーや SSL、WebSocket、QUIC リスナーは影響を受けません。 | TCP リスナーの設定で `tcp_backend = gen_tcp` を指定してください。例：`listeners.tcp.default.tcp_backend = gen_tcp`。 | 修正対応中 ([emqx/emqx#19201](https://github.com/emqx/emqx/issues/19201)) |
