# Known Issues in EMQX 6.3

## 6.3.1

| Since version | Issue                                                        | Workaround                                                   | Status |
| ------------- | ------------------------------------------------------------ | ------------------------------------------------------------ | ------ |
| 6.0.0         | **Slow message intake on TCP listeners with `tcp_backend = socket` and a small `tcp_options.recbuf`**<br />A TCP listener configured with the non-default `tcp_backend = socket` together with a small `tcp_options.recbuf` (for example `4KB`) receives large MQTT messages very slowly: the client is throttled to about `recbuf` bytes every 200 ms, so a 256 KB message takes several seconds to arrive. Listeners with the default `tcp_backend = gen_tcp`, and `socket` listeners without an explicit `recbuf`, are not affected. | Remove `tcp_options.recbuf` (and `sndbuf`) from listeners that use `tcp_backend = socket`, or switch the listener to `tcp_backend = gen_tcp`. | Fix scheduled for 6.3.2 ([emqx/emqx#19190](https://github.com/emqx/emqx/pull/19190)) |
