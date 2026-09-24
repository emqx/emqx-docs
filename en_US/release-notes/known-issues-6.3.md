# Known Issues in EMQX 6.3

## 6.3.1

| Since version | Issue                                                        | Workaround                                                   | Status |
| ------------- | ------------------------------------------------------------ | ------------------------------------------------------------ | ------ |
| 6.0.0         | **Throttled inbound traffic on TCP listeners with `tcp_backend = socket` and a small `tcp_options.recbuf`**<br />A TCP listener configured with the non-default `tcp_backend = socket` together with a `tcp_options.recbuf` below the operating system's default receive buffer (for example `4KB`) receives all inbound traffic very slowly, whatever the message size: with `recbuf = 4KB` each connection receives about 20 KB/s, so a 256 KB message takes several seconds to arrive. Listeners with the default `tcp_backend = gen_tcp`, and `socket` listeners that do not set `recbuf`, are not affected. | Remove `tcp_options.recbuf` from listeners that use `tcp_backend = socket`, or switch the listener to `tcp_backend = gen_tcp`. | Fix scheduled for 6.3.2 ([emqx/emqx#19190](https://github.com/emqx/emqx/pull/19190)) |
