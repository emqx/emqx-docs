# Rate Limit
EMQX Broker specifies the limit on access speed and message speed. When the client's connection request speed exceeds the specified limit, the establishment of a new connection is suspended; when the message reception speed exceeds the specified limit, the reception of messages is suspended.

Rate limit is a *backpressure* scheme that avoids system overload from the entrance and guarantees system stability and predictable throughput. The rate limit can be configured in `etc/emqx.conf` :

| Configuration Item                        | Type             | Default Value | Description                                                  |
| ----------------------------------------- | ---------------- | ------------- | ------------------------------------------------------------ |
| listener.tcp.external.max_conn_rate       | Number           | 1000          | The maximum allowable connection rate on this node (conn/s)  |
| zone.external.rate_limit.conn_messages_in | Number, Duration | No limit      | Maximum allowable publish rate on a single connection (msg/s) |
| zone.external.rate_limit.conn_bytes_in    | Size, Duration   | No limit      | Maximum allowable packet rate on a single connection (bytes/s) |

- **max_conn_rate** is the rate limit for connection establishment on a single EMQX node. `1000` means that 1000 clients can access at most.
- **conn_messages_in** is the rate limit for receiving PUBLISH packets on a single connection. `100,10s` means that the maximum PUBLISH message rate allowed on each connection is 100 every 10 seconds.
- **conn_bytes_in** is the rate limit for receiving TCP packets on a single connection. `100KB,10s` means that the maximum TCP packet rate allowed on each connection is 100KB every 10 seconds.

`conn_messages_in` and `conn_bytes_in` both provide limits for a single connection. EMQX Broker currently does not provide a global message rate limit.

## Rate Limit Explanation 
EMQX Broker uses the [Token Bucket](https://en.wikipedia.org/wiki/Token_bucket) algorithm to control all Rate Limits. The logic of the token bucket algorithm is as follows:

![image-20190604103907875](../assets/token-bucket.jpg)

- A *bucket* holds tokens, with a maximum capacity of $burst$ tokens, abbreviated as $b$.
- Tokens are added to the bucket at a constant rate $rate$, abbreviated as $r$. When the bucket is full, no additional tokens are added.
- When a request (or N requests) arrives, it must consume 1 (or N) tokens from the bucket. If there are not enough tokens, the request is blocked until more tokens are generated.

In this algorithm:

1. When a large number of requests arrive and the bucket is full:

   The maximum number of tokens that can be consumed per unit time is $b + r/1$, i.e., all the tokens in the bucket plus those generated during the unit time.

   This is the maximum achievable rate: $M = b + r/1$.

2. After all tokens in the bucket are consumed (i.e., the bucket is empty):

   The number of tokens available per unit time is $0 + r/1$, meaning only newly generated tokens can be consumed.

   This represents the long-term average rate: $r$.

### Application of Token Bucket Algorithm in EMQX
When the following configuration is used for packet rate limiting:

```
zone.external.rate_limit.conn_bytes_in = 100KB,10s
```

EMQX will initialize the rate-limit processor of each connection with two values:

- rate = 100 KB / 10s = 10240 B/s
- burst = 100 KB = 102400 B

According to the algorithm in [Rate Limit Explanation](#rate-limit-explanation), it is known:

- In the long run, the allowable average rate is limited to 10240 B/s
- The allowable peak rate is 102400 + 10240 = 112640 B/s

To improve system throughput, the access module of EMQX Broker does not read packets from the socket one by one, but reads N packets from the socket each time. The timing of the rate-limit check is after receiving these N messages and before preparing to continue to receive the next N messages. Therefore, the actual rate limit will not be as accurate as the algorithm. EMQX Broker only provides a rough rate limit. The value of `N` can be configured in  `etc/emqx.conf` :

| Configuration item             | Type   | Default value | Description                                                  |
| ------------------------------ | ------ | ------------- | ------------------------------------------------------------ |
| listener.tcp.external.active_n | Number | 100           | how many messages are read from the TCP stack by emqx at a time |
