# Rate limit
EMQ X Broker specifies the limit on access speed and message speed. When the client's connection request speed exceeds the specified limit, the establishment of a new connection is suspended; when the message reception speed exceeds the specified limit, the reception of messages is suspended.

Rate limit is a *backpressure* scheme that avoids system overload from the entrance and guarantees system stability and predictable throughput. The rate limit can be configured in `etc/emqx.conf` :

| Configuration item                  | Type            | Default value | Description                                                  |
| ----------------------------------- | --------------- | ------------- | ------------------------------------------------------------ |
| listener.tcp.external.max_conn_rate | Number          | 1000          | The maximum allowable connection rate on this node (conn/s)  |
| zone.external.publish_limit         | Number,Duration | No limit      | Maximum allowable publish rate on a single connection (msg/s) |
| listener.tcp.external.rate_limit    | Size,Duration   | No limit      | Maximum allowable packet rate on a single connection (bytes/s) |

- **max_conn_rate** is the rate limit for connection establishment on a single emqx node. `1000` means that 1000 clients can access at most.
- **publish_limit** is the rate limit for receiving PUBLISH packets on a single connection. `100,10s` means that the maximum PUBLISH message rate allowed on each connection is 100 every 10 seconds.
- **rate_limit** is the rate limit for receiving TCP packets on a single connection. `100KB,10s` means that the maximum TCP packet rate allowed on each connection is 100KB every 10 seconds.

`publish_limit` and `rate_limit` both provide limits for a single connection. EMQ X Broker currently does not provide a global message rate limit.

## Rate limit explanation 
EMQ X Broker uses the [Token Bucket](https://en.wikipedia.org/wiki/Token_bucket) algorithm to control all Rate Limits. The logic of the token bucket algorithm is as follows:

![image-20190604103907875](../assets/token-bucket.jpg)

- There is a bucket that can hold the maximum burst of the token. The maximum burst is abbreviated as b.
- There is a rate for adding tokens to the bucket per second, abbreviated as r. When the bucket is full, no tokens are added to the bucket.
- Whenever 1 (or N) request arrives, take 1 (or N) token from the bucket. If the token is not enough, it will be blocked and wait for the token to be generated.

It can be seen from this algorithm:

- In the long run, the average value of the limited request rate is equal to the value of rate.

- When the actual request reaching speed is M, and M> r, then the maximum (peak) rate that can be achieved in actual operation is M = b + r.

It is easy to think that the maximum rate M is the speed that can consume the full state token bucket in 1 unit of time. The consumption rate of token bucket is M-r, so we can see that: b / (M-r) = 1, and we get M = b + r
  
  

### Application of Token Bucket Algorithm in EMQ X Broker
When the following configuration is used for packet rate limiting:

```
listener.tcp.external.rate_limit = 100KB,10s
```

EMQ X Broker will initialize the rate-limit processor of each connection with two values:

- rate = 100 KB / 10s = 10240 B/s
- burst = 100 KB = 102400 B

According to the algorithm in [Message Rate Limitation Explanation](#rate-limit-explanation), it is known:

- In the long run, the allowable average rate is limited to 10240 B/s
- The allowable peak rate is 102400 + 10240 = 112640 B/s

To improve system throughput, the access module of EMQ X Broker does not read packets from the socket one by one, but reads N packets from the socket each time. The timing of the rate-limit check is after receiving these N messages and before preparing to continue to receive the next N messages. Therefore, the actual rate limit will not be as accurate as the algorithm. EMQ X Broker only provides a rough rate limit. The value of `N` can be configured in  `etc/emqx.conf` :

| Configuration item             | Type   | Default value | Description                                                  |
| ------------------------------ | ------ | ------------- | ------------------------------------------------------------ |
| listener.tcp.external.active_n | Number | 100           | how many messages are read from the TCP stack by emqx at a time |
