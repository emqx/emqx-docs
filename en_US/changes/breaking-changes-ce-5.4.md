# Incompatible Changes in EMQX 5.4 

## v5.4.0

- [#11994](https://github.com/emqx/emqx/pull/11994) Stopped releasing packages for Windows.

- [#11998](https://github.com/emqx/emqx/pull/11998) Stopped releasing packages for MacOS 11 (BigSur).

- [#12112](https://github.com/emqx/emqx/pull/12112) Stopped supporting UDP multicast based clustering strategy.

- [#10976](https://github.com/emqx/emqx/pull/10976) Fixed topic-filter overlapping handling in shared subscription.
  * Hook callback `session.subscribed` and `client.subscribe` will now receive shared subscription in its full representation, e.g. `$share/group1/topic1/#`, and the `share` property is deleted from `subopts`.
  * Hook callback `session.unsubscribed` and `client.unsubscribe` will now receive shared subscription in its full representation, e.g. `$share/group1/topic1/#` instead of just `topic1/#`.
  * ExHook Proto changed. The `share` field in message `SubOpts` was deprecated.
    ExHook Server will now receive shared subscription in its full representation, e.g. `$share/group1/topic1/#`, and the `share` property is deleted from message `SubOpts`.
  * `session.subscribed` and `session.unsubscribed` rule-engine events will have shared subscriptions in their full representation for `topic`, e.g. `$share/group1/topic1/#` instead of just `topic1/#`.
