# Incompatible Changes in EMQX 6.0

## e6.0.0

- [#15613](https://github.com/emqx/emqx/pull/15613) Stopped releasing packages for Debian 10.

- [#15635](https://github.com/emqx/emqx/pull/15635) We no longer support setting key templates (and thus implicitly specifying key dispatch strategy) in the `parameters.strategy` field of RocketMQ Action. Instead, users should set `parameters.strategy = key_dispatch` and specify the template in `parameters.key`.
