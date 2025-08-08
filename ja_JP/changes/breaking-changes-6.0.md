# EMQX 6.0 の非互換な変更

## e6.0.0

- [#15613](https://github.com/emqx/emqx/pull/15613) Debian 10 のパッケージリリースを停止しました。

- [#15635](https://github.com/emqx/emqx/pull/15635) RocketMQ アクションの `parameters.strategy` フィールドでキーテンプレートを設定する（つまり、暗黙的にキーディスパッチ戦略を指定する）ことをサポートしなくなりました。代わりに、ユーザーは `parameters.strategy = key_dispatch` を設定し、`parameters.key` でテンプレートを指定する必要があります。