# EMQX 6.2 における非互換変更点

## 6.2.1

- [#17157](https://github.com/emqx/emqx/pull/17157) 新しいルールエンジン設定 `rule_engine.limit_selects_in_namespace` を導入しました。デフォルト値は `true` です。有効にすると、ルールはルール自身と同じネームスペース上のクライアントによってパブリッシュされたメッセージにのみトリガーされます。

## 6.2.0

- [#16589](https://github.com/emqx/emqx/pull/16589) ルールエンジンのランタイムで使用される jq 言語がバージョン 1.6.1 から 1.8.1 にアップグレードされ、いくつかの微妙な非互換性が導入されました。これらは通常のデプロイメントに影響を与える可能性は低いですが、完全性のために以下に記載します。

  - **空文字列を jq プログラムとして指定することはエラーとみなされるようになりました**：代わりに `"."` を使用してください。([jq#2790](https://github.com/jqlang/jq/pull/2790))

  - **文字列関数はコードポイントインデックスを使用するようになりました**：`indices/1`、`index/1`、`rindex/1` 関数はバイトインデックスではなくコードポイントインデックスを使用します。バイトインデックスが必要な場合は `utf8bytelength/0` を使用してください。([jq#3065](https://github.com/jqlang/jq/pull/3065))

  - **`tonumber/0` は前後の空白を含む数字を拒否します**：`tonumber/0` を呼び出す前に `trim/0` で前後の空白を除去してください。([jq#3055](https://github.com/jqlang/jq/pull/3055), [jq#3195](https://github.com/jqlang/jq/pull/3195))

  - **`last(empty)` の挙動が変更されました**：`last(empty)` は出力値を返さず、`first(empty)` と一貫した動作になります。([jq#3179](https://github.com/jqlang/jq/pull/3179))

  - **`limit/2` は負のカウントに対してエラーを返すようになりました**：これまでは黙って受け入れていました。([jq#3181](https://github.com/jqlang/jq/pull/3181))

  - **Tclスタイルの複数行コメントがサポートされました**：既存コードのパースに微妙な影響を与える可能性があります。([jq#2989](https://github.com/jqlang/jq/pull/2989))

  - **10進数の変換方法が変更されました**：10進数は decimal64 ではなく binary64（倍精度浮動小数点数）に変換されるようになり、jq の動作が JSON 仕様や他の言語により近くなりました。([jq#2949](https://github.com/jqlang/jq/pull/2949))

  - **`nth/2` は範囲外のインデックスに対して空を出力し、エラーを返さなくなりました**。([jq#2674](https://github.com/jqlang/jq/pull/2674))

  - **文字列の 0 または 1 未満の乗算は空文字列を返すようになりました**：これまでは元の文字列を返していました。([jq#2142](https://github.com/jqlang/jq/pull/2142))
