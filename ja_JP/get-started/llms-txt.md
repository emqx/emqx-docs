---
next:
  text: 'ユーザーガイド'
  link: '../guides/user-guide'
---

# LLMs.txt

EMQXドキュメントは、`llms.txt`ファイルを提供しています。これは大規模言語モデル（LLM）向けに構造化されたドキュメントのインデックスであり、AIツールやエージェントがEMQXのドキュメントコンテンツを迅速に発見・参照できるように設計されています。これにより、エージェントの構築やAIコーディングアシスタントの利用時に、より正確で関連性の高い結果を得ることができます。

## llms.txtとは？

`llms.txt`は、製品の主要なドキュメントページを一覧化したプレーンテキストファイルで、標準的なURLパスに配置されます。これはドキュメントをAIフレンドリーにするためのオープン標準である[llms.txt仕様](https://llmstxt.org/)に準拠しています。

AIツールが`llms.txt`に対応している場合、このファイルを指定するだけで、手動でコンテンツを貼り付けることなく、EMQXドキュメントへの構造化され最新のアクセスを提供できます。

::: tip

`llms.txt`ファイルはドキュメントのビルドごとに自動生成されるため、常に最新のドキュメント構造と内容を反映しています。

:::

## 利用可能なファイル

トップレベルのインデックスファイルには、EMQXの製品やバージョンごとに利用可能なすべての`llms.txt`ファイルが一覧化されています。

```
https://docs.emqx.com/llms.txt
```

EMQXドキュメントは製品とバージョンごとに整理されており、各製品バージョンごとに独自の`llms.txt`があります。例えば：

```
https://docs.emqx.com/en/emqx/latest/llms.txt
https://docs.emqx.com/en/emqx/v6.0/llms.txt
```

AIツールにトップレベルのインデックスを指定すれば、完全な一覧を自動で発見し、適切なバージョンへナビゲートできます。

## AIツールでの利用方法

AIツールやコーディングアシスタントにトップレベルの`llms.txt`のURLを指定してください。

```
https://docs.emqx.com/llms.txt
```

追加方法はツールによって異なります。設定画面でドキュメントソースを直接登録できるものもあれば、プロンプトにURLを貼り付けるだけで対応するものもあります。わからない場合は、AIツールに直接問い合わせてください。例えば：

```
How do I add https://docs.emqx.com/llms.txt as a documentation source?
```

AIは自身のインターフェースに応じた最新の手順を案内してくれます。

## Markdown形式でドキュメントにアクセス

任意のEMQXドキュメントページは、URLの`.html`を`.md`に置き換えることで、生のMarkdown形式で取得できます。これは特定のページを直接AIツールやスクリプトに渡す際に便利です。

```
# HTMLページ
https://docs.emqx.com/en/emqx/latest/guides/access-control/authn/jwt.html

# 同じページのMarkdown形式
https://docs.emqx.com/en/emqx/latest/guides/access-control/authn/jwt.md
```
