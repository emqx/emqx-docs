---
next:
  text: 'ユーザーガイド'
  link: '../guides/user-guide'
---

# LLMs.txt

EMQXドキュメントは、`llms.txt`ファイルを提供しています。これは大規模言語モデル（LLM）向けに構造化されたドキュメントの索引ファイルであり、AIツールやエージェントがEMQXのドキュメント内容を迅速に発見・参照できるように設計されています。これにより、エージェント構築やAIコーディングアシスタントの利用時に、より正確で関連性の高い結果を得ることが可能です。

## llms.txtとは？

`llms.txt`は、製品の主要なドキュメントページを一覧化したプレーンテキストファイルで、標準的なURLパスに配置されます。これはドキュメントをAIフレンドリーにするためのオープン標準である[llms.txt仕様](https://llmstxt.org/)に準拠しています。

AIツールが`llms.txt`に対応している場合、このファイルを指定することで、手動で内容を貼り付けることなく、EMQXドキュメントへの構造化された最新のアクセスを提供できます。

::: tip

`llms.txt`ファイルはドキュメントのビルド時に自動生成されるため、常に最新のドキュメント構造と内容を反映しています。

:::

## 利用可能なファイル

トップレベルのインデックスファイルには、EMQXの各製品およびバージョンに対応したすべての`llms.txt`ファイルが一覧化されています：

```
https://docs.emqx.com/llms.txt
```

EMQXドキュメントは製品とバージョンごとに整理されており、各製品バージョンごとに専用の`llms.txt`があります。例：

```
https://docs.emqx.com/en/emqx/latest/llms.txt
https://docs.emqx.com/en/emqx/v6.0/llms.txt
```

AIツールにトップレベルのインデックスを指定すれば、完全な一覧を自動で発見し、適切なバージョンへナビゲートできます。

## AIツールでの利用方法

AIツールやコーディングアシスタントにトップレベルの`llms.txt`のURLを指定してください：

```
https://docs.emqx.com/llms.txt
```

追加方法はツールによって異なります。設定画面で直接ドキュメントソースを登録できるものもあれば、プロンプトにURLを貼り付ける形式のものもあります。不明な場合は、AIツールに直接問い合わせてください。例えば：

```
https://docs.emqx.com/llms.txt をドキュメントソースとして追加するにはどうすればよいですか？
```

AIが自身のインターフェースに応じた最新の手順を案内してくれます。

## Markdown形式でドキュメントにアクセスする

任意のEMQXドキュメントページは、URLの拡張子`.html`を`.md`に置き換えることで、生のMarkdown形式で取得可能です。これは特定ページを直接AIツールやスクリプトに入力する際に便利です：

```
# HTMLページ
https://docs.emqx.com/en/emqx/latest/guides/access-control/authn/jwt.html

# 同じページのMarkdown形式
https://docs.emqx.com/en/emqx/latest/guides/access-control/authn/jwt.md
```
