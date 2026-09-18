---
next:
  text: 'ユーザーガイド'
  link: '../guides/user-guide'
---

# LLMs.txt

EMQXドキュメントは、`llms.txt`ファイルを提供しています。これは大規模言語モデル（LLM）向けに構造化されたドキュメントのインデックスであり、AIツールやエージェントがEMQXのドキュメント内容を迅速に検索・参照できるように設計されています。これにより、エージェント構築やAIコーディングアシスタント利用時に、より正確で関連性の高い結果を得られます。

## llms.txtとは？

`llms.txt`は、製品の主要なドキュメントページを一覧化したプレーンテキストファイルで、標準的なURLパスに配置されます。これはドキュメントをAIに優しい形で提供するためのオープン標準である[llms.txt仕様](https://llmstxt.org/)に準拠しています。

AIツールが`llms.txt`をサポートしている場合、このファイルを指定するだけで、内容を手動で貼り付けることなく、EMQXドキュメントの構造化された最新情報にアクセスできます。

::: tip

`llms.txt`ファイルはドキュメントのビルド時に自動生成されるため、常に最新のドキュメント構造と内容を反映しています。

:::

## 利用可能なファイル

トップレベルのインデックスファイルには、EMQX製品およびバージョンごとに利用可能なすべての`llms.txt`ファイルが一覧化されています：

```
https://docs.emqx.com/llms.txt
```

EMQXドキュメントは製品とバージョンごとに整理されており、各製品バージョンに専用の`llms.txt`があります。例：

```
https://docs.emqx.com/en/emqx/latest/llms.txt
https://docs.emqx.com/en/emqx/v5.8/llms.txt
```

AIツールにトップレベルのインデックスを指定すれば、全リストを自動で検出し、適切なバージョンにナビゲートできます。

## AIツールでの利用方法

AIツールやコーディングアシスタントにトップレベルの`llms.txt`のURLを指定してください：

```
https://docs.emqx.com/llms.txt
```

設定方法はツールによって異なります。設定画面でドキュメントソースを直接登録できるものもあれば、プロンプトにURLを貼り付けるだけのものもあります。不明な場合は、AIツールに直接問い合わせてください。例えば：

```
https://docs.emqx.com/llms.txt をドキュメントソースとして追加するにはどうすればよいですか？
```

AIが自身のインターフェースに応じた最新の手順を案内します。

## Markdown形式でドキュメントにアクセス

任意のEMQXドキュメントページは、URLの`.html`を`.md`に置き換えることで、生のMarkdown形式で取得できます。これは特定ページをAIツールやスクリプトに直接渡す際に便利です：

```
# HTMLページ
https://docs.emqx.com/en/emqx/latest/guides/access-control/authn/jwt.html

# 同じページのMarkdown形式
https://docs.emqx.com/en/emqx/latest/guides/access-control/authn/jwt.md
```
