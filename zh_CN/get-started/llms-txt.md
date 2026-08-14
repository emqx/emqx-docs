---
next:
  text: '使用指南'
  link: '../guides/user-guide'
---

# LLMs.txt

EMQX 文档提供 `llms.txt` 文件，专为大语言模型（LLM）设计的结构化文档索引。AI 工具和 Agent 可以通过这些文件快速发现和引用 EMQX 文档内容，帮助你在构建 Agent 或使用 AI 编程助手时获得更准确、更相关的回答。

## 什么是 llms.txt？

`llms.txt` 是一个纯文本文件，放置在标准 URL 路径下，列出了产品的关键文档页面。它遵循 [llms.txt 规范](https://llmstxt.org/)，一个让文档对 AI 更友好的开放标准。

当 AI 工具支持 `llms.txt` 时，你只需将该文件的 URL 提供给它，即可让 AI 获得结构化、最新的 EMQX 文档访问能力，无需手动粘贴内容。

::: tip

`llms.txt` 文件在每次文档构建时自动生成，因此始终反映最新的文档结构和内容。

:::

## 可用文件

以下顶层索引文件列出了所有 EMQX 产品和版本的 `llms.txt`：

```
https://docs.emqx.com/llms.txt
```

EMQX 文档按产品和版本进行组织，每个产品版本都有对应的 `llms.txt`，例如：

```
https://docs.emqx.com/en/emqx/latest/llms.txt
https://docs.emqx.com/en/emqx/v6.0/llms.txt
```

将顶层索引提供给 AI 工具，它可以自行发现完整列表并导航到正确的版本。

## 在 AI 工具中使用

将顶层 `llms.txt` URL 提供给你使用的 AI 工具或编程助手：

```
https://docs.emqx.com/llms.txt
```

具体操作方式因工具而异。有些工具支持在设置中直接注册文档来源，有些则接受在提示词中直接粘贴 URL。如果不确定如何操作，可以直接问该工具，例如：

```
如何将 https://docs.emqx.com/llms.txt 添加为文档来源？
```

AI 会告诉你当前版本的具体操作步骤。

## 以 Markdown 格式访问文档

将任意 EMQX 文档页面 URL 中的 `.html` 替换为 `.md`，即可获取该页面的原始 Markdown 内容。这在需要将特定页面直接传入 AI 工具或脚本时非常实用：

```
# HTML 页面
https://docs.emqx.com/en/emqx/latest/access-control/authn/jwt.html

# 同一页面的 Markdown 格式
https://docs.emqx.com/en/emqx/latest/access-control/authn/jwt.md
```
