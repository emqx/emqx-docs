# LLMs.txt

EMQX 文档提供 `llms.txt` 文件，专为大语言模型（LLM）设计的结构化文档索引。AI 工具和 Agent 可以通过这些文件快速发现和引用 EMQX 文档内容，帮助你在构建 Agent 或使用 AI 编程助手时获得更准确、更相关的回答。

## 什么是 llms.txt？

`llms.txt` 是一个纯文本文件，放置在标准 URL 路径下，列出了产品的关键文档页面。它遵循 [llms.txt 规范](https://llmstxt.org/)，一个让文档对 AI 更友好的开放标准。

当 AI 工具支持 `llms.txt` 时，你只需将该文件的 URL 提供给它，即可让 AI 获得结构化、最新的 EMQX 文档访问能力，无需手动粘贴内容。

::: tip

`llms.txt` 文件在每次文档构建时自动生成，因此始终反映最新的文档结构和内容。

:::

## 可用文件

以下 `llms.txt` 端点适用于各 EMQX 产品：

| 产品 | URL |
|---|---|
| EMQX 企业版 | `https://docs.emqx.com/en/emqx/latest/llms.txt` |
| EMQX Cloud | `https://docs.emqx.com/en/cloud/latest/llms.txt` |
| EMQX Edge | `https://docs.emqx.com/en/emqx-edge/latest/llms.txt` | <!-- TODO: llms.txt for EMQX Edge returns 404; check doc build pipeline -->
| EMQX Neuron | `https://docs.emqx.com/en/neuronex/latest/llms.txt` |

此外，顶层索引文件列出了所有可用的 `llms.txt`：

```
https://docs.emqx.com/llms.txt
```

## 在 AI 工具中使用

### Cursor

在 [Cursor](https://docs.cursor.com/context/rules) 中将 EMQX 文档添加为上下文来源：

1. 打开 **Cursor Settings** -> **Features** -> **Docs**。
2. 点击 **Add new doc**。
3. 输入所需产品的 `llms.txt` URL，例如：
   ```
   https://docs.emqx.com/en/emqx/latest/llms.txt
   ```
4. Cursor 会对文档进行索引，之后可在对话中通过 `@Docs` 引用。

### Windsurf

在 [Windsurf](https://docs.windsurf.com/windsurf/memories#adding-a-documentation-source) 中使用 EMQX 文档：

1. 打开 **Memories** 面板。
2. 点击 **Add documentation source**。
3. 粘贴 `llms.txt` URL。

### Claude、ChatGPT 及其他 AI 工具

对于支持 URL 或文件附件作为上下文的 AI 工具，可直接在提示词中粘贴 `llms.txt` URL：

```
请参考 https://docs.emqx.com/en/emqx/latest/llms.txt 中的 EMQX 文档，帮我配置 JWT 认证。
```

::: tip 注意

部分 AI 工具需要手动输入 `@` 符号来引用文档来源。如果 URL 未被自动识别，请查阅该工具的文档了解如何添加自定义文档来源。

:::

## 以 Markdown 格式访问文档

将任意 EMQX 文档页面 URL 中的 `.html` 替换为 `.md`，即可获取该页面的原始 Markdown 内容。这在需要将特定页面直接传入 AI 工具或脚本时非常实用：

```
# HTML 页面
https://docs.emqx.com/en/emqx/latest/access-control/authn/jwt.html

# 同一页面的 Markdown 格式
https://docs.emqx.com/en/emqx/latest/access-control/authn/jwt.md
```
