---
next:
  text: 'User Guides'
  link: '/en/emqx/latest/guides/user-guide'
---

# LLMs.txt

EMQX documentation provides `llms.txt` files, which are structured documentation indexes designed for large language models (LLMs). These files help AI tools and agents quickly discover and reference EMQX documentation content, so you get more accurate and relevant results when building agents or using AI coding assistants.

## What Is llms.txt?

`llms.txt` is a plain-text file placed at a standard URL path that lists the key documentation pages for a product. It follows the [llms.txt specification](https://llmstxt.org/), which is an open standard for making documentation AI-friendly.

When an AI tool supports `llms.txt`, you can point it at this file to give it structured, up-to-date access to EMQX documentation without needing to paste content manually.

::: tip

The `llms.txt` file is automatically generated at each documentation build, so it always reflects the latest documentation structure and content.

:::

## Available Files

The top-level index file lists all available `llms.txt` files across EMQX products and versions:

```
https://docs.emqx.com/llms.txt
```

EMQX documentation is organized by product and version. Each product version has its own `llms.txt`, for example:

```
https://docs.emqx.com/en/emqx/latest/llms.txt
https://docs.emqx.com/en/emqx/v5.8/llms.txt
```

Point your AI tool at the top-level index, and it can discover the full list and navigate to the right version on its own.

## Usage with AI Tools

Point your AI tool or coding assistant at the top-level `llms.txt` URL:

```
https://docs.emqx.com/llms.txt
```

How you add it depends on the tool. Some tools let you register a documentation source directly in their settings; others accept a URL pasted into the prompt. If you are not sure, ask your AI tool directly. For example:

```
How do I add https://docs.emqx.com/llms.txt as a documentation source?
```

The AI will know the current steps for its own interface.

## Access Documentation as Markdown

Any EMQX documentation page can be retrieved as raw Markdown by replacing `.html` with `.md` in the URL. This is useful for feeding specific pages directly into an AI tool or script:

```
# HTML page
https://docs.emqx.com/en/emqx/latest/guides/access-control/authn/jwt.html

# Same page as Markdown
https://docs.emqx.com/en/emqx/latest/guides/access-control/authn/jwt.md
```
