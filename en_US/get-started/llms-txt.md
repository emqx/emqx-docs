# LLMs.txt

EMQX documentation provides `llms.txt` files, which are structured documentation indexes designed for large language models (LLMs). These files help AI tools and agents quickly discover and reference EMQX documentation content, so you get more accurate and relevant results when building agents or using AI coding assistants.

## What Is llms.txt?

`llms.txt` is a plain-text file placed at a standard URL path that lists the key documentation pages for a product. It follows the [llms.txt specification](https://llmstxt.org/), which is an open standard for making documentation AI-friendly.

When an AI tool supports `llms.txt`, you can point it at this file to give it structured, up-to-date access to EMQX documentation without needing to paste content manually.

::: tip

The `llms.txt` file is automatically generated at each documentation build, so it always reflects the latest documentation structure and content.

:::

## Available Files

The following `llms.txt` endpoints are available for EMQX products:

| Product | URL |
|---|---|
| EMQX Enterprise | `https://docs.emqx.com/en/emqx/latest/llms.txt` |
| EMQX Cloud | `https://docs.emqx.com/en/cloud/latest/llms.txt` |
| EMQX Edge | `https://docs.emqx.com/en/emqx-edge/latest/llms.txt` | <!-- TODO: llms.txt for EMQX Edge returns 404; check doc build pipeline -->
| EMQX Neuron | `https://docs.emqx.com/en/neuronex/latest/llms.txt` |

A top-level index that lists all available `llms.txt` files is also available at:

```
https://docs.emqx.com/llms.txt
```

## Usage with AI Tools

### Cursor

To add EMQX documentation as a context source in [Cursor](https://docs.cursor.com/context/rules):

1. Open **Cursor Settings** > **Features** > **Docs**.
2. Click **Add new doc**.
3. Enter the `llms.txt` URL for the product you need, for example:
   ```
   https://docs.emqx.com/en/emqx/latest/llms.txt
   ```
4. Cursor indexes the documentation and makes it available via `@Docs` in chat.

### Windsurf

To use EMQX documentation in [Windsurf](https://docs.windsurf.com/windsurf/memories#adding-a-documentation-source):

1. Open the **Memories** panel.
2. Click **Add documentation source**.
3. Paste the `llms.txt` URL.

### Claude, ChatGPT, and Other AI Tools

For AI tools that accept URLs or file attachments as context, paste the `llms.txt` URL directly into your prompt:

```
Using the EMQX documentation at https://docs.emqx.com/en/emqx/latest/llms.txt, help me configure authentication with JWT tokens.
```

::: tip Note

Some AI tools require you to type the `@` symbol manually to reference a documentation source. If the URL is not recognized automatically, check your tool's documentation for how to add custom doc sources.

:::

## Access Documentation as Markdown

Any EMQX documentation page can be retrieved as raw Markdown by replacing `.html` with `.md` in the URL. This is useful for feeding specific pages directly into an AI tool or script:

```
# HTML page
https://docs.emqx.com/en/emqx/latest/access-control/authn/jwt.html

# Same page as Markdown
https://docs.emqx.com/en/emqx/latest/access-control/authn/jwt.md
```
