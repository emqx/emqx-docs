# EMQ Documentation Writing Guide

## Table of Contents

- [EMQX Documentation Writing Guide](#emq-documentation-writing-guide)
  - [Table of Contents](#table-of-contents)
  - [Introduction](#introduction)
  - [Left menu configuration](#left-menu-configuration)
    - [Configuration files](#configuration-files)
    - [Configuration examples](#configuration-examples)
    - [Notes](#notes)
  - [Markdown writing specifications](#markdown-writing-specifications)
    - [Must have a level 1 heading](#must-have-a-level-1-heading)
    - [Headings must obey the hierarchy](#headings-must-obey-the-hierarchy)
    - [Code block](#code-block)
    - [Escape special characters](#escape-special-characters)
    - [Resource reference](#resource-reference)
    - [Special grammars](#special-grammars)
    - [Update EMQX API documentation](#update-emqx-api-documentation)
    - [Update configuration manual](#update-configuration-manual)


## Introduction

EMQX documentation is written in Markdown format and use [Vuepress](https://vuepress.vuejs.org/) compiling the Markdown file to HTML file.

The final presentation of the documentation can be divided into three parts:

- Left menu.

  This part needs to be configured mutually by the document writer. The configuration contains three parts: directory name, directory hierarchy, and directory order.

- Intermediate document content.

  This part will display the specific content of the Markdown file.

- In-page index on the right hand.

  This part will automatically display all level 2 headings within the Markdown file. Therefore, a sensible Markdown heading will allow users to quickly understand the outline of document content and jump around the page.

![intro](./assets/intro.jpg)

## Left menu configuration

### Configuration files

The menu configuration file is `dir.yaml` in the document root directory. This is shown below:

### Configuration examples

We take the following configuration for `Introduction`  as an example.

![introduction-dir](./assets/introduction-dir.jpg)

The corresponding configuration is:

```json
{
  "en": [
    {
      "title": "Introduction",
      "children": [
        {
          "title": "EMQX Broker",
          "path": "./"
        },
        {
          "title": "Features List",
          "path": "introduction/checklist"
        }
      ]
    },
    ...
  ]
  "cn": [
    ...
  ]
}
```

Corresponding file structure:

```bash
.
├── en_US
│   ├── README.md
│   └── introduction
│       └── checklist.md
```

The corresponding page routing of the file structure:

| Relative path to the file  | Page routing address         |
| -------------------------- | ---------------------------- |
| /README.md                 | /                            |
| /introduction/checklist.md | /introduction/checklist.html |

### Notes

- The contents of the `path` configuration item must not be duplicated;
- `path` only need to specify the Markdown file, and can not use paths with anchors;
- Nested next-level directories using `children`, supporting multiple levels of nesting;
- When using `children`, you can now specify their `path` at the same time. This means that even if a directory has subdirectories, it can still be set as a page;

## Markdown writing specifications

EMQ documents support standard Markdown specification syntax, but the following conventions need to be adhered to when writing documents.

### Must have a level 1 heading

Each Markdown file must have a globally unique level 1 heading that clearly represents the content of the file.

### Headings must obey the hierarchy

The document will read the level 2 heading as the right-hand navigation, obeying the hierarchical relationship to ensure a clear directory structure.

```markdown
# h1
  ## h2
    ### h3
  ## h2
    ### h3
```

### Code block

- Code blocks in documents are uniformly wrapped in **three backquotes**  ` ``` ` and using **indentation** style blocks is **forbidden**.
- Try to append a valid language alias when using code blocks to show correct syntax highlighting.

### Escape special characters

- If you need the original article to output the `<xxx>` tag and this tag is not in a code block or in-line code, you need to add a backslash `\` before the tag.

  Use `### log set-level \<Level>` instead of `### log set-level <Level>`;

- If you need the original article to output the double curly braces `{{ xxx }}`, you need to wrap it with v-pre (you don't need to wrap it when inside a code block).

  Input

  ```markdown
  ::: v-pre
  {{ This will be displayed as-is }}
  :::
  ```

  Output

  {{ This will be displayed as-is }}

### Resource reference

- The name of the image must be in English and contain no spaces.

- Relative paths must be used for image references.

  For example, using `![image](./assets/bridge_mqtt.png)` instead of `![image](/assets/bridge_mqtt.png)`

### Special grammars

The documentation supports the following special syntax.

```markdown
::: tip
This is a tip
:::

::: warning
This is a warning
:::

::: danger
This is a dangerous warning
:::
```

The output is as follows.

![block](./assets/block.jpg)

### Update EMQX API documentation

EMQX automatically generates a JSON formatted description file (Swagger) that conforms to the OpenAPI 3.0 specification. It is then rendered and displayed using [Redocly](https://github.com/Redocly/redoc).

The swagger file is saved in the`./redocly` directory configuration. After each official EMQX release, the API documentation need to be updated by following the steps below:

- Clone emqx/emqx, and checkout current release branch

```bash
git clone git@github.com:emqx/emqx.git
cd emqx
git checkout release-XY
```

- Build docker image

```bash
make emqx-enterprise-docker
```

- Go back to `emqx-docs` repo and run `update-api-docs.sh` script. Use docker image tag produced by the step above, and set `SCHEMA_BASE_DIR` to the path of the generated configuration documentation.

```bash
# For example, if EMQX version is 5.10.0, the docker image is emqx/emqx-enterprise:5.10.0,
# then run the following command:
./update-api-docs.sh "5.10.0" "emqx/emqx-enterprise:5.10.0"
```

- Check out new branch, add changes to git, commit and push

```bash
git checkout -b update-api-docs
git add "redocly/ee-en.json"
git add "redocly/ee-zh.json"
git commit -m "update api docs for EMQX 5.10.0"
git push -u origin update-api-docs
```

- Send a pull request

You can also upload swagger.json to <https://redocly.github.io/redoc/> to preview the API doc rendering results.

### Update configuration manual

The configuration docs are generated from source code.
Steps to  update:

- Clone emqx/emqx, and checkout current release branch

```bash
git clone git@github.com:emqx/emqx.git
cd emqx
git checkout release-XY
```

- Build release

```bash
make emqx-enterprise-rel
```

- Go back to `emqx-docs` repo and copy generated configuration schema files.

```bash
# For example, if EMQX version is 5.10.0, and emqx is in /home/me/emqx,
# then run the following command:
cp /home/me/emqx/schema-v2-en.json hocon/hocon-ee-v5.10.0-en.json
cp /home/me/emqx/schema-v2-zh.json hocon/hocon-ee-v5.10.0-zh.json
```

- Check out new branch, add changes to git, commit and push

```bash
git checkout -b update-config-manual
git add hocon/hocon-ee-v5.10.0-en.json
git add hocon/hocon-ee-v5.10.0-zh.json
git commit -m "update configuration manual for EMQX 5.10.0"
git push -u origin update-config-manual
```

- Send a pull request
