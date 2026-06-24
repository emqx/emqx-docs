# EMQX English Documentation Writing Guide

This guide defines how to structure, write, format, and review EMQX English documentation in this repository. It is broader than a style guide. It covers information architecture, technical writing, Markdown and VitePress usage, EMQX-specific terminology, and review standards.

Use this guide when you create a new page, update an existing page, review a pull request, or translate technical content into English.

## Core Principles

Follow these principles for all EMQX documentation:

- **Be accurate**: Preserve technical meaning, product behavior, configuration paths, API paths, parameter names, examples, limits, and version information.
- **Be clear**: State the main point early. Use concrete terms and avoid vague wording.
- **Be concise**: Use short, direct sentences. Remove filler words and unnecessary background.
- **Be complete**: Include the prerequisites, steps, examples, expected results, and limitations that readers need to complete the task.
- **Be consistent**: Use consistent terminology, formatting, capitalization, and UI wording across the page and related pages.
- **Be user-focused**: Explain what users can do with EMQX, not only how EMQX is implemented.
- **Be AI-friendly**: Use semantic headings, self-contained examples, and structured content that can be consumed by search, retrieval, and AI tools.

Unless a page is explicitly written for beginners, assume that the reader is a technical user.

## Information Types

Structure content according to the purpose of the page or section. A page can contain more than one information type, but keep each section focused.

### Concept Topics

Use a concept topic to explain what something is, why it matters, and when to use it.

A concept topic should:

- Start with a short definition or summary.
- Explain the problem, use case, or relationship to other EMQX features.
- Avoid step-by-step instructions unless they are needed as a short example.
- Link to task or reference pages for configuration and operation details.

### Task Topics

Use a task topic to help users complete a goal.

A task topic should include:

- A goal-oriented title, such as `Create a Connector` or `Configure TLS for MQTT Listeners`.
- Prerequisites, if the task requires them.
- Numbered steps written in imperative mood.
- Configuration examples or screenshots when they help the user complete the task.
- A verification step when the result can be checked.
- Related links for advanced options or reference information.

### Reference Topics

Use a reference topic for lookup information, such as configuration fields, API parameters, CLI options, limits, default values, or compatibility notes.

A reference topic should:

- Use tables or definition lists for structured information.
- Keep descriptions precise and parallel.
- Include units, default values, allowed values, and version scope where relevant.
- Avoid long conceptual explanations inside parameter descriptions.

### Troubleshooting Topics

Use a troubleshooting topic to help users diagnose and fix a problem.

A troubleshooting topic should include:

- Symptoms or error messages.
- Likely causes.
- Steps to diagnose the issue.
- Resolution steps.
- A way to verify that the issue is fixed.

### Release Notes

Release notes must stay close to the source change.

When editing release notes:

- Preserve PR numbers, issue links, feature names, API paths, configuration paths, and version numbers.
- Do not reuse wording from another version unless the behavior is identical.
- State user-visible behavior first.
- Avoid marketing language.
- Use past tense for completed changes.

## Page Structure

### Titles and Headings

Each Markdown page must have one H1 heading. The H1 must clearly describe the page topic.

Use heading levels in order:

```markdown
# Configure MQTT Listeners

## Add a TCP Listener

### Configure Listener Options
```

Do not skip heading levels. Do not use headings only for visual emphasis.

Use Title Case for:

- Page titles
- Section headings
- Navigation titles
- Table titles
- Table column headings
- Procedure headings
- Tab titles and grouped content titles

Use sentence-style capitalization for normal paragraph text, list items, table cell content, notes, warnings, and descriptions unless the text contains a product name, UI label, API name, configuration name, or other proper noun.

Do not change existing heading capitalization as part of an unrelated edit. Keep headings consistent across the page.

### Introductions

Start each page with a short introduction that answers:

- What is this page about?
- What can the reader do after reading it?
- Which product, edition, version, or deployment mode does it apply to?

Avoid generic introductions such as "This page introduces..." when a direct statement is clearer.

### Prerequisites

Use a `Prerequisites` section or a `tip` container when users must complete setup before the task.

Include only prerequisites that are required for the current task. Do not use prerequisites as a place for long background explanations.

### Procedures

Use numbered lists for multi-step procedures.

Procedure rules:

- Use imperative mood: `Click`, `Select`, `Enter`, `Configure`, `Run`.
- Use complete sentences and end each sentence with a period.
- Keep each step focused on one user action or a small group of related actions.
- Mark optional steps with `[Optional]` at the beginning of the step.
- Include the final action, such as clicking **Create**, **Save**, or **Apply**.
- Add a verification step when possible.

Example:

```markdown
1. In the left navigation menu, click **Data Integration** -> **Connectors**.
2. Click **Create**.
3. Select **MQTT** from the connector type list.
4. Configure the required fields.
5. Click **Create**.
6. Verify that the connector status is **Connected**.
```

Keep procedures short. If a procedure becomes long, split it into phases or separate task sections.

### Related Links

Use related links when users need reference information or a follow-up task.

Do not add unrelated links only because they mention the same feature. Link text must describe the target page.

## Language Style

### Voice, Person, and Tense

Use second person (`you`) when addressing the reader. Use `EMQX` or the full product name when referring to the system.

Use active voice and present tense where possible.

Use passive voice only when:

- The actor is unknown or unimportant.
- The receiver of the action is more important.
- Active voice would blame the user in an error or warning.

Examples:

| Recommended | Not Recommended |
| --- | --- |
| EMQX stores the configuration in `cluster.hocon`. | The configuration is stored by EMQX in `cluster.hocon`. |
| The file is saved. | You saved the file. |
| More than 50 conflicts were found. | You created more than 50 conflicts. |

### Words and Sentences

Use simple and precise words.

Follow these rules:

- Use `to` instead of `in order to`.
- Use `because` instead of `since` when you mean cause and effect.
- Avoid unnecessary adverbs, such as `very`, `quite`, `simply`, `easily`, and `obviously`.
- Avoid weak or vague verbs when a precise verb is available.
- Avoid connecting more than three clauses with `and`, `or`, or `but`.
- Avoid line breaks inside a paragraph.
- Avoid one-word or overly fragmented paragraphs unless they improve readability.

### Inclusive Language

Avoid terms that can carry unnecessary bias or historically loaded meaning.

| Use | Do Not Use |
| --- | --- |
| primary/subordinate | master/slave |
| perimeter network | demilitarized zone, DMZ |
| stop responding | hang |

Do not use gendered pronouns in generic references.

Avoid vague pronouns such as `this`, `that`, and `it` when the referent is unclear.

### Timeless Wording

Technical documentation can be read long after it is written. Avoid wording that depends on the time of writing.

Avoid:

- `currently`
- `does not yet`
- `eventually`
- `existing`
- `future`
- `in the future`
- `latest`
- `new`
- `newer`
- `now`
- `old`
- `older`
- `presently`
- `soon`

When a feature is version-specific, state the version explicitly.

Recommended:

```markdown
Starting from EMQX 5.8.4, EMQX includes the `base.hocon` file.
```

Not recommended:

```markdown
The latest version now includes the new `base.hocon` file.
```

## Terminology and Product Names

Use official product and feature names consistently.

Product names include:

- EMQX
- EMQX Enterprise
- EMQX Open Source
- EMQX Cloud
- EMQX ECP
- HStreamDB
- Neuron
- NanoMQ
- XMeter
- MQTTX

Do not create acronyms from product names or feature names.

Use full product names when the distinction matters. For example, use `EMQX Enterprise` when a feature is Enterprise-only.

Use `Dashboard` when referring to the EMQX Dashboard after the first mention on a page.

## Acronyms

Only use acronyms that the target audience is likely to know.

Rules:

- Spell out an acronym on first use, followed by the acronym in parentheses: `Internet of Things (IoT)`.
- Do not spell out common acronyms such as `API`, `CLI`, `HTML`, `HTTP`, `JSON`, `MQTT`, `RAM`, `REST`, `SQL`, `TLS`, `URL`, and `USB`.
- Do not introduce an acronym if it is used only once.
- Avoid acronyms in titles and headings. If an acronym is necessary in a heading, spell it out in the following body text.
- Use `a` or `an` according to pronunciation: `a URL`, `an MQTT client`.
- Add `s` to make acronyms plural: `APIs`.
- Do not use acronyms as verbs.

## Formatting

### Text Formatting

Use formatting consistently.

| Item | Format | Example |
| --- | --- | --- |
| UI labels, buttons, fields, tabs, and menu items | Bold | Click **Create**. |
| Parameters, variables, values, file names, paths, directories, commands, and code | Code | Set `node.name` in `etc/emqx.conf`. |
| Error messages | Quotation marks or code, depending on context | If you see `"connection refused"`, check the listener port. |
| Emphasis | Italic, used sparingly | This setting is *not* synchronized across nodes. |

Do not use bold for general emphasis. Do not use code formatting for product names or ordinary technical terms.

### Numbers and Units

Use numerals for numbers.

Rules:

- Do not start a sentence with a numeral. Rewrite the sentence instead.
- Use commas in numbers with four or more digits: `1,000`.
- Do not abbreviate `thousand`, `million`, and `billion` as `K`, `M`, and `B` in prose.
- Do not add a space between a currency symbol and the number: `$0.01`, `¥0.01`.
- Add a space between a number and a unit in prose: `10 MB`, `5 seconds`.
- Preserve exact values, units, and limits from source material.

### Punctuation

Rules:

- Use a colon at the end of a phrase that directly introduces a list.
- When a title or heading contains a colon, capitalize the word after the colon.
- Use the Oxford comma in a list of three or more items: `Android, iOS, and Windows`.
- End complete sentences with a period.
- Do not use periods in headings.
- Use a slash only for established combinations, such as `TCP/IP`.

Avoid em dashes unless there is a clear need.

## UI Text and Interactions

Match UI labels exactly as they appear in the product.

Use `->` for short navigation paths. Include one space before and after `->`. Do not make the arrow bold.

Example:

```markdown
In the Dashboard, click **Data Integration** -> **Connectors**.
```

Use these verbs and prepositions for UI elements:

| UI Element | Recommended Wording |
| --- | --- |
| Button | Click **Create**. |
| Drop-down list | Select **MQTT** from the **Connector Type** list. |
| Checkbox | Select the **Enable TLS** checkbox. |
| Text field | Enter `emqx1` in the **Name** field. |
| Navigation menu | In the left navigation menu, click **Monitoring**. |
| Icon | In the upper-right corner, click the **Settings** icon. |
| Page | On the **Create Connector** page, configure the connector. |
| Tab | On the **Advanced Settings** tab, configure the timeout. |
| Toggle | Turn on the **Enable** toggle. |
| Dialog | In the **Delete Connector** dialog, click **Confirm**. |
| Section | In the **Connection Information** section, configure the server address. |
| Pane | In the **Received** pane, check the message payload. |

Use `click` for mouse actions and `press` for keyboard actions.

Avoid keyboard shortcuts unless the shortcut is essential to the task.

## EMQX Technical Writing Rules

### Configuration Files and HOCON

EMQX configuration documentation must reflect the current configuration model for the target version.

For EMQX 5.x and later, EMQX uses HOCON as the configuration file format.

Use the correct configuration file names and paths:

- `etc/base.hocon`: The base configuration file. Starting from EMQX 5.8.4, it contains default settings that can be overridden by higher-priority configuration files.
- `data/configs/cluster.hocon`: The cluster-wide dynamic configuration file. Dashboard, REST API, and CLI changes are persisted to this file.
- `etc/emqx.conf`: The immutable configuration file for critical settings such as `node` and `cluster`.
- Environment variables: The highest-priority configuration source for values that use the `EMQX_` prefix.

When writing configuration examples:

- Use `hocon` as the code block language for HOCON examples.
- Use real configuration paths, such as `listeners.tcp.default.bind`.
- State whether the example is intended for `base.hocon`, `cluster.hocon`, `emqx.conf`, Dashboard, REST API, or CLI.
- Do not tell users to manually edit `cluster.hocon` unless the page is explicitly about recovery or advanced maintenance.
- Include version notes when a configuration file or option is version-specific.
- Preserve HOCON quoting when special characters such as `#`, `:`, or `=` are part of a string value.

Example:

```hocon
listeners.tcp.default {
  bind = "0.0.0.0:1883"
  max_connections = 1024000
}
```

When documenting configuration priority, use this order for EMQX 5.8.4 and later:

```text
base.hocon < cluster.hocon < emqx.conf < environment variables
```

For versions earlier than EMQX 5.8.4, do not mention `base.hocon` as an available file.

### CLI Commands

Use fenced code blocks with the `bash` language for shell commands.

Rules:

- Show commands that users can copy and run.
- Include required environment variables or working directory assumptions.
- Avoid shell prompts such as `$` unless the prompt is needed to distinguish input from output.
- Separate command output from command input.

### REST API

Use exact HTTP methods and API paths.

Examples:

- `GET /api/v5/clients`
- `POST /api/v5/rules`

Use code formatting for API paths, request fields, response fields, and status codes.

### MQTT Examples

Use MQTTX CLI or EMQX-native tools for MQTT examples when possible.

When writing MQTT examples:

- Use realistic topics and payloads.
- Preserve MQTT topic wildcard semantics.
- Distinguish topic names from topic filters.
- State QoS, retain flag, username, password, and listener port when they affect the result.

## Markdown and VitePress Rules

EMQX documentation is written in Markdown and rendered with VitePress. Use standard Markdown unless an EMQX or VitePress extension is needed.

### Links

Use descriptive link text.

Recommended:

```markdown
For more information, see [Configuration Files](./configuration.md).
```

Not recommended:

```markdown
For more information, click [here](./configuration.md).
```

Use relative links for internal documentation pages and assets. Verify anchors after changing headings.

### Code Blocks

Use fenced code blocks with three backticks. Do not use indented code blocks.

Always add a language identifier when possible:

- `bash` for shell commands
- `hocon` for EMQX configuration
- `json` for JSON
- `sql` for SQL
- `yaml` for YAML
- `text` for plain text output

Use VitePress line highlighting only when it helps readers focus on changed or important lines.

### Tables

Use tables for structured reference information.

Rules:

- Use Title Case for table titles and column headings.
- Use sentence-style capitalization for table cell content.
- Keep column types parallel.
- Include units, default values, and allowed values when applicable.
- Use periods in table cells only when the cells contain complete sentences or a mix of fragments and sentences.

### Custom Containers

Use VitePress custom containers for information that needs special attention.

```markdown
::: tip
Use tips for helpful information that is not required to complete the task.
:::

::: warning
Use warnings for risks, limitations, compatibility issues, or actions that can cause unexpected behavior.
:::

::: danger
Use danger blocks only for actions that can cause data loss, security exposure, or service interruption.
:::

::: details
Use details blocks for optional examples or long reference content.
:::
```

Do not overuse containers. If most of a page is inside notes or warnings, restructure the page.

### Tabs

Use tabs when users must choose one of several equivalent paths, such as different operating systems, editions, or tools.

Example:

```markdown
:::: tabs type:card

::: tab EMQX Enterprise
Enterprise-specific content.
:::

::: tab EMQX Open Source
Open-source content.
:::

::::
```

Keep tab labels short and use Title Case.

### Images and Screenshots

Use screenshots only when they help users understand a UI layout, complete a complex procedure, or recognize a new or changed interface.

Do not use screenshots for:

- Code samples
- Simple confirmation dialogs
- License text
- Error message text that can be written directly
- Screens that change frequently

Screenshot rules:

- Use unique and meaningful English file names without spaces.
- Store screenshots in the page's `assets` directory when possible.
- Use relative paths, for example, `![Create Connector](./assets/create-connector.png)`.
- Mask or remove hostnames, usernames, passwords, tokens, IP addresses, and other sensitive information.
- Keep screenshots focused on the relevant UI area.
- Make sure the screenshot matches the current UI labels and order.
- Do not rely on screenshots as the only source of required information.

### Conditional Content

Use conditional content blocks when content applies only to EMQX Open Source or EMQX Enterprise.

```markdown
{% emqxce %}
Content for EMQX Open Source.
{% endemqxce %}

{% emqxee %}
Content for EMQX Enterprise.
{% endemqxee %}
```

Keep opening and closing tags on separate lines when the content is more than one short phrase.

## AI-Friendly Writing

Write content that is easy for users, search engines, and AI tools to parse.

Rules:

- Use semantic headings that describe the content, not generic headings such as `Overview` repeated across a page.
- Keep each section focused on one topic or task.
- Make examples self-contained.
- Include required context before code blocks.
- Do not hide important requirements only in screenshots.
- Use stable terms for the same concept across pages.
- Prefer lists and tables for structured information.
- Use exact names for configuration paths, API paths, CLI commands, metrics, and error messages.
- State version scope explicitly when behavior differs by version.

## Localization Guidelines

English source content should be easy to translate and maintain.

Rules:

- Avoid long sentences with nested clauses.
- Keep UI labels, API paths, configuration keys, file paths, and code examples unchanged.
- Do not rewrite technical details only to make the sentence sound smoother.
- Preserve examples, thresholds, default values, version numbers, and placeholders.
- Avoid idioms, jokes, metaphors, and culture-specific references.
- Keep terminology consistent across related English and Chinese pages.

## Navigation and Repository Structure

The documentation navigation is generated from repository configuration files and checked by CI.

When adding or moving pages:

- Update the navigation configuration required by the target branch.
- Use unique paths.
- Do not use anchors in navigation paths.
- Keep the file location consistent with the information architecture.
- Run the repository checks if the branch provides them.

The exact navigation generation flow can differ by branch. Check `README.md`, `dir.yaml`, `directory.json`, `nav.yaml`, `gen.py`, and related CI scripts in the branch that you are editing.

## API and Configuration Reference Updates

Some API and configuration reference files are generated from EMQX source code or schema files.

When updating generated reference content:

- Identify the source of truth before editing.
- Do not manually rewrite generated files unless the branch workflow requires manual post-processing.
- Preserve generated field names, enum values, defaults, and descriptions unless the source is wrong.
- Update both Open Source and Enterprise references when the change applies to both editions.
- Update both English and Chinese references when the change requires localization.
- Preview generated API documentation when possible.

## Review Checklist

Before submitting a documentation change, check the following items:

- The page has one clear H1.
- Headings use Title Case and follow the correct hierarchy.
- The page has a clear purpose and target reader.
- The content uses the correct information type: concept, task, reference, troubleshooting, or release notes.
- Technical details match the source of truth.
- Version-specific behavior is explicitly scoped.
- EMQX product names, feature names, UI labels, configuration paths, API paths, and CLI commands are correct.
- HOCON examples use `hocon` code blocks and valid configuration paths.
- Procedures use imperative mood and include required final and verification steps.
- Links are descriptive and valid.
- Screenshots are current, focused, and free of sensitive information.
- Tables have parallel structure and include units or defaults where relevant.
- Content is concise, consistent, and easy to translate.
- Important context is present in text, not only in images.
- The change does not touch unrelated files or versions.

## References

This guide is adapted for EMQX documentation practice and is informed by the following resources:

- [VitePress Markdown Extensions](https://vitepress.dev/guide/markdown)
- [Google Developer Documentation Style Guide](https://developers.google.com/style)
- [Microsoft Writing Style Guide](https://learn.microsoft.com/en-us/style-guide/welcome/)
- [OASIS DITA 1.3 Specification](https://docs.oasis-open.org/dita/dita/v1.3/dita-v1.3-part1-base.html)
