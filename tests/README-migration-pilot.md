# Validate the Documentation Migration Pilot

This opt-in fixture exercises the proposed four workspaces without changing production Markdown, `dir.yaml`, or `nav.yaml`. Run it before preparing a migration PR. Its task excerpts come from the supplied Enterprise rebuild; their command bytes and troubleshooting conditions are preserved. Titles and table headings follow `DOCS-WRITING-GUIDE-EN.md`. The fixture is not complete product documentation or evidence of runtime verification.

## Run Migration-Specific Tests

Use Python 3.9 or later and PyYAML. This command runs only the isolated fixture and legacy-entry retention checks:

```bash
python3 -m unittest discover -s tests -p 'test_migration_*.py' -v
```

For the complete suite, functional test inventory, shared section contract, and build-copy safety requirements, see [Run Documentation Regression Tests](README.md). CI runs the complete suite.

## Generate Isolated Content

Choose a new output directory outside the checkout. The generator refuses existing destinations and never cleans production documentation:

```bash
python3 .github/scripts/prepare_migration_pilot.py --output /tmp/emqx-migration-pilot-output
```

The output contains generated `en_US`, `directory.json`, and an include dependency manifest. Pilot directory-entry links are normalized to trailing-slash paths in generated navigation so they match VitePress routing and pagination. Production navigation generation is not normalized by this PR. Copy `en_US` into an isolated formal frontend checkout under `docs/en/emqx/v6.3/`, and copy `directory.json` into `docs/.vitepress/config/`. Build with `DOCS_TYPE=emqx VERSION=v6.3` and the frontend's Node.js and pnpm versions. Do not overwrite an unrelated local preview.

## Validate the Formal Frontend

Check workspace selection, page titles, the MQTTX CLI section, the shared section's Reference link, the relative SVG image, and directory-index pagination. A successful build alone is insufficient because the frontend can ignore dead links. Check generated HTML links and fragment IDs separately:

```bash
python3 .github/scripts/check_migration_pilot_html.py --dist /path/to/frontend/docs/.vitepress/dist
```

This check expects the six-page v6.3 fixture, validates content links, images, and fragment IDs, and fails on missing destinations. It does not replace a full-site link checker or browser navigation testing.

The current pilot does not implement old-URL redirects or EN/ZH/JA correspondence. Do not enable new production navigation until those policies and tests are ready. Chinese and Japanese product content is unchanged.
