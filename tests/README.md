# Run Documentation Regression Tests

Run these checks from the repository root with Python 3.9 or later and PyYAML installed. CI uses Python 3.11 and PyYAML 6.0.3.

```bash
python3 -m unittest discover -s tests -p 'test_*.py' -v
```

The suite contains 31 functional regression tests and 6 migration-specific tests. The tests use temporary directories for generated output and do not expand shared sections in tracked source files.

## Functional Regression Tests

| File | Coverage |
| --- | --- |
| `test_markdown_links.py` | Link destinations, required angle brackets, reference links, images, and code preservation. |
| `test_markdown_title.py` | H1 and front matter title extraction, encoding, and invalid metadata. |
| `test_shared_sections.py` | Section selection, nested include safety, link rebasing, code preservation, repeatable build-copy expansion, and validation before writes. |
| `test_documentation_structure.py` | Tutorial and reference roles, navigation groups, task sequence, and pagination. |
| `test_documentation_links.py` | Local links and anchors in the nine migrated pages after shared section expansion, one H1 per page, and source preservation. |

Navigation and content assertions describe the documented structure. Update them deliberately when the information architecture changes. The link check covers the pages listed in `test_documentation_links.py`; it is not a full-site link checker. It excludes the preserved homepage overview content from its link checks.

## Migration-Specific Tests

- `test_migration_pilot.py` validates the isolated six-page fixture, generated navigation, HTML checker failure behavior, and output safety.
- `test_migration_batch1.py` checks that legacy pages, navigation entries, and homepage overview content remain available during migration.

Run only these checks with:

```bash
python3 -m unittest discover -s tests -p 'test_migration_*.py' -v
```

Keep these checks while the migration depends on their compatibility guarantees. Retire them only after confirming the replacement coverage and legacy-entry policy. For fixture generation and frontend verification, see [Validate the Documentation Migration Pilot](README-migration-pilot.md).

## Shared Section Contract

Use a standalone `<!-- include: relative.md#heading-slug -->` outside code blocks. Selection includes the heading and ends at the next heading of equal or higher level. `#intro` and `#body` are reserved selectors. Heading lookup is case-insensitive through the preprocessor's slug convention; changing Title Case alone preserves its ordinary Latin anchors. Special characters and duplicate headings still require output checks against VitePress.

The preprocessor resolves nested includes, fails on cycles, missing or ambiguous sections, and paths outside the content root, and rebases Markdown links, reference links, and Markdown image destinations. It preserves code blocks. It does not promise HTML `href`/`src` rebasing; use Markdown links and images for shared sections.

## Expand Production Build Copies

Tracked English Markdown retains section directives. The deployment workflow runs `python3 .github/scripts/expand_shared_sections.py --root en_US --in-place` in its disposable `docs-files` checkout before navigation generation and unused-page removal. Do not run this command in an authoring checkout: it replaces directives with expanded content. All selected sections are resolved before any page is written. Repeating expansion does not change already expanded pages. Chinese and Japanese sources are not expanded by this step.

## CI Coverage

`.github/workflows/migration-pilot.yaml` runs the complete `test_*.py` suite, including both functional and migration-specific checks. The workflow filename and job identifier are retained to avoid unnecessary changes to CI references.
