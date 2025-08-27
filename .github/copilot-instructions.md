# EMQX Documentation Repository

EMQX Documentation is a multi-language (English, Chinese, Japanese) documentation site built with VitePress, using Python scripts for content generation and Docker for preview/serving. The documentation covers EMQX MQTT broker installation, clustering, security, data integration, and advanced features.

**ALWAYS reference these instructions first** and fallback to search or bash commands only when you encounter unexpected information that does not match the info here.

## Working Effectively

### Bootstrap and Build Repository
- Install system dependencies:
  - **Python3**: Already available (3.12.3+)  
  - **Docker**: Already available (28.0.4+)
  - **PyYAML**: Already available
  - **Node.js**: For markdownlint-cli (install globally)
- Install markdownlint for validation: `sudo npm install -g markdownlint-cli` -- takes 7 seconds
- **IMPORTANT**: Ensure configuration manual files exist:
  - If `python3 gen.py ce` fails with "FileNotFoundError", create placeholder files:
  - `mkdir -p cfg-manual-docgen en_US/configuration zh_CN/configuration ja_JP/configuration`
  - `echo "# Configuration Manual" > cfg-manual-docgen/configuration-manual-ce-en.md`
  - `echo "# Configuration Manual EE" > cfg-manual-docgen/configuration-manual-ee-en.md`
  - Create similar files for `-zh.md` versions
- Generate directory structure: `python3 gen.py ce` or `python3 gen.py ee` -- takes 0.1 seconds
- Validate markdown files: `markdownlint -c .github/workflows/markdown_config.json ./en_US ./zh_CN ./ja_JP` -- takes 7 seconds total. NEVER CANCEL.
- Directory validation: `python3 .github/scripts/directory_check.py directory.json $(pwd)` -- takes 9 seconds

### Preview Documentation Locally  
- **Community Edition Preview**: `./preview.sh ce 8080` -- takes 60 seconds for first run (Docker pull), 30 seconds for subsequent runs. NEVER CANCEL during Docker pull.
- **Enterprise Edition Preview**: `./preview.sh ee 8080` -- same timing as CE. NEVER CANCEL during Docker pull.
- Access at: `http://localhost:8080/en/emqx/latest/` (or `/zh/emqx/latest/`, `/ja/emqx/latest/`)
- Stop preview: `docker rm emqx-doc-preview-ce` or `docker rm emqx-doc-preview-ee`

### Quick Content Generation
- Generate CE navigation: `python3 gen.py ce > directory.json`
- Generate EE navigation: `python3 gen.py ee > directory.json`  
- Update version variables: Edit `current-version.env` with EE_VERSION, CE_VERSION, etc.

## Validation

### Mandatory Validation Steps
- **ALWAYS** run markdown linting before committing: `markdownlint -c .github/workflows/markdown_config.json ./en_US ./zh_CN ./ja_JP`
- **ALWAYS** test directory generation: `python3 gen.py ce && python3 gen.py ee`
- **ALWAYS** verify directory structure: `python3 .github/scripts/directory_check.py directory.json $(pwd)`
- **NEVER CANCEL** the Docker preview startup - it takes up to 60 seconds for image pull on first run
- **NEVER CANCEL** markdown linting - it takes 7 seconds total but may appear to hang

### Manual Testing Scenarios
After making content changes, ALWAYS validate by:
1. **Start Preview**: Run `./preview.sh ce 8080` and wait for "Local: http://localhost:8080/" message
2. **Navigation Test**: Open `http://localhost:8080/en/emqx/latest/` and verify navigation menu loads
3. **Content Verification**: Navigate to 2-3 sections you modified and verify content displays correctly
4. **Multi-Language Check**: Test at least one page in all languages (en, zh, ja)
5. **Cross-References**: Verify internal links work by clicking 2-3 links in modified content
6. **Stop Preview**: `docker rm emqx-doc-preview-ce` when done

### CI/CD Validation
- Lint check runs automatically on PR: `markdownlint -c .github/workflows/markdown_config.json`
- Directory check runs automatically: `python3 .github/scripts/directory_check.py`
- Deploy workflow triggers on tag push, takes 5-10 minutes. NEVER CANCEL deployment builds.

## Common Tasks

### Repository Structure
```
/home/runner/work/emqx-docs/emqx-docs/
├── README.md                    # Main project README
├── CONTRIBUTING-EN.md           # Contribution guide  
├── DOCS-WRITING-GUIDE-EN.md    # Documentation style guide
├── dir.yaml                     # Navigation structure definition
├── gen.py                       # Directory generation script
├── preview.sh                   # Docker preview runner
├── current-version.env          # Version variables
├── en_US/                       # English documentation  
├── zh_CN/                       # Chinese documentation
├── ja_JP/                       # Japanese documentation
├── hocon/                       # Configuration schema files
├── redocly/                     # API documentation specs
├── cfg-manual-docgen/           # Configuration manual source files
├── .github/workflows/           # CI/CD workflows
├── .github/scripts/             # Build automation scripts
└── assets/                      # Shared assets
```

### Key File Contents

#### dir.yaml structure
Navigation is defined in YAML with multi-language titles and paths:
```yaml
- title_en: Section Title
  title_cn: 中文标题  
  title_ja: 日本語タイトル
  path: section/page
  children:
    - subsection/page
```

#### current-version.env contents
```bash
EE_VERSION=5.10.0
CE_VERSION=5.9.0  
EE_MINOR_VERSION=5.10
CE_MINOR_VERSION=5.9
```

### Directory Generation Output (gen.py ce)
Creates JSON structure for VitePress navigation:
```json
{
  "en": [{"title": "EMQX Overview", "path": "./", "children": [...]}],
  "cn": [...],
  "ja": [...]
}
```

### Common Command Outputs

#### Python3 gen.py timing (validated)
```bash
$ time python3 gen.py ce
real    0m0.106s
user    0m0.097s  
sys     0m0.008s
```

#### Markdownlint timing (validated)
```bash  
$ time markdownlint -c .github/workflows/markdown_config.json ./en_US ./zh_CN ./ja_JP
real    0m7.511s  # ~7.5 seconds total for all languages
```

#### Directory validation timing (validated)
```bash
$ time python3 .github/scripts/directory_check.py directory.json $(pwd)
real    0m9.381s  # ~9.4 seconds
```

#### Docker preview startup
```bash
$ ./preview.sh ce 8080
# First run: 30-60 seconds (image pull)
# Subsequent runs: 10-30 seconds
# Success indicator: "Local: http://localhost:8080/"
```

## Content Development

### Documentation Languages
- **en_US/**: English (primary language)
- **zh_CN/**: Simplified Chinese
- **ja_JP/**: Japanese
- Always update English first, then other languages if needed

### Markdown Style  
- Use ATX headers (`# Title`)
- Fenced code blocks with backticks 
- Internal links: `[text](../path/page.md)`
- External links: `[text](https://example.com)`
- Images: `![alt](../assets/image.png)`

### Navigation Updates
1. Edit `dir.yaml` for structural changes
2. Test with `python3 gen.py ce` and `python3 gen.py ee` 
3. Verify navigation in preview before committing

### Version Management
- Update `current-version.env` for version changes
- Use `${EE_VERSION}` and `${CE_VERSION}` placeholders in content
- Run `./cut-release.sh v5.x` to create release tags

## Troubleshooting

### Build Issues
- "PyYAML not available": Already installed, check import syntax
- "Docker not found": Already available, check Docker daemon  
- "Markdownlint not found": Run `sudo npm install -g markdownlint-cli`
- "FileNotFoundError configuration-manual": Create placeholder files as described in Bootstrap section

### Preview Issues
- Port 8080 busy: Change port in `./preview.sh ce 8081`
- Container conflicts: Remove with `docker rm -f emqx-doc-preview-ce`
- Image pull timeout: Wait up to 60 seconds, NEVER CANCEL

### Markdown Linting
- MD025: Multiple H1 headers - fix by using single H1 per file
- MD046: Code block style - use fenced blocks with backticks
- MD001: Header levels - ensure proper hierarchy (H1 → H2 → H3)

### Performance Notes (All Validated)
- Directory generation: ~0.1 seconds
- Markdown linting: ~7.5 seconds total for all languages  
- Directory validation: ~9.4 seconds
- Docker preview startup: 30-60 seconds first run, 10-30 seconds subsequent
- Full CI build: 5-10 minutes including deploy