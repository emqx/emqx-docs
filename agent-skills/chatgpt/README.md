# EMQX ChatGPT Skills

This directory contains an Agent Skills-compatible folder for helping ChatGPT or Codex work with EMQX Enterprise installation tasks.

## Available Skills

- `emqx-enterprise-installer`: Help install EMQX Enterprise using RPM/DEB packages or Docker.

The skill is a standalone directory with a `SKILL.md` file at its root.

## Install in ChatGPT

ChatGPT installs skills from the Skills page. Upload the skill directory; do not upload the whole `agent-skills/chatgpt/` directory as a single skill.

1. Open ChatGPT.
2. Select your profile icon.
3. Select `Skills`.
4. Select `New skill`.
5. Select `Upload from your computer`.
6. Upload this skill folder:
   - `agent-skills/chatgpt/emqx-enterprise-installer`
7. Wait for ChatGPT to scan the skill, then install or enable it when prompted.

If your workspace requires file uploads instead of folder uploads, create one archive per skill and keep `SKILL.md` at the archive root:

```sh
mkdir -p /tmp/emqx-chatgpt-skills

(cd agent-skills/chatgpt/emqx-enterprise-installer && zip -r /tmp/emqx-chatgpt-skills/emqx-enterprise-installer.zip .)
```

For ChatGPT Enterprise or Edu workspaces, an admin may need to enable skills and skill uploading first. See the OpenAI Help Center article: <https://help.openai.com/en/articles/20001066-skills-in-chatgpt>.

## Install in Codex

Codex loads user-installed skills from `$CODEX_HOME/skills`. If `CODEX_HOME` is not set, use `~/.codex/skills`.

From the root of this repository:

```sh
DEST="${CODEX_HOME:-$HOME/.codex}/skills"
mkdir -p "$DEST"

cp -R agent-skills/chatgpt/emqx-enterprise-installer "$DEST"/
```

Restart Codex after copying the folder so it can discover the new skill.

For local development, symlink the folder instead of copying it:

```sh
DEST="${CODEX_HOME:-$HOME/.codex}/skills"
mkdir -p "$DEST"

ln -sfn "$PWD/agent-skills/chatgpt/emqx-enterprise-installer" "$DEST/emqx-enterprise-installer"
```

Restart Codex after creating or updating symlinks.

## Validate

The skill should contain a valid `SKILL.md` with `name` and `description` frontmatter. If you have the local skill creator tools installed, validate with:

```sh
python3 ~/.codex/skills/.system/skill-creator/scripts/quick_validate.py agent-skills/chatgpt/emqx-enterprise-installer
```
