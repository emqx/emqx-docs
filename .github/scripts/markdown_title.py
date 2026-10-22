"""Read navigation titles without treating YAML front matter as prose."""
from pathlib import Path
import re
import yaml


def read_markdown_title(path):
    lines = Path(path).read_text(encoding="utf-8-sig").splitlines()
    if lines and lines[0].strip() == "---":
        end = next((i for i in range(1, len(lines)) if lines[i].strip() in ("---", "...")), None)
        if end is None:
            raise ValueError(f"Unclosed front matter: {path}")
        metadata = yaml.safe_load("\n".join(lines[1:end]))
        if metadata is not None and not isinstance(metadata, dict):
            raise ValueError(f"Front matter must be a mapping: {path}")
        title = metadata.get("title") if metadata else None
        if isinstance(title, str) and title.strip():
            return title.strip()
        lines = lines[end + 1:]
    for line in lines:
        if line.strip():
            # Preserve the legacy first-content-line fallback for existing pages.
            return re.sub(r"^#{1,6}\s+|\s+#+$", "", line.strip()).strip()
    return None
