#!/usr/bin/env python3
"""Expand shared sections in an explicitly selected disposable build copy.

Tracked Markdown remains the source of truth. Only invoke --in-place against
the staging checkout, before navigation generation and unused-page removal.
"""
import argparse
from pathlib import Path
from markdown_sections import INCLUDE, _mask_code, expand_markdown


def expand_build_copy(root):
    root = Path(root).resolve()
    if not root.is_dir() or root == Path(root.anchor):
        raise ValueError("Expected a dedicated content directory")
    pending = {}
    for path in sorted(root.rglob("*.md")):
        text = path.read_text(encoding="utf-8")
        visible, _ = _mask_code(text)
        if "<!-- include:" in visible or INCLUDE.search(visible):
            pending[path] = expand_markdown(path, root, project_root=root).text
    # Resolve all directives before the first write, so a broken dependency
    # never leaves a partially expanded set of build inputs.
    for path, text in pending.items():
        path.write_text(text, encoding="utf-8")
    return tuple(pending)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, required=True)
    parser.add_argument("--in-place", action="store_true", required=True,
                        help="Acknowledge that root is a disposable build copy")
    args = parser.parse_args()
    pages = expand_build_copy(args.root)
    print(f"Expanded shared sections in {len(pages)} build-copy pages")
