#!/usr/bin/env python3
"""Generate an opt-in, isolated migration fixture without modifying product docs."""
import argparse
import json
from pathlib import Path
import shutil
import subprocess
import sys
from markdown_sections import expand_markdown


def prepare(repository, output):
    repository = repository.resolve()
    output = output.resolve()
    if output.exists():
        raise ValueError("Output must be a new directory; refusing to overwrite files")
    # Prevent generated files from entering a tracked or fixture source directory.
    if output.is_relative_to(repository):
        raise ValueError("Output must be outside the repository")
    fixture = repository / "tests/fixtures/migration-pilot"
    shutil.copytree(fixture, output)
    manifest = []
    for path in sorted((fixture / "en_US").rglob("*.md")):
        expanded = expand_markdown(path, fixture / "en_US", project_root=fixture)
        target = output / path.relative_to(fixture)
        target.write_text(expanded.text, encoding="utf-8")
        manifest.append({"page": str(path.relative_to(fixture)), "dependencies": [str(p.relative_to(fixture)) for p in expanded.dependencies]})
    generated = subprocess.check_output([sys.executable, str(repository / "gen.py"), "ce"], cwd=output, text=True)
    directory = json.loads(generated)
    def normalize_entries(items):
        for item in items:
            path = item.get("path", "")
            if path == "index":
                item["path"] = "./"
            elif path.endswith("/index") and not path.startswith(("https://", "http://")):
                item["path"] = path[:-len("index")]
            normalize_entries(item.get("children", []))
    for items in directory["en"].values():
        normalize_entries(items)
    (output / "directory.json").write_text(json.dumps(directory, indent=2) + "\n", encoding="utf-8")
    (output / "include-manifest.json").write_text(json.dumps(manifest, indent=2) + "\n", encoding="utf-8")
    return output


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    prepare(Path(__file__).resolve().parents[2], args.output)
    print(args.output)
