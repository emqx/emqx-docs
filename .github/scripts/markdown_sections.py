#!/usr/bin/env python3
"""Expand Markdown sections with bounded paths and rebased Markdown links.

An include occupies its own line outside a code block::

    <!-- include: ../connect/example.md#prepare-a-client -->

The heading is included through the next heading of the same or a higher level.
``#intro`` selects the prose after H1 and before the first H2; ``#body`` selects
the document without its front matter or H1. Included links are rebased to the
destination page. The original Develop document remains the maintained source.
"""
from __future__ import annotations

from dataclasses import dataclass
import html
import os
from pathlib import Path
import re
from urllib.parse import quote, unquote, urlsplit


INCLUDE = re.compile(r"^ {0,3}<!--\s*include:\s*(\S+)\s*-->[ \t]*$", re.M)
FRONT_MATTER = re.compile(r"\A---[ \t]*\r?\n.*?\r?\n---[ \t]*(?:\r?\n|$)", re.S)


class IncludeError(ValueError):
    """A shared section or one of its local destinations cannot be resolved."""


@dataclass(frozen=True)
class ExpandedMarkdown:
    text: str
    dependencies: tuple[Path, ...]


@dataclass(frozen=True)
class Heading:
    level: int
    slug: str
    start: int
    end: int


def _code_spans(text: str) -> list[tuple[int, int]]:
    """Find fenced/indented blocks and inline code without modifying their bytes."""
    spans: list[tuple[int, int]] = []
    offset = 0
    fence: tuple[str, int, int] | None = None
    for line in text.splitlines(keepends=True):
        marker = re.match(r"^ {0,3}(`{3,}|~{3,})(.*)$", line.rstrip("\r\n"))
        if fence:
            if marker and marker[1][0] == fence[0] and len(marker[1]) >= fence[1] and not marker[2].strip():
                spans.append((fence[2], offset + len(line)))
                fence = None
        elif marker and not (marker[1][0] == "`" and "`" in marker[2]):
            fence = (marker[1][0], len(marker[1]), offset)
        elif line.startswith(("    ", "\t")):
            spans.append((offset, offset + len(line)))
        offset += len(line)
    if fence:
        spans.append((fence[2], len(text)))
    block_spans = sorted(spans)
    for begin, end in _gaps(len(text), block_spans):
        i = begin
        while i < end:
            if text[i] != "`" or _escaped(text, i):
                i += 1
                continue
            j = i
            while j < end and text[j] == "`":
                j += 1
            ticks = text[i:j]
            closing = re.search(r"(?<!`)" + re.escape(ticks) + r"(?!`)", text[j:end])
            if closing:
                last = j + closing.end()
                spans.append((i, last))
                i = last
            else:
                i = j
    return sorted(spans)


def _gaps(length: int, spans: list[tuple[int, int]]):
    last = 0
    for begin, end in spans:
        if begin > last:
            yield last, begin
        last = max(last, end)
    if last < length:
        yield last, length


def _escaped(text: str, index: int) -> bool:
    count = 0
    while index > 0 and text[index - 1] == "\\":
        count += 1
        index -= 1
    return count % 2 == 1


def _mask_code(text: str) -> tuple[str, dict[str, str]]:
    placeholders: dict[str, str] = {}
    result = []
    last = 0
    for index, (begin, end) in enumerate(_code_spans(text)):
        key = f"\ue000SHARED_CODE_{index}\ue001"
        if key in text:
            raise IncludeError("Reserved shared-content placeholder in Markdown")
        placeholders[key] = text[begin:end]
        result.extend((text[last:begin], key))
        last = end
    result.append(text[last:])
    return "".join(result), placeholders


def _restore_code(text: str, placeholders: dict[str, str]) -> str:
    for key, value in placeholders.items():
        text = text.replace(key, value)
    return text


def _visible_markup(text: str) -> str:
    # Preserve offsets for heading and directive selection.
    chars = list(text)
    for begin, end in _code_spans(text):
        # Inline code is significant in heading labels, but cannot contain an
        # include directive or a heading beginning at the start of a line.
        if "\n" not in text[begin:end] and not text[begin:end].startswith(("    ", "\t")):
            continue
        for index in range(begin, end):
            if chars[index] not in "\r\n":
                chars[index] = " "
    return "".join(chars)


def _heading_slug(label: str) -> str:
    # Mirror build_preview.slug after Markdown inline formatting is rendered.
    label = re.sub(r"!?\[([^\]]+)\]\([^)]*\)", r"\1", label)
    label = re.sub(r"\[([^\]]+)\]\[[^\]]*\]", r"\1", label)
    label = re.sub(r"<[^>]+>", "", label)
    label = re.sub(r"(`+)(.*?)\1", r"\2", label)
    label = re.sub(r"(?<!\\)[*_~]", "", label)
    label = re.sub(r"\\([!\"#$%&'()*+,\-./:;<=>?@\[\\\]^_`{|}~])", r"\1", label)
    return re.sub(r"[^\w-]+", "-", html.unescape(label).lower()).strip("-")


def _headings(text: str) -> list[Heading]:
    visible = _visible_markup(text)
    headings = []
    for match in re.finditer(r"^ {0,3}(#{1,6})[ \t]+(.+?)[ \t]*#*[ \t]*$", visible, re.M):
        headings.append(Heading(len(match[1]), _heading_slug(match[2]), match.start(), match.end()))
    # Existing pages use ATX headings; support ordinary Setext headings too.
    for match in re.finditer(r"^ {0,3}(\S[^\n]*)\n {0,3}(=+|-+)[ \t]*$", visible, re.M):
        if not re.match(r"(?:#{1,6}\s|[-*+]\s|\d+[.)]\s|>)", match[1]):
            headings.append(Heading(1 if match[2][0] == "=" else 2, _heading_slug(match[1]), match.start(), match.end()))
    return sorted(headings, key=lambda h: h.start)


def _select(text: str, selector: str, path: Path) -> tuple[str, set[str]]:
    text = FRONT_MATTER.sub("", text, count=1)
    headings = _headings(text)
    if selector == "intro":
        title = next((h for h in headings if h.level == 1), None)
        start = title.end if title else 0
        end = next((h.start for h in headings if h.level <= 2 and h.start >= start), len(text))
        selected = text[start:end].strip("\r\n")
    elif selector == "body":
        title = next((h for h in headings if h.level == 1), None)
        selected = (text[:title.start] + text[title.end:]) if title else text
        selected = selected.strip("\r\n")
    else:
        matches = [h for h in headings if h.slug == selector]
        if len(matches) != 1:
            reason = "missing" if not matches else "ambiguous"
            raise IncludeError(f"{reason.capitalize()} section #{selector} in {path}")
        heading = matches[0]
        end = next((h.start for h in headings if h.start > heading.start and h.level <= heading.level), len(text))
        selected = text[heading.start:end].rstrip("\r\n")
    anchors = {h.slug for h in _headings(selected)}
    anchors.update(re.findall(r'\bid=["\']([^"\']+)["\']', _visible_markup(selected)))
    return selected, anchors


def _matching(text: str, start: int, opener: str, closer: str) -> int | None:
    depth = 0
    for index in range(start, len(text)):
        if _escaped(text, index):
            continue
        if text[index] == opener:
            depth += 1
        elif text[index] == closer:
            depth -= 1
            if depth == 0:
                return index
    return None


def _destination(value: str) -> tuple[str, str] | None:
    """Split a Markdown destination from its optional, unchanged title."""
    value = value.strip()
    if value.startswith("<"):
        end = next((i for i in range(1, len(value)) if value[i] == ">" and not _escaped(value, i)), None)
        return (value[1:end], value[end + 1:]) if end is not None else None
    depth = 0
    for index, char in enumerate(value):
        if _escaped(value, index):
            continue
        if char == "(":
            depth += 1
        elif char == ")":
            depth -= 1
        elif char.isspace() and depth == 0:
            return value[:index], value[index:]
    return value, ""


def _reference_label(value: str) -> str:
    return re.sub(r"\s+", " ", value).strip().casefold()


def _references(document: str) -> dict[str, tuple[str, str]]:
    visible, _ = _mask_code(document)
    refs = {}
    for match in re.finditer(r"^ {0,3}\[([^\]\n]+)\]:[ \t]*(\S[^\n]*)$", visible, re.M):
        destination = _destination(match[2])
        if destination:
            refs.setdefault(_reference_label(match[1]), destination)
    return refs


def _rewrite_links(text: str, document: str, rebase) -> str:
    """Rewrite inline/image/reference links, preserving all code byte for byte."""
    masked, placeholders = _mask_code(text)
    refs = _references(document)
    # References used by a selected section may be defined outside it. Resolve
    # them to inline links so included documents cannot collide on label names.
    masked = re.sub(r"^ {0,3}\[([^\]\n]+)\]:[ \t]*\S[^\n]*(?:\n|$)", "", masked, flags=re.M)
    output = []
    cursor = 0
    index = 0
    while index < len(masked):
        if masked[index] != "[" or _escaped(masked, index):
            index += 1
            continue
        label_end = _matching(masked, index, "[", "]")
        if label_end is None:
            index += 1
            continue
        label = masked[index + 1:label_end]
        end = label_end + 1
        destination = None
        if end < len(masked) and masked[end] == "(":
            close = _matching(masked, end, "(", ")")
            if close is not None:
                destination = _destination(masked[end + 1:close])
                end = close + 1
        elif end < len(masked) and masked[end] == "[":
            close = _matching(masked, end, "[", "]")
            if close is not None:
                key = masked[end + 1:close] or label
                destination = refs.get(_reference_label(_restore_code(key, placeholders)))
                if destination:
                    end = close + 1
        else:
            destination = refs.get(_reference_label(_restore_code(label, placeholders)))
        if destination is None:
            index = label_end + 1
            continue
        url, title = destination
        new_url = rebase(url, image=index > 0 and masked[index - 1] == "!" and not _escaped(masked, index - 1))
        # Angle brackets protect spaces and parentheses without changing label,
        # optional title, image prefix, or link text.
        output.extend((masked[cursor:index], f"[{label}](<{new_url}>{title})"))
        cursor = end
        index = end
    output.append(masked[cursor:])
    return _restore_code("".join(output), placeholders)


def expand_markdown(
    path: Path,
    content_root: Path,
    context_links: dict[str, str] | None = None,
    *,
    project_root: Path | None = None,
) -> ExpandedMarkdown:
    """Return expanded Markdown and every input file used (absolute paths).

    ``context_links`` uses exact project-relative source URIs, including any
    fragment. It is applied only to links from included content, never to the
    wrapper's authored links. ``project_root`` defaults to ``content_root``'s
    grandparent, matching this project's ``content/en`` layout.
    """
    content_root = Path(content_root).resolve()
    project_root = Path(project_root).resolve() if project_root else content_root.parent.parent
    destination = Path(path).resolve()
    dependencies: set[Path] = set()
    contexts = context_links or {}

    def read(source: Path) -> str:
        if not source.is_relative_to(content_root):
            raise IncludeError(f"Shared-content path is outside content root: {source}")
        if not source.is_file():
            raise IncludeError(f"Missing shared-content file: {source}")
        dependencies.add(source)
        return source.read_text(encoding="utf-8")

    def expand(source: Path, selector: str | None, stack: tuple[Path, ...]) -> str:
        if source in stack:
            raise IncludeError("Shared-content include cycle: " + " -> ".join(str(p) for p in (*stack, source)))
        document = read(source)
        included = selector is not None
        selected, local_anchors = _select(document, selector, source) if included else (document, set())

        def rebase(url: str, *, image: bool = False) -> str:
            # Markdown punctuation escapes are syntax, not part of a URL.
            url = html.unescape(re.sub(r"\\([()<>\[\] ])", r"\1", url))
            parts = urlsplit(url)
            if parts.scheme or parts.netloc:
                return url
            target = (source.parent / unquote(parts.path)).resolve() if parts.path else source
            if not target.is_relative_to(content_root):
                raise IncludeError(f"Included local link is outside content root in {source}: {url}")
            if not target.exists():
                raise IncludeError(f"Missing included local link in {source}: {url}")
            suffix = ("?" + parts.query if parts.query else "") + ("#" + parts.fragment if parts.fragment else "")
            key = target.relative_to(project_root).as_posix() + suffix
            if not image and key in contexts:
                mapped = urlsplit(contexts[key])
                if mapped.scheme or mapped.netloc:
                    raise IncludeError(f"Context destination must be local: {contexts[key]}")
                target = (project_root / unquote(mapped.path)).resolve()
                if not target.is_relative_to(content_root) or not target.is_file():
                    raise IncludeError(f"Invalid context destination: {contexts[key]}")
                suffix = ("?" + mapped.query if mapped.query else "") + ("#" + mapped.fragment if mapped.fragment else "")
            elif not image and target == source and unquote(parts.fragment) in local_anchors and not parts.query:
                return "#" + parts.fragment
            if target == destination and suffix.startswith("#"):
                return suffix
            relative = Path(os.path.relpath(target, destination.parent)).as_posix()
            return quote(relative, safe="/@:+,;=-_.!~*'()") + suffix

        result = []
        cursor = 0
        visible = _visible_markup(selected)
        for directive in INCLUDE.finditer(visible):
            segment = selected[cursor:directive.start()]
            result.append(_rewrite_links(segment, document, rebase) if included else segment)
            value = directive[1]
            parts = urlsplit(value)
            if parts.scheme or parts.netloc or parts.query or not parts.path:
                raise IncludeError(f"Invalid shared-content directive in {source}: {value}")
            include_path = (source.parent / unquote(parts.path)).resolve()
            if include_path.suffix != ".md":
                raise IncludeError(f"Shared-content source must be Markdown: {value}")
            result.append(expand(include_path, unquote(parts.fragment) or "body", (*stack, source)))
            cursor = directive.end()
        segment = selected[cursor:]
        result.append(_rewrite_links(segment, document, rebase) if included else segment)
        expanded = "".join(result)
        visible_expanded, _ = _mask_code(expanded)
        if re.search(r"<!--\s*include:", visible_expanded):
            raise IncludeError(f"Malformed or unresolved shared-content directive in {source}")
        return expanded

    text = expand(destination, None, ())
    return ExpandedMarkdown(text=text, dependencies=tuple(sorted(dependencies)))
