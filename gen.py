#!/usr/bin/env python3

import yaml
import json
import os
import sys
import shutil

if len(sys.argv) != 2:
    print('expecting ce or ee as arg1')
    exit(1)

if sys.argv[1] != r'ce' and sys.argv[1] != r'ee':
    print('expecting ce or ee as arg1')
    exit(2)

## check if the 'lang' field matches expected input
## when no 'lang' is defined, it matches both 'en', 'cn' and 'ja'
def is_lang_match(i, lang):
    if isinstance(i, dict) and ('lang' in i):
        return i['lang'] == lang
    else:
        return True

EDITION = sys.argv[1]

# Determine supported languages based on actual doc content
SUPPORTED_LANGS = ['en', 'cn']
if os.path.isdir('ja_JP'):
    ja_dirs = [d for d in os.listdir('ja_JP')
               if os.path.isdir(os.path.join('ja_JP', d)) and d != 'configuration']
    if ja_dirs:
        SUPPORTED_LANGS.append('ja')

# parser document env file
def parse_env_file(file_path):
    config = {}
    with open(file_path, 'r') as file:
        for line in file:
            line = line.strip()
            if line:
                key, value = line.split('=')
                config[key] = value
    return config

## check if the 'edition' field matches expected input
## when no 'edition' is defined, it matches both 'ce' and 'ee'
def is_edition_match(i, ce_or_ee):
    if 'edition' in i:
        return i['edition'] == ce_or_ee
    else:
        return True

def read_title_from_md(lang, path):
    #print(f"Reading title from {path} for lang {lang}", file=sys.stderr)
    if lang == 'en':
        dir = 'en_US'
    elif lang == 'cn':
        dir = 'zh_CN'
    elif lang == 'ja':
        dir = 'ja_JP'
    full_path = dir + '/' + path + '.md'
    if not os.path.isfile(full_path):
        return None
    with open(full_path) as f:
        for line in f:
            if line.strip():
                return line.strip('\n').strip('#').strip()

def parse(children, lang, edition):
    acc=[]
    for i in range(len(children)):
        child = children[i]
        if 'path' in child:
            child['path'] = child['path'].replace('${edition}', edition)
        if isinstance(child, str):
            child = child.replace('${edition}', edition)
        if not is_lang_match(child, lang):
            continue
        if not is_edition_match(child, edition):
            continue

        if 'title_en' in child:
            title = child['title_en']
            if lang == 'cn' and 'title_cn' in child:
                title = child['title_cn']
            elif lang == 'ja' and 'title_ja' in child:
                title = child['title_ja']
        else:
            title = read_title_from_md(lang, child)
            if title is None:
                continue
        _child = {'title': title}

        if isinstance(child, str):
            _child['path'] = child
        else:
            if 'path' in child:
                _child['path'] = child['path']

            if 'collapsed' in child:
                _child['collapsed'] = child['collapsed']

            if 'children' in child:
                godeep = parse(child['children'], lang, edition)
                _child['children'] = godeep

        acc.append(_child)
    return acc

def move_manual(lang, edition):
    if lang == 'cn':
        lang = 'zh'
        baseDir = 'zh_CN'
    elif lang == 'ja':
        baseDir = 'ja_JP'
    else:
        baseDir = 'en_US'
    source_path = f'cfg-manual-docgen/configuration-manual-{edition}-{lang}.md'
    if lang == 'ja':
        source_path = f'cfg-manual-docgen/configuration-manual-{edition}-en.md'

    target_path = f'{baseDir}/configuration/configuration-manual.md'
    if not os.path.isfile(source_path) or not os.path.isdir(baseDir):
        return
    os.makedirs(os.path.dirname(target_path), exist_ok=True)
    shutil.copyfile(source_path, target_path)

with open(r'dir.yaml', encoding='utf-8') as file:
    # Read file and replace the str with env variable
    content = file.read()
    version = parse_env_file(r'current-version.env')
    for key in version:
        content = content.replace('${' + key + '}', version[key])

    # The FullLoader parameter handles the conversion from YAML
    # scalar values to Python the dictionary format
    all = yaml.load(content, Loader=yaml.FullLoader)
    for lang in SUPPORTED_LANGS:
        move_manual(lang, EDITION)

    res = {}
    if isinstance(all, list):
        # Original format: dir.yaml is a flat list
        for lang in SUPPORTED_LANGS:
            res[lang] = parse(all, lang, EDITION)
    elif isinstance(all, dict):
        # Multi-path format: dir.yaml is a dict with path prefixes as keys
        for lang in SUPPORTED_LANGS:
            res[lang] = {}
        for path_prefix, children in all.items():
            for lang in SUPPORTED_LANGS:
                res[lang][path_prefix] = parse(children, lang, EDITION)
    else:
        print('dir.yaml must be a list or a dict', file=sys.stderr)
        exit(3)

    # Optionally parse nav.yaml for top navigation config
    if os.path.isfile('nav.yaml'):
        with open('nav.yaml', encoding='utf-8') as nav_file:
            nav_content = nav_file.read()
            for key in version:
                nav_content = nav_content.replace('${' + key + '}', version[key])
            nav_all = yaml.load(nav_content, Loader=yaml.FullLoader)
            if nav_all:
                nav = {}
                for lang in SUPPORTED_LANGS:
                    nav[lang] = parse(nav_all, lang, EDITION)
                res['nav'] = nav

    json.dump(res, sys.stdout, indent=2, ensure_ascii=False)