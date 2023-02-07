#!/usr/bin/env python3

import yaml
import json
import sys
import shutil

if len(sys.argv) != 2:
    print('expecting ce or ee as arg1')
    exit(1)

if sys.argv[1] != r'ce' and sys.argv[1] != r'ee':
    print('expecting ce or ee as arg1')
    exit(2)

## check if the 'lang' field matches expected input
## when no 'lang' is defined, it matches both 'en' and 'cn'
def is_lang_match(i, en_or_cn):
    if 'lang' in i:
        return i['lang'] == en_or_cn
    else:
        return True

EDITION = sys.argv[1]

## check if the 'edition' field matches expected input
## when no 'edition' is defined, it matches both 'ce' and 'ee'
def is_edition_match(i, ce_or_ee):
    if 'edition' in i:
        return i['edition'] == ce_or_ee
    else:
        return True

def copy_cfg_md(lang):
    if lang == 'en':
        dir = 'en_US'
    else:
        dir = 'zh_CN'
    src = dir + '/admin/cfg-' + EDITION + '.md'
    dst = dir + '/admin/cfg.md'
    shutil.copyfile(src, dst)

def read_title_from_md(lang, path):
    if lang == 'en':
        dir = 'en_US'
    else:
        dir = 'zh_CN'
    path = dir + '/' + path + '.md'
    with open(path) as f:
        for line in f:
            if line.strip():
                return line.strip('\n').strip('#').strip()

def parse(children, lang, edition):
    acc=[]
    for i in range(len(children)):
        child = children[i]
        if not is_lang_match(child, lang):
            continue
        if not is_edition_match(child, edition):
            continue
        if 'title_en' in child:
            title = child['title_en']
            if lang == 'cn' and 'title_cn' in child:
                title = child['title_cn']
            if 'children' in child:
                godeep = parse(child['children'], lang, edition)
                acc.append({'title': title, 'children': godeep})
            else:
                acc.append({'title': title, 'path': child['path']})
        else:
            title = read_title_from_md(lang, child)
            acc.append({'title': title, 'path': child})
    return acc

copy_cfg_md('en')
copy_cfg_md('cn')

with open(r'dir.yaml', encoding='utf-8') as file:
    # The FullLoader parameter handles the conversion from YAML
    # scalar values to Python the dictionary format
    all = yaml.load(file, Loader=yaml.FullLoader)
    en = parse(all, 'en', EDITION)
    cn = parse(all, 'cn', EDITION)
    res ={'en': en, 'cn': cn}
    json.dump(res, sys.stdout, indent=2, ensure_ascii=False)
