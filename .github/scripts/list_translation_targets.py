# /// script
# requires-python = ">=3.12"
# ///
import json
import sys


def collect_paths(items):
    paths = []
    for item in items:
        if item.get('path'):
            p = item['path']
            if p.startswith(('http://', 'https://')):
                continue
            paths.append('en_US/index.md' if p == './' else f'en_US/{p}.md')
        if item.get('children'):
            paths += collect_paths(item['children'])
    return paths


if __name__ == '__main__':
    directory_file = sys.argv[1]
    with open(directory_file) as f:
        data = json.load(f)
    for path in collect_paths(data.get('en', [])):
        print(path)
