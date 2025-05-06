import os
import sys
import json

directory_file = sys.argv[1]
docs_path = sys.argv[2]


def get_markdown_file(dir_config, base_path):
    current_files = []
    for row in dir_config:
        if row.get('path'):
            current_files.append(
                f'{base_path}/index.md' if row['path'] == './'
                else f'{base_path}/{row["path"]}.md'
            )
        if row.get('children'):
            current_files += get_markdown_file(row['children'], base_path)
    return current_files


if __name__ == '__main__':
    r = open(f'{docs_path}/{directory_file}', 'r')
    directory_config = json.load(r)
    markdown_files = get_markdown_file(directory_config['cn'], f'{docs_path}/zh_CN')
    markdown_files += get_markdown_file(directory_config['en'], f'{docs_path}/en_US')

    for file_path, dir_list, file_list in os.walk(docs_path):
        for file_name in file_list:
            if not file_name.endswith('.md'):
                continue
            with open(os.path.join(file_path, file_name), 'r', encoding='utf-8') as f:
                lines = f.readlines()
                for line in lines:
                    if line.strip().startswith('<!--@include:'):
                        include_file = line.split('<!--@include: ')[1].split('-->')[0]
                        include_file_path = os.path.join(file_path, include_file)
                        include_file_path = os.path.normpath(include_file_path)
                        markdown_files.append(include_file_path)

    for file_path, dir_list, file_list in os.walk(docs_path):
        for file_name in file_list:
            if not file_name.endswith('.md'):
                continue
            if os.path.join(file_path, file_name) not in markdown_files:
                print(f'Remove {os.path.join(file_path, file_name)}')
                os.remove(os.path.join(file_path, file_name))
