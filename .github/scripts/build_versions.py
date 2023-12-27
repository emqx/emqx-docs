import sys
import json

current_tag = sys.argv[1]
tag_list = sys.argv[2:]
if len(tag_list) == 1:
    tag_list = tag_list[0].split(' ')
tag_list.sort(reverse=True)

current_version = current_tag.split('-')[0]
if current_tag.startswith('v'):
    docs_type = 'broker'
else:
    docs_type = 'enterprise'


def find_latest_tag(v):
    for tag in tag_list:
        if tag.startswith(v):
            return tag
    return None


version_list = list(map(lambda v: v.split('-')[0], tag_list))
version_list = list(set(version_list))
version_list.sort(
    key=lambda v: [int(u) for u in v[1:].split('.')],
    reverse=True
)

same_version = filter(lambda x: x.startswith(current_version.split('.')[0]), version_list)
same_version = list(same_version)

if current_version == same_version[0]:
    build_list = [
        {
            'version': current_version,
            'tag': current_tag,
            'docs_type': docs_type,
            'canonical_version': current_version.split('.')[0],
            'latest_version': version_list[0]
        },
        {
            'version': current_version.split('.')[0],
            'tag': current_tag,
            'docs_type': docs_type,
            'canonical_version': current_version.split('.')[0],
            'latest_version': version_list[0]
        }

    ]
    if len(same_version) > 1:
        build_list.append({
            'version': same_version[1],
            'tag': find_latest_tag(same_version[1]),
            'docs_type': docs_type,
            'canonical_version': same_version[1],
            'latest_version': version_list[0]
        })

else:
    build_list = [{
        'version': current_version,
        'tag': current_tag,
        'docs_type': docs_type,
        'canonical_version': current_version,
        'latest_version': version_list[0]
    }]

if docs_type == 'enterprise':
    for _v in build_list:
        _v['version'] = _v['version'].replace('e', 'v')
        _v['canonical_version'] = _v['canonical_version'].replace('e', 'v')
        _v['latest_version'] = _v['latest_version'].replace('e', 'v')


records = {'include': build_list}
print(json.dumps(records))
