import sys
import json

version_list = sys.argv[1:]

if __name__ == '__main__':
    version_list.sort(
        key=lambda v: [int(u) for u in v.replace('v', '').split('.')],
        reverse=True
    )
    print(json.dumps(version_list))
