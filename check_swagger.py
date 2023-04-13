#!/usr/bin/python3

import json
import os
import sys

if len(sys.argv) < 2:
    print("Please provide the names of the files to open as command-line arguments.")
    sys.exit(1)

for filename in sys.argv[1:]:
    print(f"Processing file {filename}...")

    swEN = open(filename).read()
    swEN = json.loads(swEN)

    for item in swEN['paths'].items():
        key, value = item
        for item2 in value.items():
            method, api = item2
            title = api.get('summary') or api.get('description')
            if '<br/>' in title:
                print({
                    'method': method,
                    'key': key,
                    'title': title,
                    'filename': filename
                })

    print(f"Finished processing file {filename}.\n")

num_files = len(sys.argv[1:])
print(f"All {num_files} files processed.")
