# File Transfer Quick Start

## Quick Start with Local Exporter

First, make sure that the file transfer is enabled, e.g. in `etc/emqx.conf`:

```
file_transfer {
    enable = true
}
```

Start the broker:

```
./bin/emqx start
```

Checkout and set up a test client:

```
git clone https://github.com/emqx/emqx-ft.git
cd emqx-ft
python3 -m venv .venv
source .venv/bin/activate
pip install .
```

Upload a file:

```
emqx-ft --file test-file.txt --file-id file-id-1 --segment-size 10 --client-id client-1 --file-name uploaded-test-file.txt
```

List the uploaded files manually:
```
>tree _build/emqx-enterprise/rel/emqx/data/file_transfer/exports
_build/emqx-enterprise/rel/emqx/data/file_transfer/exports
├── 8E
│   └── B5
│       └── 7023DA998C12F0B2A6CA586027E48BEC6271
│           └── client-1
│               └── file-id-1
│                   ├── uploaded-test-file.txt
│                   └── uploaded-test-file.txt.MANIFEST.json
└── tmp
```

List the uploaded files using HTTP API:

```
>curl -u '...' -s 'http://127.0.0.1:18083/api/v5/file_transfer/files' | jq
{
  "files": [
    {
      "clientid": "client-1",
      "fileid": "file-id-1",
      "metadata": {
        "checksum": "8CFE0BE8A1A5C9BF0F019ABAA8AEEA5D1E26251A0B000883C8875C99A5CFF2F8",
        "name": "uploaded-test-file.txt",
        "size": 168
      },
      "name": "uploaded-test-file.txt",
      "size": 168,
      "timestamp": "2023-06-13T00:43:25+02:00",
      "uri": "/api/v5/file_transfer/file?node=emqx%40127.0.0.1&fileref=8E%2FB5%2F7023DA998C12F0B2A6CA586027E48BEC6271%2Fclient-1%2Ffile-id-1%2Fuploaded-test-file.txt"
    }
  ]
}
```

Download the file:

```
>curl -u '...' -s 'http://127.0.0.1:18083/api/v5/file_transfer/file?node=emqx%40127.0.0.1&fileref=8E%2FB5%2F7023DA998C12F0B2A6CA586027E48BEC6271%2Fclient-1%2Ffile-id-1%2Fuploaded-test-file.txt'
```

## Quick Start with S3 Exporter

First, make sure that the file transfer is enabled and the S3 exporter is configured, e.g. in `etc/emqx.conf`:

```
file_transfer {
    enable = true
    storage {
        local {
            enable = true
            exporter {
                s3 {
                    enable = true
                    host = "s3.eu-north-1.amazonaws.com" # or any other S3-compatible storage
                    port = "443"

                    access_key_id = "..."
                    secret_access_key = "..."

                    bucket = "YOURBUCKET"
                    acl = private

                    transport_options {
                        ssl {
                            enable = true
                        }
                    }
                }
            }
        }
    }
}
```

Start the broker:

```
./bin/emqx start
```

Checkout and set up a test client:

```
git clone https://github.com/emqx/emqx-ft.git
cd emqx-ft
python3 -m venv .venv
source .venv/bin/activate
pip install .
```

Upload a file:

```
emqx-ft --file test-file.txt --file-id file-id-1 --segment-size 10 --client-id client-1 --file-name uploaded-test-file.txt
```

List the uploaded files manually (be sure that `s3cmd` is installed and configured):
```
s3cmd ls -r s3://YOURBUCKET/
2023-06-12 22:58          168  s3://YOURBUCKET/client-1/file-id-1/uploaded-test-file.txt
```

List the uploaded files using HTTP API:

```
>curl -u '...' -s 'http://127.0.0.1:18083/api/v5/file_transfer/files' | jq
{
  "files": [
    {
      "clientid": "client-1",
      "fileid": "file-id-1",
      "name": "uploaded-test-file.txt",
      "size": 168,
      "timestamp": "2023-06-13T00:58:53+02:00",
      "uri": "https://s3.eu-north-1.amazonaws.com/YOURBUCKET/client-1/file-id-1/uploaded-test-file.txt?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=...&X-Amz-SignedHeaders=host&X-Amz-Signature=..."
    }
  ]
}
```

Download the file:

```
>curl "https://s3.eu-north-1.amazonaws.com/YOURBUCKET/client-1/file-id-1/uploaded-test-file.txt?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=...&X-Amz-SignedHeaders=host&X-Amz-Signature=..."
```

Note that, unlike the local exporter, the download link does not lead to EMQX, but to the S3 storage because the file is not stored locally.
