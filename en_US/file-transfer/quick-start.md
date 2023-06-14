# File Transfer Quick Start

This page provides a step-by-step guide to quickly get started with the File Transfer over MQTT feature in EMQX. It covers two scenarios: using the Local Exporter and using the S3 Exporter for file storage. Follow the instructions below to set up the necessary configurations, upload files, and access them using the provided APIs.

## Quick Start with Local Exporter

1. Enable the file transfer feature by setting the following configuration in the EMQX configuration file `etc/emqx.conf`:

   ```bash
   file_transfer {
       enable = true
   }
   ```

2. Start EMQX by executing the following command:

   ```bash
   ./bin/emqx start
   ```

3. Set up the test client. Clone the `emqx-ft` repository from GitHub and prepare the test client environment. Run the following commands:

   ```bash
   git clone https://github.com/emqx/emqx-ft.git
   cd emqx-ft
   python3 -m venv .venv
   source .venv/bin/activate
   pip install .
   ```

4. Use the `emqx-ft` command-line tool to upload a file. Execute the following command, replacing the placeholders with appropriate values:

   ```css
   emqx-ft --file test-file.txt --file-id file-id-1 --segment-size 10 --client-id client-1 --file-name uploaded-test-file.txt
   ```

5. Manually list the uploaded files by navigating to the file storage directory. Execute the following command. 

   The directory structure displayed will contain the uploaded file.

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

6. Use the HTTP API to retrieve a list of uploaded files. Execute the following command, replacing the necessary placeholders.

   The response will contain details about the uploaded file, including its name, size, and timestamp.

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

7. Use the provided API endpoint to download the file. Execute the following command, replacing the necessary placeholders.

   The downloaded file will be retrieved from EMQX.

   ```
   >curl -u '...' -s 'http://127.0.0.1:18083/api/v5/file_transfer/file?node=emqx%40127.0.0.1&fileref=8E%2FB5%2F7023DA998C12F0B2A6CA586027E48BEC6271%2Fclient-1%2Ffile-id-1%2Fuploaded-test-file.txt'
   ```

## Quick Start with S3 Exporter

The file transfer allows exporting uploaded files to an S3-compatible object storage system, such as Amazon S3. 

::: tip Prerequisite

Before you start, you need to ensure that `s3cmd` is installed and configured correctly. You can refer to [Official s3cmd repo)](https://github.com/s3tools/s3cmd) for more information.

:::

1. Enable the file transfer feature and configure the S3 exporter in the EMQX configuration file  `etc/emqx.conf`:

   ```bash
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

2. Start EMQX by executing the following command:

   ```bash
   ./bin/emqx start
   ```

3. Set up the test client. Clone the `emqx-ft` repository from GitHub and prepare the test client environment. Run the following commands:

   ```bash
   git clone https://github.com/emqx/emqx-ft.git
   cd emqx-ft
   python3 -m venv .venv
   source .venv/bin/activate
   pip install .
   ```

4. Use the `emqx-ft` command-line tool to upload a file. Execute the following command, replacing the placeholders with appropriate values:

   ```
   emqx-ft --file test-file.txt --file-id file-id-1 --segment-size 10 --client-id client-1 --file-name uploaded-test-file.txt
   ```

5. Manually list the uploaded files using the S3 command-line tool. Execute the following command:

   ```
   s3cmd ls -r s3://YOURBUCKET/
   2023-06-12 22:58          168  s3://YOURBUCKET/client-1/file-id-1/uploaded-test-file.txt
   ```

   The output will display the uploaded file in the specified S3 bucket.

6. the HTTP API to retrieve a list of uploaded files. Execute the following command, replacing the necessary placeholders:

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

7. Use the provided URI to download the file directly from the S3 storage. Execute the following command, replacing the necessary placeholders:

   ::: tip:

   In the S3 Exporter scenario, the download link provided does not lead to EMQX but directly to the S3 storage, as the file is not stored locally on EMQX.

   :::

   ```
   >curl "https://s3.eu-north-1.amazonaws.com/YOURBUCKET/client-1/file-id-1/uploaded-test-file.txt?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=...&X-Amz-SignedHeaders=host&X-Amz-Signature=..."
   ```

