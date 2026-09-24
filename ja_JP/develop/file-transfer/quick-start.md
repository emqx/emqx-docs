# MQTTファイル転送のクイックスタート

このページでは、EMQXのMQTTファイル転送機能を素早く始めるためのステップバイステップガイドを提供します。ローカルディスクとS3バケットを使用したファイル保存の2つのシナリオを扱います。以下の手順に従って、必要な設定を行い、ファイルをアップロードし、提供されるAPIを使ってアクセスしてください。

::: warning 重要なお知らせ

ネームスペース付きダッシュボードユーザーおよびAPIキーは、ロールやスコープに関わらず、ファイル転送のエンドポイントを使用してファイルの一覧表示やローカル保存ファイルのダウンロードはできません。グローバルダッシュボードユーザーまたはグローバルAPIキーで認証してください。S3ストレージの場合、EMQX認証はファイル一覧の取得時のみ必要で、S3は返される署名付きURLを通じてアクセスを制御します。

:::

## ファイルをアップロードしてローカルディスクに保存する

1. EMQXの設定ファイルで以下の設定を行い、ファイル転送機能を有効にしてEMQXを起動します。

   ```bash
   file_transfer {
      enable = true
   }
   ```

   この設定では、アップロードされた断片ファイルをローカルディスクに保存し、転送完了後に断片ファイルはマージされません。

2. 別のシェルで以下のコマンドを実行し、ファイル転送のサンプルプログラム `emqx-ft` をダウンロードしてテストクライアント環境をセットアップします。

   ```bash
   git clone https://github.com/emqx/emqx-ft.git
   cd emqx-ft
   python3 -m venv .venv
   source .venv/bin/activate
   pip install .
   ```

3. `emqx-ft` コマンドラインツールを使って以下のコマンドでファイルをアップロードします。

   ```bash
   emqx-ft --file test-file.txt \
      --file-id file-id-1 --segment-size 10 \
      --client-id client-1 \
      --file-name uploaded-test-file.txt
   ```

   コマンドのパラメータ説明は以下の通りです：

   | パラメータ        | 説明                                                         |
   | ---------------- | ------------------------------------------------------------ |
   | `--file`         | アップロードするファイルのパス。                              |
   | `--file-id`      | アップロードするファイルの一意識別子。                        |
   | `--segment-size` | ファイルのセグメントサイズ（バイト単位）。大きなファイルを小さなセグメントに分割してアップロードするために使用します。 |
   | `--client-id`    | ファイルアップロード操作を行うクライアントのID。              |
   | `--file-name`    | アップロード後のファイル名。                                  |

4. 以下のコマンドを使ってファイル保存ディレクトリに移動し、アップロードされたファイルを手動で一覧表示します。

   ```bash
   $ tree /var/lib/emqx/file_transfer/exports
   /var/lib/emqx/file_transfer/exports
   _./data/file_transfer/exports
   ├── 8E
   │   └── B5
   │       └── 7023DA998C12F0B2A6CA586027E48BEC6271
   │           └── client-1
   │               └── file-id-1
   │                   ├── uploaded-test-file.txt
   │                   └── uploaded-test-file.txt.MANIFEST.json
   └── tmp
   ```

5. 以下のコマンドを実行してHTTP API経由でアップロードされたファイルの一覧を取得します。

   レスポンスにはファイル名、サイズ、タイムスタンプなどの詳細が含まれます。

   ```bash
   $ curl -u '...' -s 'http://127.0.0.1:18083/api/v5/file_transfer/files' | jq
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

6. 以下のコマンドを使って提供されたAPIエンドポイントからファイルをダウンロードします。

   ダウンロードしたファイルはEMQXから取得されます。

   ```bash
   curl -u '...' -s 'http://127.0.0.1:18083/api/v5/file_transfer/file?node=emqx%40127.0.0.1&fileref=8E%2FB5%2F7023DA998C12F0B2A6CA586027E48BEC6271%2Fclient-1%2Ffile-id-1%2Fuploaded-test-file.txt'
   ```

## ファイルをアップロードしてS3バケットに保存する

ファイル転送機能は、Amazon S3やMinIOなどのS3互換オブジェクトストレージシステムへのエクスポートをサポートしています。S3バケットを使用する場合、EMQXはファイル自体ではなくファイル転送リストのみを管理します。

::: tip 前提条件

開始前に`s3cmd`がインストールされ、正しく設定されていることを確認してください。詳細は[公式s3cmdリポジトリ](https://github.com/s3tools/s3cmd)を参照してください。

:::

1. 設定ファイルでファイル転送機能を有効にし、S3バケットを設定してEMQXを起動します。

   ```bash
   file_transfer {
      # ファイル転送機能を有効化
      enable = true

      # ファイルをS3バケットにエクスポート
      storage.local.exporter.s3 {
         host = "s3.us-east-1.amazonaws.com"
         port = 443

         # S3アクセス用の認証情報
         access_key_id = "AKIA27EZDDM9XLINWXFE"
         secret_access_key = "******"

         # ファイルエクスポート先のバケット
         bucket = "my-bucket"

         # S3とのHTTP(S)接続設定。安全なファイルアップロードと接続プール管理を可能にします。
         transport_options {
            ssl.enable = true
            connect_timeout = 15s
         }
      }
   }
   ```

2. 別のシェルで以下のコマンドを実行し、ファイル転送のサンプルプログラム `emqx-ft` をダウンロードしてテストクライアント環境をセットアップします。

   ```bash
   git clone https://github.com/emqx/emqx-ft.git
   cd emqx-ft
   python3 -m venv .venv
   source .venv/bin/activate
   pip install .
   ```

3. `emqx-ft` コマンドラインツールを使って以下のコマンドでファイルをアップロードします。

   ```bash
   emqx-ft --file test-file.txt \
      --file-id file-id-1 --segment-size 10 \
      --client-id client-1 \
      --file-name uploaded-test-file.txt
   ```

   コマンドのパラメータ説明は以下の通りです：

   | パラメータ        | 説明                                                         |
   | ---------------- | ------------------------------------------------------------ |
   | `--file`         | アップロードするファイルのパス。                              |
   | `--file-id`      | アップロードするファイルの一意識別子。                        |
   | `--segment-size` | ファイルのセグメントサイズ（バイト単位）。大きなファイルを小さなセグメントに分割してアップロードするために使用します。 |
   | `--client-id`    | ファイルアップロード操作を行うクライアントのID。              |
   | `--file-name`    | アップロード後のファイル名。                                  |

4. S3コマンドラインツールを使って以下のコマンドを実行し、アップロードされたファイルを手動で一覧表示します。

   ::: tip

   `s3cmd`がインストールされ、正しく設定されていることを必ず確認してください。詳細は[公式s3cmdリポジトリ](https://github.com/s3tools/s3cmd)を参照してください。

   :::

   ```bash
   $ s3cmd ls -r s3://YOURBUCKET/
   2023-06-12 22:58          168  s3://YOURBUCKET/client-1/file-id-1/uploaded-test-file.txt
   ```

   出力には指定したS3バケット内のアップロード済みファイルが表示されます。

5. 以下のコマンドを使ってHTTP API経由でアップロードされたファイルの一覧を取得します。

   ```bash
   $ curl -u '...' -s 'http://127.0.0.1:18083/api/v5/file_transfer/files' | jq
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

6. 以下のコマンドを使って提供されたURIから直接S3ストレージからファイルをダウンロードします。

   ::: tip

   S3エクスポーターのシナリオでは、提供されるダウンロードリンクはEMQXではなく直接S3ストレージを指しているため、ファイルはEMQX上にローカル保存されていません。

   :::

   ```bash
   $ curl "https://s3.eu-north-1.amazonaws.com/YOURBUCKET/client-1/file-id-1/uploaded-test-file.txt?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=...&X-Amz-SignedHeaders=host&X-Amz-Signature=..."
   ```
