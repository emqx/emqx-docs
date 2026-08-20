# バックアップとリストア

EMQXは分散ストレージスキーマを採用し、システムの高可用性を確保するためにクラスター転送機能も導入しています。

このページでは、システム障害時のデータ損失を防ぐために、運用データおよび設定ファイルのバックアップ方法について説明します。

## 機能説明

EMQXはバックアップとリカバリーを実現するために、データのインポートおよびエクスポート用のCLIコマンドを提供しています。EMQX 4.xのコマンドに似ていますが、エクスポートファイルのフォーマットは4.xとは互換性がありません。

- EMQX 4.xでは、EMQXの設定および組み込みデータベースの必要なすべてのデータを単一のJSONファイルで保存していました。
- EMQX 5.xでは、エクスポートされたデータはtarファイル形式に圧縮されており、大量のユーザーデータをより効率的かつ構造的に扱うことが可能です。

CLIコマンドに加え、EMQX EnterpriseではEMQXダッシュボードにデータのバックアップおよびリカバリー用のページがあり、そこでデータのインポートおよびエクスポートが可能です。

EMQXがインポートおよびエクスポートをサポートするデータは以下の通りです。

- EMQXの[設定リライトファイル](./configuration/configuration.md#configuration-rewrite-file)の内容：
  - 認証および認可の設定
  - ルール、コネクター、Sink/Source
  - リスナー、ゲートウェイ設定
  - その他のEMQX設定
- 組み込みデータベース（Mnesia）データ
  - ダッシュボードユーザーおよびREST APIキー
  - クライアント認証情報（組み込みデータベースのパスワード認証、強化認証）
  - PSK認証データ
  - 認可ルール
  - ブラックリストデータ
  - 保持メッセージ
- EMQXデータディレクトリ（`node.data_dir`）に保存されているSSL/TLS証明書
- EMQXデータディレクトリに保存されている認可用の`acl.conf`ファイル

::: warning 重要なお知らせ

- 組み込みデータベースの認証情報およびネームスペースに関連する認可ルールは、個別のネームスペース単位でのエクスポートやインポートはできません。これらのレコードをバックアップまたはリストアするには、グローバルバックアップを使用してください。グローバルバックアップはすべてのネームスペースのレコードをまとめて処理します。
- バックアップにはEMQXデータディレクトリに保存されているSSL/TLS証明書および`acl.conf`ファイルのみが含まれます。バックアップをインポートする前に、データディレクトリ外に保存されている証明書や`acl.conf`ファイルは別途適切な場所にコピーしてください。

:::

::: tip バックアップファイルの詳細

- エクスポートされたファイル名の形式は`emqx-export-YYYY-MM-DD-HH-mm-ss.sss.tar.gz`で、エクスポート先ディレクトリは`<EMQX data directory>/backup`です。
- EMQX v5.7.1以降、保持メッセージがストレージ方式として`ram`（メモリ）に設定されている場合でもバックアップされます。

:::

### エクスポート

データは稼働中の任意のクラスターのノードからエクスポート可能です。

### インポート

データをインポートするには、EMQXノードが稼働中である必要があり、インポート操作が成功するためには以下の条件を満たす必要があります。

- [コアノード＋レプリカノード](../develop/cluster/mria-introduction.md)モードが有効な場合、データのインポートはコアノードでのみ実行可能です。これは実際のインポート動作に影響しません。データはコアノードおよびレプリカノードを含むすべてのクラスターのノードにレプリケートされます。コアノードで操作することで正しいデータインポートが保証されます。
- データファイルの名前を変更してはいけません。

上記の条件を満たさない場合、インポート処理は中止され、対応するエラーメッセージが表示されます。

データインポート操作中、データはEMQXに存在しない場合は挿入され、競合がある場合は更新されます。インポート処理は既存のEMQXクラスターのデータを削除しません。

::: tip 特別な注意事項

稀に、既存のデータがインポートされるデータと互換性がない場合があります。例えば、EMQXクラスターが組み込みデータベース認証を使用し、ソルトの位置を「サフィックス（接尾辞）」に設定している一方で、インポートされるデータでは同じ設定が「プレフィックス（接頭辞）」に設定されている場合です。インポート後、新しい設定が有効となり、以前作成された古いユーザー認証情報は機能しなくなります。

そのため、データをクリアせずにEMQXクラスターにデータをインポートする場合は特に注意が必要です。

:::

## ダッシュボードでのバックアップファイル管理

グローバル管理者は**Global**または特定の[ネームスペース](./multi-tenancy/namespace-overview.md)においてバックアップファイルを管理できます。ネームスペース管理者は割り当てられたネームスペースのバックアップファイルを管理およびダウンロードできますが、**Global**や他のネームスペースのバックアップファイルにはアクセスできません。

:::tip

- ダッシュボードによるバックアップおよびリストア機能はEMQX Enterpriseエディションv5.4.0以降で利用可能です。
- CLIでエクスポートされたバックアップファイルもダッシュボードのバックアップ＆リストアページで管理可能です。

:::

1. ダッシュボードにログインし、**System** -> **Backup & Restore**に移動します。

2. グローバル管理者の場合、ネームスペースセレクターから**Global**または特定のネームスペースを選択します。選択したスコープのバックアップファイル一覧が表示されます。ネームスペースを選択した場合は、一覧上部の通知で対象ネームスペースが確認できます。

   ネームスペース管理者はセレクターが表示されません。EMQXはバックアップ操作を割り当てられたネームスペースに制限します。

3. データをエクスポートするには、**Create**をクリックします。グローバル管理者は**Global**ビューでのみバックアップを作成できます。特定のネームスペースを選択した場合、**Create**は無効になります。ネームスペース管理者は割り当てられたネームスペースのバックアップを作成可能です。

   バックアップファイル一覧には以下の情報が表示されます。

   - **File Name**：バックアップファイル名
   - **Node Name**：バックアップファイルが保存されているノード名。バックアップがそのノードのデータのみを含むことを意味しません。
   - **Created At**：バックアップファイルの作成日時
   - **File Size**：バックアップファイルのサイズ

4. バックアップファイルを選択したスコープに追加するには、**Upload**をクリックします。アップロードはデータのリストアを行いません。特定のネームスペースの場合、成功メッセージに対象ネームスペースが表示されます。アップロード成功後、ファイルがバックアップファイル一覧に表示されていることを確認してください。

5. バックアップファイルの管理は、**Actions**列の以下のボタンで行います。

   - **Download**：バックアップファイルをローカルデバイスにダウンロードします。
   - **Delete**：選択したスコープからバックアップファイルを削除します。
   - **Restore**：選択したスコープにバックアップファイルをインポートします。特定のネームスペースを選択している場合は、確認ダイアログで対象ネームスペースを確認してからリストアを実行してください。リストア成功後、成功メッセージに対象ネームスペースが表示されます。

特定のネームスペースビューでは、アップロード、ダウンロード、削除、リストアの操作はそのネームスペースに適用されます。グローバル管理者はこのビューでバックアップファイルの管理およびリストアが可能ですが、バックアップの作成はできません。

### REST APIによるバックアップファイル管理

グローバル管理者は以下のエンドポイントにオプションの`namespace`クエリパラメータを渡すことができます。

- `GET /api/v5/data/files`：バックアップファイル一覧取得
- `POST /api/v5/data/files`：バックアップファイルアップロード
- `GET /api/v5/data/files/{filename}`：バックアップファイルダウンロード
- `DELETE /api/v5/data/files/{filename}`：バックアップファイル削除
- `POST /api/v5/data/import`：バックアップファイルインポート

グローバル管理者が`namespace`を省略した場合、操作は**Global**のバックアップファイルに適用されます。ネームスペース管理者の場合、EMQXはこのパラメータを無視し、割り当てられたネームスペースに操作を適用します。

## CLI例

このセクションでは、コマンドラインインターフェースを使用したデータのインポートおよびエクスポート方法を示します。

1. データをエクスポートします。エクスポートファイルの名前形式は`emqx-export-YYYY-MM-DD-HH-mm-ss.sss.tar.gz`で、エクスポート先ディレクトリは`<EMQX data directory>/backup`です。

    ```bash
    $ ./emqx ctl data export
    Exporting data to "data/backup/emqx-export-2023-06-19-15-14-19.947.tar.gz"...
    Exporting cluster configuration...
    Exporting additional files from EMQX data_dir: "data"...
    Exporting built-in database...
    Exporting emqx_admin database table...
    Exporting emqx_authn_mnesia database table...
    Exporting emqx_enhanced_authn_scram_mnesia database table...
    Exporting emqx_app database table...
    Exporting emqx_acl database table...
    Exporting emqx_psk database table...
    Exporting emqx_banned database table...
    Data has been successfully exported to data/backup/emqx-export-2023-06-19-15-14-19.947.tar.gz.
    ```
2. データをインポートします。インポートするファイル名は絶対パスまたは相対パスで指定可能です。ファイルが`<EMQX data directory>/backup`ディレクトリにある場合は、パスなしのベース名でも指定できます。例：

    ```bash
    # 絶対パスでファイルをインポート
    $ ./emqx ctl data import /tmp/emqx-export-2023-06-19-15-14-19.947.tar.gz
    Importing data from "/tmp/emqx-export-2023-06-19-15-14-19.947.tar.gz"...
    Importing cluster configuration...
    Importing built-in database...
    Importing emqx_banned database table...
    Importing emqx_psk database table...
    Importing emqx_acl database table...
    Importing emqx_app database table...
    Importing emqx_enhanced_authn_scram_mnesia database table...
    Importing emqx_authn_mnesia database table...
    Importing emqx_admin database table...
    Data has been imported successfully.

    # EMQXルートディレクトリからの相対パスでファイルをインポート
    $ ./emqx ctl data import ../../../tmp/emqx-export-2023-06-21-13-28-06.418.tar.gz
    Importing data from "../../../tmp/emqx-export-2023-06-21-13-28-06.418.tar.gz"...
    Importing cluster configuration...
    Importing built-in database...
    Importing emqx_enhanced_authn_scram_mnesia database table...
    Importing emqx_authn_mnesia database table...
    Importing emqx_admin database table...
    Importing emqx_acl database table...
    Importing emqx_banned database table...
    Importing emqx_psk database table...
    Importing emqx_app database table...
    Data has been imported successfully.

    # `<EMQX data directory>/backup`ディレクトリからファイルをインポート
    $ cp /tmp/emqx-export-2023-06-21-13-28-06.418.tar.gz /opt/emqx/data/backup/
    $ ./emqx ctl data import emqx-export-2023-06-21-13-28-06.418.tar.gz
    Importing data from "data/backup/emqx-export-2023-06-21-13-28-06.418.tar.gz"...
    Importing cluster configuration...
    Importing built-in database...
    Importing emqx_enhanced_authn_scram_mnesia database table...
    Importing emqx_authn_mnesia database table...
    Importing emqx_admin database table...
    Importing emqx_acl database table...
    Importing emqx_banned database table...
    Importing emqx_psk database table...
    Importing emqx_app database table...
    Data has been imported successfully.
    ```
