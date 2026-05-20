# バックアップとリストア

EMQXは分散ストレージスキーマを採用しており、システムの高可用性を確保するためにクラスター転送機能も導入しています。

本ページでは、システム障害時のデータ損失を防ぐために、運用データおよび設定ファイルのバックアップ方法について説明します。

## 機能説明

EMQXはバックアップとリカバリーを実現するために、データのインポートおよびエクスポート用のCLIコマンドを提供しています。EMQX 4.xのコマンドと似ていますが、エクスポートファイルのフォーマットは4.xとは互換性がありません。

- EMQX 4.xでは、EMQXの設定および組み込みデータベースの必要なデータをすべて1つのJSONファイルに保存していました。
- EMQX 5.xでは、エクスポートされたデータはtarファイル形式で圧縮されており、大量のユーザーデータをより効率的かつ構造的に扱うことが可能です。

CLIコマンドに加えて、EMQX Enterpriseではダッシュボード上にデータバックアップおよびリカバリーページがあり、そこでデータのインポートおよびエクスポート操作を行うことができます。

EMQXがインポートおよびエクスポートに対応しているデータは以下の通りです：

- EMQXの[設定リライトファイル](../configuration/configuration.md#configuration-rewrite-file)の内容：
  - 認証および認可設定
  - ルール、コネクター、Sink/Source
  - リスナー、ゲートウェイ設定
  - その他のEMQX設定
- 組み込みデータベース（Mnesia）データ
  - ダッシュボードユーザーおよびREST APIキー
  - クライアント認証情報（組み込みデータベースのパスワード認証、強化認証）
  - PSK認証データ
  - 認可ルール
  - ブラックリストデータ
  - リテインドメッセージ
- EMQXデータディレクトリ（`node.data_dir`）に保存されているSSL/TLS証明書
- EMQXデータディレクトリに保存されている認可用の`acl.conf`ファイル

::: tip 特記事項

1. エクスポートファイルにはEMQXデータディレクトリに保存されているSSL/TLS証明書および`acl.conf`ファイルのみが含まれます。データディレクトリ外にある証明書や`acl.conf`ファイルは、データインポート前に手動で適切な場所にコピーしておく必要があります。これにより完全性と正確性が保証されます。
2. エクスポートファイルの名前形式は`emqx-export-YYYY-MM-DD-HH-mm-ss.sss.tar.gz`で、エクスポート先ディレクトリは`<EMQX data directory>/backup`です。
3. EMQX v5.7.1以降では、リテインドメッセージの保存方法がram（メモリ）に設定されていてもバックアップされます。

:::

### エクスポート

データは稼働中の任意のクラスターのノードからエクスポート可能です。

### インポート

データをインポートするにはEMQXノードが稼働中である必要があり、インポート操作が成功するためには以下の条件を満たす必要があります：

- [コアノード＋レプリカノード](../deploy/cluster/mria-introduction.md)モードが有効な場合、データインポートはコアノードでのみ実行可能です。実際のインポート動作に影響はなく、データはコアノードおよびレプリカノードを含むすべてのクラスターのノードにレプリケートされます。コアノードで操作することで正しいデータインポートが保証されます。
- データファイルの名前を変更してはいけません。

上記の条件を満たさない場合、インポート処理は中止され、対応するエラーメッセージが表示されます。

データインポート時には、対象のEMQXクラスターに存在しないデータは挿入され、競合がある場合は更新されます。インポート処理によって既存のEMQXクラスターのデータが削除されることはありません。

::: tip 特記事項

稀に既存データとインポートデータの互換性がない場合があります。例えば、EMQXクラスターが組み込みデータベース認証を使用し、ソルトの位置を「サフィックス（suffix）」に設定している一方で、インポートデータでは同じ設定が「プレフィックス（prefix）」に設定されている場合です。インポート後は新しい設定が有効となり、以前作成された古いユーザー認証情報は使用できなくなります。

したがって、データをクリアせずにEMQXクラスターにデータをインポートする場合は、特に注意が必要です。

:::

## ダッシュボードの例

本節では、ダッシュボード上でのデータインポートおよびエクスポート操作の方法を説明します。

:::tip

- ダッシュボードによるバックアップおよびリカバリー機能はEMQX Enterpriseエディション v5.4.0以降で利用可能です。
- CLIでエクスポートしたバックアップファイルもダッシュボードのバックアップ＆リカバリーページで管理できます。

:::

1. ダッシュボードにログインし、**System** -> **Backup & Restore** ページに移動します。

2. データをエクスポートするには、右上の**Create**ボタンをクリックして、現在のEMQXクラスターのデータに基づくバックアップファイルを作成します。バックアップファイル一覧ページでファイル情報を確認できます：

   - **File Name**：バックアップファイルの名前
   - **Node Name**：バックアップファイルが保存されているノード名（このノードのデータのみを含むわけではありません）
   - **Created At**：バックアップファイルの作成日時
   - **File Size**：バックアップファイルのサイズ

   **Actions**列の**Download**をクリックするとファイルをダウンロードしてローカルに保存できます。

3. データをインポートするには、右上の**Upload**ボタンをクリックしてバックアップファイルを現在のEMQXクラスターにアップロードします。アップロード直後には復元は行われません。バックアップファイル一覧の**Actions**列にある**Restore**をクリックして、バックアップファイルを現在のEMQXクラスターにインポートします。

![EMQX バックアップ＆リストア](./assets/backup-restore.png)

## CLIの例

本節では、コマンドラインインターフェースを使用したデータのインポートおよびエクスポート方法を示します。

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
2. データをインポートします。インポートするファイル名は絶対パスまたは相対パスで指定可能です。ファイルが`<EMQX data directory>/backup`ディレクトリ内にある場合は、パスなしのベース名でも指定できます。例：

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
