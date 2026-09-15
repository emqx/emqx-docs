# プラグイン管理

このページでは、EMQXにおけるプラグインのライフサイクルについて説明し、Dashboard、CLI、REST APIを使ったプラグインのインストール、設定、起動、停止、アンインストール、アップグレード方法を解説します。

## プラグインのライフサイクル

EMQXのプラグインは主に以下の3つのライフサイクル状態を経ます：

- **Installed（インストール済み）**：プラグインのコードと設定が読み込まれているが、アプリケーションはまだ起動していない状態。
- **Started（起動済み）**：プラグインが実行中で、EMQXと積極的に連携している状態。
- **Uninstalled（アンインストール済み）**：プラグインがシステムから完全に削除された状態。

### インストールプロセス

インストールの流れは以下の通りです：

1. プラグインパッケージ（`make rel`コマンドで作成されたtarball）をDashboard、API、またはCLI経由でアップロードします。詳細なインストール手順は[Install and Manage Plugins](#install-and-manage-plugins)を参照してください。
2. プラグインパッケージはEMQXクラスターの各ノードに転送されます。
3. 各ノード上で以下が行われます：
   - tarballはEMQXルートディレクトリの`plugins`サブディレクトリに保存されます（`plugins.install_dir`オプションで上書き可能）：`$EMQX_ROOT/plugins/my_emqx_plugin-1.0.0.tar.gz`
   - 同じディレクトリに展開されます：`$EMQX_ROOT/plugins/my_emqx_plugin-1.0.0/`
   - 初期設定（メインプラグインのappからの`config.hocon`）が`$EMQX_DATA_DIR/plugins/my_emqx_plugin/config.hocon`にコピーされます。
   - Avroスキーマが存在する場合は検証のためにロードされます。
   - プラグインコードはノードにロードされますが、アプリケーションは起動されません。
   - プラグインはEMQX設定の`plugins.states`に`disabled`として登録されます。

::: tip

プラグインの状態はEMQX設定内の`enable`フラグ（`true`または`false`）のみが保存されます。プラグインの詳細設定は各ノードの`$EMQX_DATA_DIR/plugins/my_emqx_plugin/config.hocon`ファイルに格納されます。

:::

### 設定

インストール後、プラグイン設定はDashboardやAPIを通じて更新可能です：

- 新しい設定はAvroスキーマ（存在する場合）に基づき検証されます。
- 更新内容はクラスター内の全ノードに配布されます。
- プラグインの`on_config_changed/2`コールバック関数が呼び出されます。プラグインが新設定を受け入れた場合、`$EMQX_DATA_DIR/plugins/my_emqx_plugin/config.hocon`に永続化されます。

::: tip

`on_config_changed/2`コールバック関数はアプリケーションが起動していなくても呼び出されます。

:::

::: tip

`on_config_changed/2`コールバック関数はEMQXクラスターの各ノードで呼び出されます。ローカルシステムの状態（例：ネットワークの可用性）に依存した設定検証は避けてください。ノード間で結果が不整合になる可能性があります。代わりにランタイムチェックは`on_health_check/1`を使用し、リソースが利用不可の場合は不健康状態を報告してください。

:::

### 設定ファイルの場所

関連する設定ファイルは以下の2箇所にあります：

- インストール済みプラグインパッケージ内のデフォルトファイル：
  - Docker環境：
    `/opt/emqx/plugins/my_emqx_plugin-1.0.0/my_emqx_plugin-1.0.0/priv/config.hocon`
  - deb/rpm環境：
    `/usr/lib/emqx/plugins/my_emqx_plugin-1.0.0/my_emqx_plugin-1.0.0/priv/config.hocon`

- DashboardやAPI経由で設定保存後にEMQXが管理する永続化されたプラグイン設定ファイル：
  - Docker環境：
    `/opt/emqx/data/plugins/my_emqx_plugin/config.hocon`
  - deb/rpm環境：
    `/var/lib/emqx/plugins/my_emqx_plugin/config.hocon`

`priv/config.hocon`はパッケージに含まれるデフォルトテンプレートであり、`data/plugins/.../config.hocon`が設定変更後にEMQXが使用する永続化設定ファイルの場所です。

### 起動

プラグインはDashboard、API、CLIから手動で起動します。起動時には：

- プラグインのアプリケーションが起動されます。
- プラグインはEMQX設定の`plugins.states`に`enabled`として登録されます。

プラグインが起動している状態で情報が要求されると、`on_health_check/1`コールバック関数が呼び出され、プラグインの状態が取得されます。

### 停止

プラグイン停止時には：

- プラグインのアプリケーションが停止されます。
- プラグインはEMQX設定の`plugins.states`に`disabled`として登録されます。

プラグインのアプリケーションは停止しますが、コードはノードにロードされたままです。停止中のプラグインも設定変更が可能です。

### アンインストールプロセス

アンインストールの流れは以下の通りです：

1. プラグインが起動中であれば停止します。
2. プラグインのコードをノードからアンロードします。
3. ノード上のパッケージファイルを削除します（設定ファイルは保持されます）。
4. EMQX設定の`plugins.states`からプラグインを登録解除します。

プラグインのアンインストールはDashboardまたはCLIから実行可能です。詳細は[Install and Manage Plugins](#install-and-manage-plugins)を参照してください。

### クラスター参加時の挙動

EMQXノードがクラスターに参加する際、プラグインおよび設定はノードのローカルファイルシステムに存在するため、必ずしもインストール・設定済みとは限りません。

新規ノードは以下を行います：

- クラスター参加時にグローバルなEMQX設定を取得します。
- 設定からプラグインの状態（インストール済みかつ有効か）を把握します。
- 他ノードからプラグインおよび実際の設定を取得します。
- プラグインをインストールし、有効なものは起動します。

## プラグインのインストールと管理

EMQXはDashboard、CLI、APIを通じてプラグインパッケージのインストール、アンインストール、管理をサポートしています。

### Dashboard経由でのパッケージインストール

プラグインがビルド済みで、`my_emqx_plugin-1.0.0.tar.gz`が用意されているとします。Dashboardから直接コンパイル済みプラグインパッケージをインストールする手順は以下の通りです：

::: tip 重要なセキュリティアップデート

セキュリティ上の理由から、EMQXはDashboard経由のプラグインインストールに対して明示的な許可を必要とします。

- インストール前に許可を付与する必要があります。
- 許可状態は一時的で、インストール完了後に自動的に取り消されます。
- クラスター環境の場合、全ノードで許可を付与する必要があります。

:::

1. CLIで以下のコマンドを実行し、プラグインインストールを明示的に許可します：

   ```bash
   emqx ctl plugins allow $NAME-$VSN
   ```

   - `{NAME}`：プラグイン名（例：`my_emqx_plugin`）
   - `{VSN}`：プラグインのバージョン（例：`1.0.0`）

   このコマンド実行後、Dashboardからインストールを進められます。

2. EMQX Dashboardの **Management** -> **Plugins** に移動します。

3. **+ Install plugin** ボタンをクリックしてインストールページを開きます。

4. プラグインパッケージを選択またはドラッグしてDashboardにアップロードします。

   ![plugin-list-empty](./assets/plugin-install.png)

5. **Install** ボタンをクリックしてインストールを完了します。プラグイン一覧に新しいプラグインが表示されます。

   ![plugin-list-installed](./assets/plugin-list-installed.png)

インストール後はプラグインの起動・停止や設定が可能です。Dashboardでプラグインをアンインストールするには、プラグイン一覧の**Actions**列の**More**メニューから**Uninstall**を選択します。

以前に許可したプラグインの許可を取り消すには、以下のいずれかを実行してください：

1. プラグインをアンインストールする（既にインストール済みの場合）。
2. 以下のコマンドで明示的に許可を取り消す：

```bash
emqx ctl plugins disallow $NAME-$VSN
```

### CLI経由でのパッケージインストール

プラグインがビルド済みで、`my_emqx_plugin-1.0.0.tar.gz`が用意されているとします。CLIから直接コンパイル済みプラグインパッケージをインストールする手順は以下の通りです：

1. EMQXノード上でtarballをEMQXのプラグインディレクトリにコピーします：

   ```
   $ cp my_emqx_plugin-1.0.0.tar.gz $EMQX_HOME/plugins
   ```

2. プラグインをインストールします：

   ```
   $ emqx ctl plugins install my_emqx_plugin-1.0.0
   ```

   クラスター内の全稼働ノードにインストールする場合は`--cluster`オプションを付けます：

   ```
   $ emqx ctl plugins install my_emqx_plugin-1.0.0 --cluster
   ```

3. プラグイン一覧を確認します：

   ```
   $ emqx ctl plugins list
   ```

4. プラグインの起動・停止：

   ```
   $ emqx ctl plugins start my_emqx_plugin-1.0.0
   $ emqx ctl plugins stop my_emqx_plugin-1.0.0
   ```

5. プラグインのアンインストール：

   ```
   $ emqx ctl plugins uninstall my_emqx_plugin-1.0.0
   ```

### API経由でのパッケージインストール

プラグインがビルド済みで、`my_emqx_plugin-1.0.0.tar.gz`が用意されているとします。APIを使ってプラグインをインストールする手順は以下の通りです：

1. インストールを許可します。以下のコマンドを実行してプラグインのインストールを有効化します：

   ```
   emqx ctl plugins allow my_emqx_plugin-1.0.0
   ```

2. `curl`を使ってPOSTリクエストを送り、プラグインをインストールします：

   ```
   $ curl -u $KEY:$SECRET -X POST http://$EMQX_HOST:18083/api/v5/plugins/install -H "Content-Type: multipart/form-data" -F "plugin=@my_emqx_plugin-1.0.0.tar.gz"
   ```

3. プラグイン一覧を確認してインストール成功を検証します：

   ```
   $ curl -u $KEY:$SECRET http://$EMQX_HOST:18083/api/v5/plugins | jq
   ```

4. プラグインの起動・停止：

   ```
   $ curl -s -u $KEY:$SECRET -X PUT "http://$EMQX_HOST:18083/api/v5/plugins/my_emqx_plugin-1.0.0/start"
   $ curl -s -u $KEY:$SECRET -X PUT "http://$EMQX_HOST:18083/api/v5/plugins/my_emqx_plugin-1.0.0/stop"
   ```

### EMQX起動前にプラグインを事前インストールする

EMQX起動時にプラグインを即座に利用可能にしたい場合（例：カスタムDockerイメージのビルド時）、プラグインパッケージを展開し、事前にEMQX設定を行うことで対応可能です。

以下はDockerfileの例ですが、deb/rpmやベアメタル環境でも同様の手順が適用されます：

1. プラグインtarballをpluginsディレクトリにコピーし展開します：

   ```dockerfile
   COPY --chown=emqx:emqx my_emqx_plugin-1.0.0.tar.gz /opt/emqx/plugins/my_emqx_plugin-1.0.0.tar.gz

   RUN cd /opt/emqx/plugins && \
       mkdir -p my_emqx_plugin-1.0.0 && \
       tar zxf my_emqx_plugin-1.0.0.tar.gz -C my_emqx_plugin-1.0.0
   ```

2. EMQXのベース設定ファイルにプラグイン起動設定を追記し、起動時に自動でプラグインが開始されるようにします：

   ```dockerfile
   RUN cat <<EOF >> /opt/emqx/etc/base.hocon
   plugins {
       states = [
           {
               name_vsn = "my_emqx_plugin-1.0.0"
               enable = true
           }
       ]
   }
   EOF
   ```

   `enable = true`で自動起動、`enable = false`でインストールのみ（起動しない）設定となります。

## プラグインのアップグレード

EMQXでは同一プラグインの複数バージョンを同時にインストールできません。

新しいバージョンをインストールするには：

- まず古いバージョンをアンインストールします。
- その後、新しいバージョンをインストールします。

プラグイン設定はインストール間で保持されます。

<!-- **注**：（EMQXエンタープライズ）プラグインはホットアップグレード後に再インストールが必要です。 -->
