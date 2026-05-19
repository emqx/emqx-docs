# プラグインの管理

このページでは、EMQXにおけるプラグインのライフサイクルについて説明し、Dashboard、CLI、REST APIを使ったプラグインのインストール、設定、起動、停止、アンインストール、アップグレード方法を解説します。

## プラグインのライフサイクル

EMQXのプラグインは主に以下の3つのライフサイクル状態を経ます：

- **インストール済み**：プラグインのコードと設定が読み込まれているが、アプリケーションはまだ起動していない状態。
- **起動済み**：プラグインが実行中で、EMQXと積極的に連携している状態。
- **アンインストール済み**：プラグインがシステムから完全に削除された状態。

### インストール手順

インストールの流れは以下の通りです：

1. プラグインパッケージ（`make rel`コマンドで作成されたtarball）をDashboard、API、またはCLIからアップロードします。詳細なインストール手順は[プラグインのインストールと管理](#install-and-manage-plugins)を参照してください。
2. プラグインパッケージはEMQXクラスターの各ノードに転送されます。
3. 各ノード上で以下が行われます：
   - tarballがEMQXルートディレクトリの`plugins`サブディレクトリに保存されます（`plugins.install_dir`オプションで上書き可能）：`$EMQX_ROOT/plugins/my_emqx_plugin-1.0.0.tar.gz`
   - 同じディレクトリに展開されます：`$EMQX_ROOT/plugins/my_emqx_plugin-1.0.0/`
   - 初期設定（メインプラグインのappからの`config.hocon`）が`$EMQX_DATA_DIR/plugins/my_emqx_plugin/config.hocon`にコピーされます。
   - Avroスキーマが存在すればロードされ、検証に使用されます。
   - プラグインコードがノードにロードされますが、アプリケーションは起動しません。
   - プラグインはEMQX設定の`plugins.states`に`disabled`として登録されます。

::: tip

プラグインの状態はEMQX設定内で`enable`フラグ（`true`または`false`）のみが保存されます。プラグインの完全な設定は各ノードの`$EMQX_DATA_DIR/plugins/my_emqx_plugin/config.hocon`に保持されます。

:::

### 設定

インストール後、プラグインの設定はDashboardまたはAPIから更新可能です：

- 新しい設定はAvroスキーマ（存在する場合）に対して検証されます。
- 更新内容はクラスター内の全ノードに配布されます。
- プラグインの`on_config_changed/2`コールバック関数が呼び出されます。プラグインが新設定を受け入れた場合、`$EMQX_DATA_DIR/plugins/my_emqx_plugin/config.hocon`に永続化されます。

::: tip

`on_config_changed/2`コールバック関数はアプリケーションが起動していなくても呼び出されます。

:::

::: tip

`on_config_changed/2`コールバック関数はEMQXクラスターの各ノードで呼び出されます。ローカルシステムの状態（例：ネットワークの可用性）に依存した設定検証は避けてください。ノード間で結果が不整合になる可能性があります。代わりに`on_health_check/1`を使ってランタイムチェックを行い、リソースが利用できない場合は不健康状態を報告してください。

:::

### 設定ファイルの場所

関連する設定ファイルの場所は2つあります：

- インストールされたプラグインパッケージ内のバンドル済みデフォルトファイル：
  - Docker環境：
    `/opt/emqx/plugins/my_emqx_plugin-1.0.0/my_emqx_plugin-1.0.0/priv/config.hocon`
  - deb/rpm環境：
    `/usr/lib/emqx/plugins/my_emqx_plugin-1.0.0/my_emqx_plugin-1.0.0/priv/config.hocon`

- DashboardやAPI経由で設定が保存された後にEMQXが管理する永続化されたプラグイン設定ファイル：
  - Docker環境：
    `/opt/emqx/data/plugins/my_emqx_plugin/config.hocon`
  - deb/rpm環境：
    `/var/lib/emqx/plugins/my_emqx_plugin/config.hocon`

`priv/config.hocon`はパッケージ化されたデフォルトテンプレートであり、`data/plugins/.../config.hocon`は設定変更後にEMQXが使用する永続化されたプラグイン設定ファイルです。

### 起動

プラグインはDashboard、API、CLIから手動で起動します。起動時には：

- プラグインのアプリケーションが起動されます。
- プラグインはEMQX設定の`plugins.states`に`enabled`として登録されます。

プラグインが起動中で情報が要求された場合、`on_health_check/1`コールバック関数が呼び出され、プラグインの状態が取得されます。

### 停止

プラグインを停止すると：

- プラグインのアプリケーションが停止されます。
- プラグインはEMQX設定の`plugins.states`に`disabled`として登録されます。

プラグインのアプリケーションは停止されますが、コードはノード上にロードされたままです。停止中のプラグインも設定変更は可能です。

### アンインストール手順

アンインストールの流れは以下の通りです：

1. プラグインが起動中であれば停止します。
2. プラグインのコードをノードからアンロードします。
3. パッケージファイルをノードから削除します（設定ファイルは保持されます）。
4. プラグインはEMQX設定の`plugins.states`から登録解除されます。

プラグインパッケージはDashboardまたはCLIからアンインストール可能です。詳細は[プラグインのインストールと管理](#install-and-manage-plugins)を参照してください。

### クラスター参加時の挙動

EMQXノードがクラスターに参加する際、プラグインや設定は各ノードのローカルファイルシステムに存在するため、新規ノードにはプラグインがインストール・設定されていない場合があります。

新規ノードは以下の処理を行います：

- クラスター参加時にグローバルなEMQX設定を取得します。
- EMQX設定からプラグインの状態（どのプラグインがインストールされていて、どれが有効か）を把握します。
- 他のノードからプラグインと実際の設定を要求します。
- プラグインをインストールし、有効なものを起動します。

## プラグインのインストールと管理

EMQXはDashboard、CLI、APIを通じてプラグインパッケージのインストール、アンインストール、管理をサポートしています。

### Dashboardからのパッケージインストール

プラグインが既にビルドされ、`my_emqx_plugin-1.0.0.tar.gz`というtarballが用意されているとします。Dashboardから直接コンパイル済みプラグインパッケージをインストールするには、以下の手順に従ってください：

::: tip 重要なセキュリティアップデート

セキュリティ上の理由から、EMQXはDashboard経由のプラグインインストールに対して明示的な許可を必要とするようになりました。

- インストール開始前に許可を与える必要があります。
- 許可状態は一時的で、インストール完了後に自動的に取り消されます。
- クラスター環境では、すべてのノードで許可を与える必要があります。

:::

1. CLIで以下のコマンドを実行し、プラグインインストールを明示的に許可します：

   ```bash
   emqx ctl plugins allow $NAME-$VSN
   ```

   - `{NAME}`：プラグイン名（例：`my_emqx_plugin`）
   - `{VSN}`：プラグインのバージョン（例：`1.0.0`）

   このコマンド実行後、Dashboardからインストールを進められます。

2. EMQX Dashboardの **Management** -> **Plugins** に移動します。

3. **+ Install plugin** ボタンをクリックし、インストールページを開きます。

4. プラグインパッケージを選択またはドラッグしてDashboardにアップロードします。

   ![plugin-list-empty](./assets/plugin-install.png)

5. **Install** ボタンをクリックしてインストールを完了します。インストール済みプラグインの一覧が表示されます。

   ![plugin-list-installed](./assets/plugin-list-installed.png)

これでプラグインの起動・停止や設定が可能になります。Dashboardからプラグインをアンインストールするには、プラグイン一覧ページの**Actions**列の**More**メニューから**Uninstall**ボタンをクリックしてください。

以前に許可したプラグインの許可を取り消すには、以下のいずれかを実行します：

1. プラグインをアンインストールする（既にインストール済みの場合）。
2. または、以下のコマンドで明示的に許可を取り消す：

```bash
emqx ctl plugins disallow $NAME-$VSN
```

### CLIからのパッケージインストール

プラグインが既にビルドされ、`my_emqx_plugin-1.0.0.tar.gz`が用意されているとします。CLIから直接コンパイル済みプラグインパッケージをインストールするには、以下の手順に従ってください：

1. EMQXノード上でtarballをEMQXのプラグインディレクトリにコピーします：

   ```
   $ cp my_emqx_plugin-1.0.0.tar.gz $EMQX_HOME/plugins
   ```

2. プラグインをインストールします：

   ```
   $ emqx ctl plugins install my_emqx_plugin-1.0.0
   ```

3. プラグイン一覧を確認します：

   ```
   $ emqx ctl plugins list
   ```

4. プラグインの起動・停止を行います：

   ```
   $ emqx ctl plugins start my_emqx_plugin-1.0.0
   $ emqx ctl plugins stop my_emqx_plugin-1.0.0
   ```

5. プラグインをアンインストールします：

   ```
   $ emqx ctl plugins uninstall my_emqx_plugin-1.0.0
   ```

### APIからのパッケージインストール

プラグインが既にビルドされ、`my_emqx_plugin-1.0.0.tar.gz`が用意されているとします。APIを使ってプラグインをインストールするには、以下の手順に従ってください：

1. インストールを許可します。以下のコマンドでプラグインインストールを許可してください：

   ```
   emqx ctl plugins allow my_emqx_plugin-1.0.0
   ```

2. `curl`を使ってAPIにPOSTリクエストを送り、プラグインをインストールします：

   ```
   $ curl -u $KEY:$SECRET -X POST http://$EMQX_HOST:18083/api/v5/plugins/install -H "Content-Type: multipart/form-data" -F "plugin=@my_emqx_plugin-1.0.0.tar.gz"
   ```

3. プラグイン一覧を確認します。インストールが成功したか確認するには以下を実行します：

   ```
   $ curl -u $KEY:$SECRET http://$EMQX_HOST:18083/api/v5/plugins | jq
   ```

4. プラグインの起動・停止を行います。以下のコマンドを使用してください：

   ```
   $ curl -s -u $KEY:$SECRET -X PUT "http://$EMQX_HOST:18083/api/v5/plugins/my_emqx_plugin-1.0.0/start"
   $ curl -s -u $KEY:$SECRET -X PUT "http://$EMQX_HOST:18083/api/v5/plugins/my_emqx_plugin-1.0.0/stop"
   ```

### EMQX起動前にプラグインを事前インストールする

EMQX起動時にプラグインを即座に利用可能にしたい場合（例：カスタムDockerイメージの構築時など）、プラグインパッケージを展開し、事前にEMQXを設定しておくことが可能です。

以下はDockerfileを例にした手順ですが、deb/rpmやベアメタルなど他のデプロイ方法でも同様の手順が適用できます：

1. プラグインtarballをpluginsディレクトリにコピーし展開します：

   ```dockerfile
   COPY --chown=emqx:emqx my_emqx_plugin-1.0.0.tar.gz /opt/emqx/plugins/my_emqx_plugin-1.0.0.tar.gz

   RUN cd /opt/emqx/plugins && \
       mkdir -p my_emqx_plugin-1.0.0 && \
       tar zxf my_emqx_plugin-1.0.0.tar.gz -C my_emqx_plugin-1.0.0
   ```

2. EMQXの基本設定ファイルにプラグイン登録を追記し、起動時に自動的にプラグインが起動されるようにします：

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

   `enable = true`に設定すると自動起動され、`enable = false`にするとインストールのみで起動はされません。

## プラグインのアップグレード

EMQXでは同じプラグインの複数バージョンを同時にインストールすることはできません。

新しいバージョンのプラグインをインストールするには：

- まず古いバージョンをアンインストールします。
- その後、新しいバージョンをインストールします。

プラグインの設定はインストール間で保持されます。

<!-- **注意**：（EMQXエンタープライズ版）プラグインはホットアップグレード後に再インストールが必要です。 -->
