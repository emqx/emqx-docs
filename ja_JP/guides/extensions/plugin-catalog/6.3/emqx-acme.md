# EMQX ACMEプラグイン

EMQX ACMEプラグインは、Let's EncryptなどのACME対応証明書機関と連携し、EMQXのSSLリスナー向けにTLS証明書の自動発行および更新を行います。本ページではEMQX 6.1でのプラグインの設定および使用方法について説明します。発行された証明書はEMQX管理の証明書バンドルに保存されます。

::: warning 重要なお知らせ
`<data_dir>/certs2/` はEMQXの再デプロイ時に永続化してください。プラグインは以下のファイルを `<data_dir>/certs2/global/<cert_bundle_name>/` 配下に保存します：

- `chain.pem` と `key.pem`：発行された証明書バンドルです。これらのファイルを紛失すると、プラグインは次回起動時に新しい証明書を発行します。新しい証明書は、Let's Encryptのドメインごとの週あたり5件の重複証明書発行制限にカウントされます。
- `acc-key.pem`：Let's Encryptに登録されたアカウントを識別するACMEアカウントキーです。このファイルを紛失すると、再デプロイごとに新しいアカウントが作成されます。これにより、3時間ごとにIPアドレスあたり10件の新規アカウント作成制限を消費し、以前のアカウントに関連付けられた証明書の失効ができなくなる可能性があります。

Docker環境では `<data_dir>` は `/opt/emqx/data` です。DEB/RPMインストールでは `/var/lib/emqx` です。Dockerでは `data/` ディレクトリ全体、または少なくとも `data/certs2/` をホストボリュームにバインドマウントしてください。Kubernetesでは永続ボリュームクレーム（PVC）を使用してください。初回発行時にプラグインはバンドル内にアカウントキーを生成し、`emqx_managed_certs` を通じてクラスターのすべてのノードに複製します。
:::

## 前提条件

- ドメインはEMQXノードのパブリックIPアドレスに解決されている必要があります。
- HTTP-01チャレンジ検証のためにパブリックポート80がインターネットから到達可能である必要があります。`challenge_port` が80以外の場合は、パブリックポート80から設定した `challenge_port` への転送を行ってください。
- ステージングテストにはLet's EncryptのステージングURL `https://acme-staging-v02.api.letsencrypt.org/directory` を使用してください。

## クイックスタート

パブリックに解決可能なドメインを持つ単一のEMQXノードでプラグインを設定する手順：

1. EMQXダッシュボードで **Management** -> **Plugins** をクリックし、プラグインをインストールして有効化します。
2. 以下の項目を設定します。他の項目はデフォルトのままで構いません：
   - `domains = "mqtt.example.com"`：カンマ区切りのドメインリストを入力します。各ドメインはこのノードにパブリックに解決されている必要があります。
   - `contact = "mailto:admin@example.com"`：証明書機関（CA）からの更新・失効通知用の連絡先アドレスをカンマ区切りで入力します。
   - `challenge_port = 5080`：EMQXがバインド可能な高位ポートを指定します。パブリックポート80へのトラフィックをこのポートに転送するリバースプロキシや `iptables` のリダイレクトを設定してください。[ポート80アクセスの設定](#configure-port-80-access)を参照してください。
   - `dir_url`：デフォルトのLet's Encrypt本番URLを使用するか、設定検証中はステージングURLを使用してください。
3. プラグインUIで **Issue / Renew Now** をクリックします。初回発行時（バンドルが空の場合）、プラグインは以下の処理を行います：
   - 管理証明書バンドル内にACMEアカウントキーが存在しなければ生成します。
   - HTTP-01チャレンジを通じて証明書を発行します。
   - `listener_ids`（デフォルトは `ssl:default,wss:default`）で指定された各リスナーの設定を新しいバンドルを使用するよう書き換えます。
   - `enable_dashboard_https` がデフォルトで `true` のため、同じ証明書を用いてポート `18084` にダッシュボードのHTTPSリスナーを作成します。

   以降の実行ではバンドルファイルのみを更新し、リスナー設定やダッシュボードHTTPS設定は変更しません。Erlang SSL PEMキャッシュはリスナーの再起動なしに新しい証明書を読み込みます。
4. `https://your.domain:18084/` を開いてダッシュボードにログインし、プラグインUIで **Disable Dashboard HTTP Listener** をクリックします。このボタンはプラグインページがHTTPSで開かれている場合のみ表示されます。操作成功後、ポート `18083` の平文リスナーはクラスター全体で無効化されます。この設定は本番環境で推奨されます。HTTPリスナーを有効にしたままだとダッシュボードに平文アクセスが可能になるためです。

プラグインは `check_interval_hours` で指定された間隔で証明書をチェックし、必要に応じて自動更新します。

## 動作概要

1. プラグインは設定されたCAにACMEアカウントを登録するか、既存のアカウントを再利用します。
2. 発行中のHTTP-01チャレンジに応答するため、一時的なHTTPリスナーを起動します。
3. 発行された証明書チェーンと秘密鍵は管理証明書バンドルに保存されます。デフォルトではACMEアカウントキーもこのバンドルに保存します。`acc_key` が設定されている場合は、オペレーター管理のファイルをそのパスから使用します。詳細は[ACMEアカウントキー](#acme-account-key)を参照してください。
4. SSLリスナーは `ssl_options.managed_certs.bundle_name` を通じてバンドルを参照します。初回発行時にプラグインは `listener_ids` で指定されたリスナーのこの設定を書き換えられます。
5. プラグインは `check_interval_hours` の間隔で証明書をチェックし、`renew_before_expiry_days` で指定された期限内に証明書が切れる場合は更新します。更新はバンドルファイルを上書きし、Erlang SSL PEMキャッシュがリスナーの再起動なしに新証明書を読み込みます。

## 設定例

プラグインは `config_schema.avsc` からフィールド説明をダッシュボードの設定フォームにレンダリングします。以下のHOCON例は典型的なプラグイン設定例です。ダッシュボードでフィールドラベルにカーソルを合わせると説明が表示されます。

```hocon
dir_url = "https://acme-v02.api.letsencrypt.org/directory"
# 証明書のSANドメインのカンマ区切りリスト
domains = "mqtt.example.com,mqtt2.example.com"
# CA連絡先アドレス（更新・失効通知）のカンマ区切りリスト
contact = "mailto:admin@example.com,mailto:ops@example.com"
cert_bundle_name = "acme"
# 移行対象のリスナーIDのカンマ区切りリスト（各要素は "ssl:<name>" または "wss:<name>"）
listener_ids = "ssl:default,wss:default"
cert_type = "ec"
# EMQXがバインド可能な高位ポート。80番からのリバースプロキシまたはiptablesリダイレクトを設定する。
challenge_port = 5080
renew_before_expiry_days = 30
check_interval_hours = 24
enable_dashboard_https = true
dashboard_https_port = 18084
# acc_keyは未設定。プラグインが証明書バンドル内で管理する。
```

次にSSLリスナーをバンドル使用に設定します。`listener_ids` で指定されたリスナーは初回発行時にプラグインがこの設定を書き換えます。

```hocon
listeners.ssl.default {
  bind = "0.0.0.0:8883"
  ssl_options {
    managed_certs {
      bundle_name = "acme"
    }
  }
}
```

## ACMEアカウントキー

RFC 8555では、ACMEアカウントの秘密鍵がアカウントを識別します。クライアントはローカルで鍵を生成し、その鍵で署名した `newAccount` リクエストを送信します。CAはその後アカウントを作成します。鍵はポータルなどで別途登録されるものではありません。

**デフォルト動作：** `acc_key` を未設定のままにします。初回発行時にプラグインはメモリ上でEC P-256鍵（`cert_type = "rsa"` の場合はRSA-2048鍵）を生成します。その後、`emqx_managed_certs:add_managed_files/3` を使って `<data_dir>/certs2/global/<cert_bundle_name>/acc-key.pem` に鍵を書き込み、クラスターの全ノードに配布します。以降の発行では同じファイルを再利用します。アカウントキーと証明書チェーンを保持するために、データディレクトリをバインドマウントやPVCで永続化してください。本ページ冒頭の永続化警告を参照してください。

**オペレーターによる上書き：** キーをバンドル外のパスで管理したい場合（例：Kubernetes Secretを既知の場所にマウントしたものや他ソフトウェアと共有する鍵）は、`acc_key` にPEMファイルの `file://` URIを設定してください。プラグインは発行時にファイルを読み込み、上書きしません。ローカルノードにファイルが存在しない場合は、そのノードで新規生成します。このファイルはクラスター間で複製されないため、各ノードに配布する必要があります。PEMファイルが暗号化されている場合は、`acc_key_password` にパスワードの平文ファイルを指す `file://` URIを設定してください。`${EMQX_ETC_DIR}` や `${VAR}` は展開され、DockerやDEB/RPMインストールで同じ設定が動作します。

## ポート80アクセスの設定

ACME証明書機関は常に検証対象ドメインのポート80に対してHTTP-01チャレンジを実施します。この動作はRFC 8555で定義されており、CA側で変更できません。EMQXは非rootユーザー `emqx` で動作し、通常1024未満のポートにバインドできません。そのため、`challenge_port = 80` は通常 `eacces` エラーで失敗します。

`challenge_port` をEMQXがバインド可能な高位ポート（例：5080）に設定し、以下のいずれかの方法でパブリックポート80から設定した `challenge_port` へトラフィックをルーティングしてください：

- **リバースプロキシ：** 同一ホストでrootまたは `CAP_NET_BIND_SERVICE` 権限を持つNGINX、Caddy、HAProxyを実行し、`http://domain/.well-known/acme-challenge/*` へのリクエストを `http://127.0.0.1:<challenge_port>` にプロキシします。他のパスは `404` を返して構いません。
- **ポートフォワーディング：** Linuxでは `iptables` を使い、ポート80の着信トラフィックを高位ポートにリダイレクトします：

  ```bash
  iptables -t nat -A PREROUTING -p tcp --dport 80 \
                  -j REDIRECT --to-port 5080
  ```

  `socat` や `systemd` のソケットアクティベーションを使ってもポート間ブリッジが可能です。
- **カーネル権限付与：** EMQXバイナリに `CAP_NET_BIND_SERVICE` 権限を付与し、直接ポート80にバインド可能にします：

  ```bash
  setcap 'cap_net_bind_service=+ep' \
         /opt/emqx/erts-*/bin/beam.smp
  ```

  この方法はOSやパッケージ方式に依存し、コンテナ環境では推奨されません。可能な限りリバースプロキシを使用してください。

## APIエンドポイント

プラグインAPIゲートウェイ `/api/v5/plugin_api/emqx_acme-<version>/` で利用可能な主なエンドポイントは以下の通りです：

| メソッド | パス | 説明 |
| --- | --- | --- |
| GET | `/status` | 現在の状態を返します。`domains`、`cert_bundle_name`、`in_progress`、`last_result`、`last_check`、`certificate` を含みます。証明書が存在する場合、`certificate` は `exists`、`chain_path`、`key_path`、`expiry` を含みます。存在しない場合は `exists: false` となります。 |
| POST | `/issue` | 非同期で証明書発行を開始します。`202 {"result":"started"}` を返し、結果は `/status` をポーリングしてください。別の処理が実行中の場合は `409` を返します。 |
| POST | `/renew` | `/issue` と同様の形で更新を開始します。 |
| POST | `/disable_dashboard_http` | クラスター全体で `dashboard.listeners.http.bind = 0` を設定し、平文HTTPリスナーを停止します。ダッシュボードHTTPSリスナーが未設定の場合は `409 NO_HTTPS_LISTENER` を返します。 |

これらのエンドポイントは主な証明書管理操作をサポートします。通常はプラグインUIがこれらを代行するため、直接呼び出す必要はありません。

## トラブルシューティング

### Let's Encryptステージングでは発行成功するが本番で失敗する場合

**症状：** 証明書発行が以下のようなエラーで失敗します：

> `During secondary validation: DNS problem: query timed out looking up A for ...`

**原因：** 二次検証時のDNSルックアップがタイムアウトしています。Let's Encryptはステージング・本番ともにマルチパースペクティブ検証を行うため、ステージングで成功しても本番で成功する保証はありません。DNSやネットワークの一時的な問題、DNS応答の不整合、ドメインのDNSレコードに到達不能なアドレスがあると異なる検証結果になることがあります。

**対処法：**

- ドメインの権威DNSサーバーが期待する `A` および `AAAA` レコードを一貫して返しているか確認してください。例：`dig @8.8.8.8 your.domain` や `dig @1.1.1.1 your.domain` を実行。
- ドメインの `A` および `AAAA` レコードで返されるすべてのアドレスに対してパブリックポート80が到達可能であり、トラフィックが設定した `challenge_port` に届いているか確認してください。
- [Let's Debug診断サービス](https://letsdebug.net) を使い、外部検証視点からドメインをチェックしてください。
- 再試行は控えてください。Let's Encrypt本番では1時間あたりアカウントごとに5回までの認証失敗制限があります。DNSやネットワークの問題を解決してから再度証明書をリクエストしてください。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## ダウンロード

各EMQXリリース向けのtarball：

| EMQXバージョン | プラグインバージョン | パッケージ |
|---|---|---|
| 6.3.0 | 0.2.0 | [emqx_acme-0.2.0.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.3.0/emqx_acme-0.2.0.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.3.0/emqx_acme-0.2.0.sha256)) |
| 6.3.1 | 0.2.0 | [emqx_acme-0.2.0.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.3.1/emqx_acme-0.2.0.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.3.1/emqx_acme-0.2.0.sha256)) |
| 6.3.2 | 0.2.0 | [emqx_acme-0.2.0.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.3.2/emqx_acme-0.2.0.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.3.2/emqx_acme-0.2.0.sha256)) |
| 6.3.3 | 0.2.0 | [emqx_acme-0.2.0.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.3.3/emqx_acme-0.2.0.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.3.3/emqx_acme-0.2.0.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
