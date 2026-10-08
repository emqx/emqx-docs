# EMQX ACMEプラグイン

EMQX ACMEプラグインは、Let's EncryptなどのACME対応証明書機関と連携し、EMQXのSSLリスナー用TLS証明書を自動で発行および更新します。本ページではEMQX 6.1でのプラグインの設定および使用方法について説明します。発行された証明書はEMQX管理の証明書バンドルに保存されます。

::: warning 重要なお知らせ
`<data_dir>/certs2/`をEMQXの再デプロイ間で永続化してください。プラグインは以下のファイルを`<data_dir>/certs2/global/<cert_bundle_name>/`に保存します：

- `chain.pem`および`key.pem`：発行された証明書バンドル。これらのファイルを紛失すると、プラグインは次回起動時に新しい証明書を発行します。この新しい証明書は、Let's Encryptのドメインあたり週5件の重複証明書制限にカウントされます。
- `acc-key.pem`：Let's Encryptに登録されたアカウントを識別するACMEアカウントキー。このファイルを紛失すると、再デプロイごとに新しいアカウントが作成されます。これにより、3時間あたりIPアドレスごとに10件の新規アカウント制限を消費し、以前のアカウントに関連付けられた証明書の失効ができなくなる可能性があります。

Docker環境では`<data_dir>`は`/opt/emqx/data`です。DEB/RPMインストールでは`/var/lib/emqx`です。Dockerでは`data/`ディレクトリ全体、または少なくとも`data/certs2/`をホストボリュームにバインドマウントしてください。Kubernetesでは永続ボリュームクレーム（PVC）を使用してください。初回発行時にプラグインはバンドル内にアカウントキーを生成し、`emqx_managed_certs`を通じてクラスター内の全ノードに複製します。
:::

## 前提条件

- ドメインはEMQXノードのグローバルIPアドレスに解決されている必要があります。
- HTTP-01チャレンジ検証のために、パブリックポート80がインターネットから到達可能である必要があります。`challenge_port`が`80`でない場合は、パブリックポート80から設定した`challenge_port`への転送を行ってください。
- ステージング環境でのテストには、Let's EncryptのステージングURL `https://acme-staging-v02.api.letsencrypt.org/directory` を使用してください。

## クイックスタート

パブリックに解決可能なドメインを持つ単一のEMQXノードでプラグインを設定する手順：

1. EMQXダッシュボードで **Management** -> **Plugins** をクリックし、プラグインをインストールして有効化します。
2. 以下のフィールドを設定します。その他のフィールドはデフォルト値のままにしてください：
   - `domains = "mqtt.example.com"`：カンマ区切りのドメインリストを入力します。各ドメインはこのノードにパブリックに解決されている必要があります。
   - `contact = "mailto:admin@example.com"`：証明書機関（CA）からの更新・失効通知用の連絡先アドレスをカンマ区切りで入力します。
   - `challenge_port = 5080`：EMQXがバインド可能な高いポート番号を入力します。リバースプロキシまたは`iptables`リダイレクトでパブリックポート80からこのポートへトラフィックを転送するよう設定してください。[ポート80アクセスの設定](#configure-port-80-access)を参照してください。
   - `dir_url`：デフォルトのLet's Encrypt本番URLのままにするか、設定テスト時はステージングURLを使用してください。
3. プラグインUIで **Issue / Renew Now** をクリックします。初回発行時はバンドルが空のため、プラグインは以下の処理を行います：
   - 管理証明書バンドル内にACMEアカウントキーが存在しない場合は生成します。
   - HTTP-01チャレンジを通じて証明書を発行します。
   - デフォルトで`listener_ids`は`ssl:default,wss:default`なので、それらのリスナーを新しいバンドルを使用するよう書き換えます。
   - `enable_dashboard_https`がデフォルトで`true`なので、ポート`18084`に同じ証明書を使ったダッシュボードHTTPSリスナーを作成します。

   2回目以降はバンドルファイルのみを更新し、リスナー設定やダッシュボードHTTPS設定は変更しません。Erlang SSL PEMキャッシュはリスナーを再起動せずに新しい証明書を読み込みます。
4. `https://your.domain:18084/`にアクセスしてダッシュボードにログインし、プラグインUIで **Disable Dashboard HTTP Listener** をクリックします。このボタンはプラグインページがHTTPSで開かれている場合のみ表示されます。操作が成功すると、ポート`18083`の平文リスナーがクラスター全体で無効化されます。この設定は本番環境で推奨されます。HTTPリスナーを有効にしたままだとダッシュボードへの平文アクセスが可能になるためです。

プラグインは`check_interval_hours`で指定された間隔で証明書をチェックし、必要に応じて自動更新します。

## 動作概要

1. プラグインは設定されたCAにACMEアカウントを登録（または既存アカウントを再利用）します。
2. 発行時にHTTP-01チャレンジに応答するための一時的なHTTPリスナーを起動します。
3. 発行された証明書チェーンと秘密鍵は管理証明書バンドルに保存されます。デフォルトではACMEアカウントキーもこのバンドルに保存されます。`acc_key`が設定されている場合は、代わりにオペレーター管理のファイルを使用します。詳細は[ACMEアカウントキー](#acme-account-key)を参照してください。
4. SSLリスナーは`ssl_options.managed_certs.bundle_name`を通じてバンドルを参照します。初回発行時にプラグインは`listener_ids`で指定されたリスナーのこのフィールドを書き換えます。
5. プラグインは`check_interval_hours`で指定された間隔で証明書をチェックし、`renew_before_expiry_days`で指定された期間内に証明書が期限切れとなる場合は更新します。更新はバンドルファイルをその場で書き換え、Erlang SSL PEMキャッシュはリスナーを再起動せずに新しい証明書を読み込みます。

## 設定例

プラグインは`config_schema.avsc`からフィールド説明をダッシュボードの設定フォームにレンダリングします。以下のHOCON例は典型的なプラグイン設定例です。ダッシュボードのフィールドラベルにカーソルを合わせると説明が表示されます。

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
# EMQXがバインド可能な高いポート。80番からのリバースプロキシまたはiptablesリダイレクト用。
challenge_port = 5080
renew_before_expiry_days = 30
check_interval_hours = 24
enable_dashboard_https = true
dashboard_https_port = 18084
# acc_keyは未設定。プラグインが証明書バンドル内で管理。
```

次にSSLリスナーをバンドルを使用するよう設定します。`listener_ids`で指定されたリスナーは初回発行時にプラグインがこの設定を書き換えます。

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

RFC 8555では、ACMEアカウントの秘密鍵がアカウントを識別します。クライアントはローカルで鍵を生成し、その鍵で署名した`newAccount`リクエストを送信します。CAはそれに基づきアカウントを作成します。鍵はポータルを通じて別途登録されません。

**デフォルト動作：** `acc_key`を未設定にします。初回発行時にプラグインはメモリ上でEC P-256鍵（`cert_type = "rsa"`の場合はRSA-2048鍵）を生成します。プラグインは`emqx_managed_certs:add_managed_files/3`を使って、各クラスター・ノードの`<data_dir>/certs2/global/<cert_bundle_name>/acc-key.pem`に鍵を書き込みます。以降の発行では同じファイルを再利用します。アカウントキーと証明書チェーンを保持するために、データディレクトリをバインドマウントやPVCで永続化してください。本ページ冒頭の永続化警告を参照してください。

**オペレーターによる上書き：** Kubernetes Secretのようにバンドル外のパスに鍵を置く必要がある場合は、`acc_key`にPEMファイルの`file://` URIを設定します。プラグインは発行時に毎回このファイルを読み込み、上書きしません。ローカルノードにファイルが存在しない場合は、そのノードで新規生成します。このファイルはクラスター間で複製されないため、各ノードに配布が必要です。PEMファイルが暗号化されている場合は、`acc_key_password`に平文パスワードファイルの`file://` URIを設定してください。`${EMQX_ETC_DIR}`や`${VAR}`は展開されるため、DockerやDEB/RPMインストールで同じ設定が動作します。

## ポート80アクセスの設定

ACME CAは常に検証対象ドメインのポート80に対してHTTP-01チャレンジを行います。この動作はRFC 8555で定義されており、CA側で変更できません。EMQXは非rootユーザー`emqx`で動作し、通常1024未満のポートにバインドできません。したがって、`challenge_port = 80`は通常`eacces`エラーになります。

`challenge_port`をEMQXがバインド可能な高いポート（例：`5080`）に設定し、以下のいずれかの方法でパブリックポート80から設定した`challenge_port`へトラフィックをルーティングしてください：

- **リバースプロキシ：** NGINX、Caddy、HAProxyをrootまたは`CAP_NET_BIND_SERVICE`権限で同一ホストに起動し、`http://domain/.well-known/acme-challenge/*`を`http://127.0.0.1:<challenge_port>`にプロキシします。他のパスは`404`を返すようにしてください。
- **ポートフォワーディング：** Linuxでは`iptables`でポート80への着信を高いポートへリダイレクトします：

  ```bash
  iptables -t nat -A PREROUTING -p tcp --dport 80 \
                  -j REDIRECT --to-port 5080
  ```

  `socat`や`systemd`のソケットアクティベーションを使ってもブリッジ可能です。
- **カーネル権限付与：** EMQXバイナリに`CAP_NET_BIND_SERVICE`権限を付与し、直接ポート80にバインド可能にします：

  ```bash
  setcap 'cap_net_bind_service=+ep' \
         /opt/emqx/erts-*/bin/beam.smp
  ```

  この方法はOSやパッケージ方式に依存し、コンテナ環境では推奨されません。可能な限りリバースプロキシを使用してください。

## APIエンドポイント

プラグインAPIゲートウェイは`/api/v5/plugin_api/emqx_acme-<version>/`で以下の主要エンドポイントを提供します：

| メソッド | パス | 説明 |
| --- | --- | --- |
| GET | `/status` | 現在の状態を返します。`domains`、`cert_bundle_name`、`in_progress`、`last_result`、`last_check`、`certificate`を含みます。証明書が存在する場合、`certificate`には`exists`、`chain_path`、`key_path`、`expiry`が含まれます。存在しない場合は`exists: false`です。 |
| POST | `/issue` | 非同期で証明書発行を開始します。`202 {"result":"started"}`を返し、結果は`/status`をポーリングしてください。別の操作が実行中の場合は`409`を返します。 |
| POST | `/renew` | `/issue`と同様の形で更新を開始します。 |
| POST | `/disable_dashboard_http` | クラスター全体で`dashboard.listeners.http.bind = 0`を設定し、平文リスナーを停止します。ダッシュボードHTTPSリスナーが設定されていない場合は`409 NO_HTTPS_LISTENER`を返します。 |

これらのエンドポイントは主な証明書管理操作をサポートします。通常はプラグインUIがこれらを実行するため、直接呼び出す必要はありません。

## トラブルシューティング

### Let's Encryptステージング環境では発行成功するが本番環境で失敗する場合

**症状：** 以下のようなテキストを含むエラーで証明書発行が失敗します：

> `During secondary validation: DNS problem: query timed out looking up A for ...`

**原因：** セカンダリ検証時のDNSルックアップタイムアウトを示しています。Let's Encryptはステージング・本番ともにマルチパースペクティブ検証を行います。ステージングで成功しても本番で成功する保証はありません。DNSやネットワークの一時的な問題、DNS応答の不整合、ドメインのDNSレコードに到達不能なアドレスがある場合に異なる検証結果となることがあります。

**対処方法：**

- ドメインの権威DNSサーバーが期待通りの`A`および`AAAA`レコードを一貫して返すか確認してください。例：`dig @8.8.8.8 your.domain`および`dig @1.1.1.1 your.domain`を実行。
- ドメインの`A`および`AAAA`レコードで返されるすべてのアドレスに対してパブリックポート80が到達可能であり、トラフィックが設定した`challenge_port`に届いていることを確認してください。
- [Let's Debug診断サービス](https://letsdebug.net)を使い、外部検証視点からドメインをチェックしてください。
- 再試行は控えてください。Let's Encrypt本番環境では1時間あたりアカウントごとに識別子あたり最大5回の認証失敗が許容されます。DNSやネットワークの問題を解決してから再度証明書をリクエストしてください。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## ダウンロード

各EMQXリリース用のtarball：

| EMQXバージョン | プラグインバージョン | パッケージ |
|---|---|---|
| 6.1.2 | 0.2.0 | [emqx_acme-0.2.0.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.1.2/emqx_acme-0.2.0.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.1.2/emqx_acme-0.2.0.sha256)) |
| 6.1.3 | 0.2.0 | [emqx_acme-0.2.0.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.1.3/emqx_acme-0.2.0.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.1.3/emqx_acme-0.2.0.sha256)) |
| 6.1.4 | 0.2.0 | [emqx_acme-0.2.0.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.1.4/emqx_acme-0.2.0.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.1.4/emqx_acme-0.2.0.sha256)) |
| 6.1.5 | 0.2.0 | [emqx_acme-0.2.0.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.1.5/emqx_acme-0.2.0.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.1.5/emqx_acme-0.2.0.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
