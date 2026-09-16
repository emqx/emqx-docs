# EMQX ACME プラグイン

EMQX ACME プラグインは、Let's Encrypt などの ACME 対応証明書機関と連携し、EMQX の SSL リスナー向けに TLS 証明書を自動発行および更新します。本ページでは、EMQX 6.1 でのプラグインの設定および使用方法について説明します。発行された証明書は EMQX 管理の証明書バンドルに保存されます。

::: warning 重要なお知らせ
`<data_dir>/certs2/` を EMQX の再デプロイ間で永続化してください。プラグインは以下のファイルを `<data_dir>/certs2/global/<cert_bundle_name>/` 配下に保存します。

- `chain.pem` と `key.pem`：発行された証明書バンドルです。これらのファイルを紛失すると、プラグインは次回起動時に新しい証明書を発行します。この新しい証明書は、Let's Encrypt のドメインごとの週あたり最大5件の重複証明書制限にカウントされます。
- `acc-key.pem`：Let's Encrypt に登録されたアカウントを識別する ACME アカウントキーです。このファイルを紛失すると、再デプロイごとに新しいアカウントが作成されます。これにより、3時間あたりIPアドレスごとに最大10件の新規アカウント制限を消費し、以前のアカウントに関連付けられた証明書の失効ができなくなる可能性があります。

Docker 環境では `<data_dir>` は `/opt/emqx/data` です。DEB/RPM インストールでは `/var/lib/emqx` です。Docker では `data/` ディレクトリ全体、または少なくとも `data/certs2/` をホストボリュームにバインドマウントしてください。Kubernetes では永続ボリュームクレーム（PVC）を使用してください。初回発行時にプラグインはバンドル内にアカウントキーを生成し、`emqx_managed_certs` を通じてクラスター内の全ノードに複製します。
:::

## 前提条件

- ドメインは EMQX ノードのパブリックIPアドレスに解決されている必要があります。
- HTTP-01 チャレンジ検証のために、パブリックポート 80 がインターネットから到達可能である必要があります。`challenge_port` が `80` でない場合は、パブリックポート 80 から設定した `challenge_port` への転送を行ってください。
- ステージングテストには、Let's Encrypt のステージングURL `https://acme-staging-v02.api.letsencrypt.org/directory` を使用してください。

## クイックスタート

パブリックに解決可能なドメインを持つ単一の EMQX ノードでプラグインを設定する手順です。

1. EMQX ダッシュボードで **Management** -> **Plugins** をクリックし、プラグインをインストールして有効化します。
2. 以下のフィールドを設定します。他のフィールドはデフォルト値のままにしてください。
   - `domains = "mqtt.example.com"`：カンマ区切りでドメインを入力します。各ドメインはこのノードにパブリックに解決されている必要があります。
   - `contact = "mailto:admin@example.com"`：証明書機関（CA）からの更新・失効通知用の連絡先アドレスをカンマ区切りで入力します。
   - `challenge_port = 5080`：EMQX がバインド可能な高位ポートを入力します。パブリックポート 80 からこのポートへトラフィックを転送するリバースプロキシや `iptables` リダイレクトを設定してください。[ポート80アクセスの設定](#configure-port-80-access)を参照してください。
   - `dir_url`：デフォルトの Let's Encrypt 本番URLを使用するか、設定テスト時はステージングURLを使用してください。
3. プラグイン UI で **Issue / Renew Now** をクリックします。初回発行時にバンドルが空の場合、プラグインは以下の処理を行います。
   - 管理証明書バンドル内に ACME アカウントキーを生成（存在しない場合）。
   - HTTP-01 を通じて証明書を発行。
   - `listener_ids`（デフォルトは `ssl:default,wss:default`）で指定された各リスナーの設定を新しいバンドルを使うよう書き換え。
   - `enable_dashboard_https` がデフォルトで `true` のため、同じ証明書でポート `18084` にダッシュボードの HTTPS リスナーを作成。

   以降の実行ではバンドルファイルのみ更新し、リスナー設定やダッシュボード HTTPS 設定は変更しません。Erlang SSL PEM キャッシュはリスナーの再起動なしに新証明書を読み込みます。
4. `https://your.domain:18084/` にアクセスしてダッシュボードにログインし、プラグイン UI で **Disable Dashboard HTTP Listener** をクリックします。このボタンはプラグインページが HTTPS で開かれている場合のみ表示されます。操作成功後、ポート `18083` の平文リスナーがクラスター全体で無効化されます。HTTP リスナーを有効にしたままにするとダッシュボードへの平文アクセスを許可してしまうため、本番環境ではこの設定を推奨します。

プラグインは `check_interval_hours` で指定された間隔で証明書をチェックし、必要に応じて自動更新します。

## 動作概要

1. プラグインは設定された CA に ACME アカウントを登録（または既存アカウントを再利用）します。
2. 発行時に HTTP-01 チャレンジに応答するため、一時的な HTTP リスナーを起動します。
3. 発行された証明書チェーンと秘密鍵は管理証明書バンドルに保存されます。デフォルトでは ACME アカウントキーもこのバンドルに保存します。`acc_key` が設定されている場合は、オペレーター管理のファイルを使用します。詳細は [ACME アカウントキー](#acme-account-key) を参照してください。
4. SSL リスナーは `ssl_options.managed_certs.bundle_name` を通じてバンドルを参照します。初回発行時にプラグインは `listener_ids` で指定されたリスナーのこのフィールドを書き換えます。
5. プラグインは `check_interval_hours` で指定された間隔で証明書をチェックし、`renew_before_expiry_days` で指定された期限前に証明書が切れる場合は更新します。更新はバンドルファイルをその場で書き換え、Erlang SSL PEM キャッシュはリスナーの再起動なしに新証明書を読み込みます。

## 設定例

プラグインは `config_schema.avsc` のフィールド説明をダッシュボードの設定フォームに表示します。以下の HOCON 例は典型的なプラグイン設定です。ダッシュボードのフィールドラベルにカーソルを合わせると説明が表示されます。

```hocon
dir_url = "https://acme-v02.api.letsencrypt.org/directory"
# 証明書の SAN ドメインのカンマ区切りリスト
domains = "mqtt.example.com,mqtt2.example.com"
# CA 連絡先アドレスのカンマ区切りリスト（更新・失効通知用）
contact = "mailto:admin@example.com,mailto:ops@example.com"
cert_bundle_name = "acme"
# 移行対象のリスナーIDのカンマ区切りリスト（各要素は "ssl:<name>" または "wss:<name>"）
listener_ids = "ssl:default,wss:default"
cert_type = "ec"
# EMQX がバインド可能な高位ポート。80 -> このポートへのリバースプロキシや iptables リダイレクトを設定
challenge_port = 5080
renew_before_expiry_days = 30
check_interval_hours = 24
enable_dashboard_https = true
dashboard_https_port = 18084
# acc_key は未設定。プラグインが証明書バンドル内で管理
```

続いて、SSL リスナーをバンドルを使うよう設定します。`listener_ids` で指定されたリスナーは初回発行時にプラグインがこの設定を書き換えます。

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

## ACME アカウントキー

RFC 8555 によると、ACME アカウント秘密鍵はアカウントを識別します。クライアントはローカルで鍵を生成し、その鍵で署名した `newAccount` リクエストを送信します。CA はこれによりアカウントを作成します。鍵はポータル経由で別途登録されません。

**デフォルト動作：** `acc_key` を未設定にします。初回発行時にプラグインはメモリ上で EC P-256 鍵（`cert_type = "rsa"` の場合は RSA-2048 鍵）を生成します。次に `emqx_managed_certs:add_managed_files/3` を使い、鍵を `<data_dir>/certs2/global/<cert_bundle_name>/acc-key.pem` にクラスター内全ノードで書き込みます。以降の発行は同じファイルを再利用します。アカウントキーと証明書チェーンを保持するため、データディレクトリはバインドマウントや PVC で永続化してください。本ページ冒頭の永続化警告を参照してください。

**オペレーターによる上書き：** Kubernetes Secret のようにバンドル外のパスで管理する必要がある場合は `acc_key` を設定します。PEM ファイルの `file://` URI を指定してください。プラグインは発行時にファイルを読み込み、上書きしません。ローカルノードにファイルが存在しない場合はそのノードで生成します。このファイルはクラスター間で複製されないため、各ノードに配布が必要です。PEM ファイルが暗号化されている場合は、平文パスワードファイルの `file://` URI を `acc_key_password` に設定してください。`${EMQX_ETC_DIR}` と `${VAR}` は展開され、Docker や DEB/RPM インストールで同じ設定を利用できます。

## ポート80アクセスの設定

ACME CA は常に検証対象ドメインのポート80で HTTP-01 チャレンジを実施します。この仕様は RFC 8555 で定義されており、CA 側で変更できません。EMQX は通常、非 root ユーザー `emqx` で動作し、1024 未満のポートにバインドできません。そのため `challenge_port = 80` は通常 `eacces` エラーで失敗します。

`challenge_port` を EMQX がバインド可能な高位ポート（例：5080）に設定し、以下のいずれかの方法でパブリックポート80から設定ポートへトラフィックをルーティングしてください。

- **リバースプロキシ：** 同一ホスト上で root または `CAP_NET_BIND_SERVICE` 権限を持つ NGINX、Caddy、HAProxy を起動し、`http://domain/.well-known/acme-challenge/*` を `http://127.0.0.1:<challenge_port>` にプロキシします。その他のパスは `404` を返します。
- **ポート転送：** Linux であれば `iptables` を使いポート80への着信を高位ポートにリダイレクトします。

  ```bash
  iptables -t nat -A PREROUTING -p tcp --dport 80 \
                  -j REDIRECT --to-port 5080
  ```

  `socat` や `systemd` ソケットアクティベーションも利用可能です。
- **カーネル権限付与：** EMQX バイナリに `CAP_NET_BIND_SERVICE` 権限を付与し、直接ポート80にバインド可能にします。

  ```bash
  setcap 'cap_net_bind_service=+ep' \
         /opt/emqx/erts-*/bin/beam.smp
  ```

  この方法は OS やパッケージ方式に依存し、コンテナ環境では推奨されません。可能な限りリバースプロキシを使用してください。

## API エンドポイント

プラグイン API ゲートウェイは `/api/v5/plugin_api/emqx_acme-<version>/` で以下の主要エンドポイントを提供します。

| メソッド | パス | 説明 |
| --- | --- | --- |
| GET | `/status` | 現在の状態を返します。`domains`、`cert_bundle_name`、`in_progress`、`last_result`、`last_check`、`certificate` を含みます。証明書が存在する場合、`certificate` は `exists`、`chain_path`、`key_path`、`expiry` を含みます。存在しない場合は `exists: false` です。 |
| POST | `/issue` | 非同期で証明書発行を開始します。`202 {"result":"started"}` を返し、結果は `/status` をポーリングしてください。別の処理が実行中の場合は `409` を返します。 |
| POST | `/renew` | `/issue` と同様の形で更新を開始します。 |
| POST | `/disable_dashboard_http` | クラスター全体で `dashboard.listeners.http.bind = 0` を設定し、平文リスナーを停止します。ダッシュボード HTTPS リスナーが設定されていない場合は `409 NO_HTTPS_LISTENER` を返します。 |

これらのエンドポイントは主な証明書管理操作をサポートしますが、通常はプラグイン UI がこれらを代行するため直接呼び出す必要はありません。

## トラブルシューティング

### Let's Encrypt ステージング環境では発行成功するが本番環境で失敗する

**症状：** 証明書発行が以下のようなエラーで失敗します。

> `During secondary validation: DNS problem: query timed out looking up A for ...`

**原因：** セカンダリ検証時の DNS ルックアップタイムアウトを示しています。Let's Encrypt はステージング・本番ともにマルチパースペクティブ検証を行います。ステージングで成功しても本番で成功する保証はありません。DNS やネットワークの一時的な問題、DNS 応答の不整合、ドメインの DNS レコードに到達不能なアドレスが含まれていることが原因となることがあります。

**対処法：**

- ドメインの権威ネームサーバーが期待通りの `A` および `AAAA` レコードを一貫して返すか確認してください。例：`dig @8.8.8.8 your.domain` および `dig @1.1.1.1 your.domain` を実行。
- ドメインの `A` および `AAAA` レコードで返されるすべてのアドレスに対して、パブリックポート80が到達可能であり、トラフィックが設定した `challenge_port` に届いていることを確認してください。
- [Let's Debug 診断サービス](https://letsdebug.net) を使い、外部検証視点からドメインをチェックしてください。
- 再試行を繰り返さないでください。Let's Encrypt 本番環境では、識別子ごとにアカウントあたり1時間に最大5回の認証失敗が許容されます。DNS またはネットワーク問題を解決してから再度証明書をリクエストしてください。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## ダウンロード

各 EMQX リリース向けの tarball：

| EMQX バージョン | プラグインバージョン | パッケージ |
|---|---|---|
| 6.3.0 | 0.2.0 | [emqx_acme-0.2.0.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.3.0/emqx_acme-0.2.0.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.3.0/emqx_acme-0.2.0.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
