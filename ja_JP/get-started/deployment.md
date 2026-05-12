# デプロイメントFAQ

## EMQXのデプロイに推奨されるOSは何ですか？

EMQXはさまざまなOSおよびハードウェアプラットフォームで動作しますが、企業レベルの安定性と信頼性を考慮すると、一般的にはCentOS、Ubuntu、DebianなどのLinuxディストリビューションでのデプロイを推奨しています。

## EMQXの推奨デプロイメントプランは？

EMQXはクラスターでのデプロイを推奨しており、クラスターのフロントエンドにロードバランサー（Nginx、HAProxyなど）を配置して、クライアント接続を各ノードに均等に振り分ける構成が望ましいです。

通信のセキュリティ要件が高いユーザーには、クライアント側でTLS接続を有効化し、ロードバランサー側でTLS終端を行うことを推奨します。つまり、クライアントとロードバランサー間はTLS暗号化通信を使用し、ロードバランサーとEMQXノード間はTCP通信を利用します。

EMQXノードはポートをパブリックネットワークに公開しないため、全体のセキュリティは低下しませんが、TLSのオフロードによりEMQXのリソース消費を効果的に節約できます。

## デバイス数やメッセージスループットが少ない場合でもクラスターを構築すべきですか？

デバイス数やメッセージスループットが少なくても、本番環境ではクラスター構成を推奨します。

クラスターはシステムの可用性を向上させ、単一障害点のリスクを低減します。ノードがダウンしても、クラスター内の他の正常なノードがサービスを継続し、業務への影響を防ぎます。

## EMQXが起動しない場合のトラブルシューティング方法は？

EMQXが起動しない場合は、[ログディレクトリ](./deploy/install.md#files-and-directories)内の `emqx.log.N` または `erlang.log.N` を確認して詳細なエラー情報を取得してください。

または、`emqx console` コマンドでコンソールから起動すると、エラーログが直接コンソールに出力されます。ログ内容に基づき、本ページの対応策を参照するか、[GitHub](https://github.com/emqx/emqx/issues)にてサポートを依頼してください。

## ログに「logger: command not found」と表示されEMQXが起動しない場合の対処法は？

以下の依存パッケージをインストールしてください。

- `CentOS/Redhat`

```
$ yum install rsyslog
```

- `Ubuntu/Debian`

```
$ apt-get install bsdutils
```

## ログに「...{on_load_function_failed,crypto}...」と表示されEMQXが起動しない場合の対処法は？

セキュリティ強化のため、EMQXはバージョン4.3以降でopenssl-1.1を使用しています。古いLinuxディストリビューションでは問題が発生する場合があります。

EMQXバージョン4.3.10未満およびEnterprise版e4.3.5未満では、以下のようなエラーが表示されることがあります。

```bash
{application_start_failure,kernel,{{shutdown,{failed_to_start_child,kernel_safe_sup,{on_load_function_failed,crypto}}}, ..}
```

それ以降のバージョンでは、以下のようなエラーが表示されます。

```bash
FATAL: Unable to start Erlang.
Please make sure openssl-1.1.1 (libcrypto) and libncurses are installed.
```

これは、EMQXが依存するErlang/OTPの「crypto」アプリケーションが、必要なopensslの動的ライブラリ（.so）を見つけられず起動に失敗したことを示しています。対処法は以下の通りです。

::: warning 重要なお知らせ

以下の対処例はあくまで一例です。

記載のソースバージョンは現時点の知見に基づいており、古いものや脆弱性が含まれる可能性があります。

最新のセキュリティアップデートを得るには、OSのパッケージマネージャーから直接 `libcrypto` をインストールすることを推奨します。

:::

:::: tabs

::: tab CentOS

Extra Packages for Enterprise Linux（EPEL）は、Fedoraの特別興味グループで、Enterprise Linux向けの高品質な追加パッケージを提供しています。CentOS 7を例に説明します。

1. RPMリポジトリをインストールするには、`yum install epel-release` を実行します。
2. インストールに失敗した場合は、https://docs.fedoraproject.org/en-US/epel/#_el7 の手順に従い、yumリポジトリが追加されていることを確認し、再度1を実行してください。
3. `yum install openssl11` を実行してopenssl-1.1をインストールします。

:::

::: tab Linux

EMQXのインストールディレクトリに移動します（パッケージ管理ツールでインストールした場合は、EMQXの `lib` と同階層のディレクトリに入ります）。

```bash
  ## パッケージインストールの場合
$ cd emqx

  ## yumなどのパッケージ管理ツールでインストールした場合
$ cd /lib/emqx
```

`crypto` が依存する `.so` 動的ライブラリの一覧とメモリ上の場所を確認します。

```bash
$ ldd lib/crypto-*/priv/lib/crypto.so

lib/crypto-4.6/priv/lib/crypto.so: /lib64/libcrypto.so.10: version `OPENSSL_1.1.1' not found (required by lib/crypto-4.6/priv/lib/crypto.so)
          linux-vdso.so.1 =>  (0x00007fff67bfc000)
          libcrypto.so.10 => /lib64/libcrypto.so.10 (0x00007fee749ca000)
          libc.so.6 => /lib64/libc.so.6 (0x00007fee74609000)
          libdl.so.2 => /lib64/libdl.so.2 (0x00007fee74404000)
          libz.so.1 => /lib64/libz.so.1 (0x00007fee741ee000)
          /lib64/ld-linux-x86-64.so.2 (0x00007fee74fe5000)
```

`OPENSSL_1.1.1' not found` は指定されたOPENSSLバージョンの `.so` ライブラリが正しくインストールされていないことを示します。

OPENSSL 1.1.1をソースからコンパイル・インストールし、システムが認識するパスに `.so` ファイルを配置してください。

```bash
## 最新の1.1.1バージョンをダウンロード
$ wget https://www.openssl.org/source/openssl-1.1.1c.tar.gz

## ct-test-haへアップロード
$ scp openssl-1.1.1c.tar.gz ct-test-ha:~/

## 解凍、コンパイル、インストール
$ tar zxf   openssl-1.1.1c.tar.gz
$ cd openssl-1.1.1c
$ ./config
$ make test   		# テスト実行。PASSが出れば続行
$ make install

## ライブラリ参照の確保
$ ln -s /usr/local/lib64/libssl.so.1.1 /usr/lib64/libssl.so.1.1
$ ln -s /usr/local/lib64/libcrypto.so.1.1 /usr/lib64/libcrypto.so.1.1
```

完了後、EMQXのlibディレクトリで `ldd lib/crypto-*/priv/lib/crypto.so` を実行し、`.so` ライブラリが `not found` なく認識されていれば正常に起動できます。

:::

::: tab macOS

EMQXのインストールディレクトリに移動します。

```bash
  ## パッケージインストールの場合
$ cd emqx

  ## brewインストールの場合
$ cd /usr/local/Cellar/emqx/<version>/
```

`crypto` が依存する `.so` 動的ライブラリの一覧を確認します。

```bash
$ otool -L lib/crypto-*/priv/lib/crypto.so

lib/crypto-4.4.2.1/priv/lib/crypto.so:
  /usr/local/opt/openssl@1.1/lib/libcrypto.1.1.dylib (compatibility version 1.1.0, current version 1.1.0)
  /usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1252.200.5)
```

`otool` の出力に基づき、指定されたディレクトリにOPENSSLが正しくインストールされているか確認します。

```bash
$ ls /usr/local/opt/openssl@1.1/lib/libcrypto.1.1.dylib
ls: /usr/local/opt/openssl@1.1/lib/libcrypto.1.1.dylib: No such file or directory
```

ファイルが存在しない場合は、`otool` の出力に対応するOPENSSLバージョンをインストールしてください。例として `openssl@1.1` の場合：

```bash
$ brew install openssl@1.1
```

インストール完了後、EMQXを正常に起動できます。

## ログに「libatomic.so.1: cannot open shared object file: No such file or directory」と表示されEMQXが起動しない場合の対処法は？

このエラーはシステムに依存ライブラリ `libatomic` が不足しているため発生します。以下のコマンドでインストールしてください。

```
# Rocky Linux, CentOS など
yum install -y libatomic
# Debian, Ubuntu など
apt install -y libatomic
```

RPMやDEBパッケージを手動でインストールする際に以下のような依存関係エラーが出る場合もあります。

```
$ rpm -ivh emqx-5.7.0-el8-amd64.rpm
error: Failed dependencies:
libatomic is needed by emqx-5.7.0-el8-amd64.rpm
```

この場合も、まず `libatomic` を手動でインストールしてください。

なお、最も推奨されるインストール方法はパッケージマネージャー（yum、aptなど）を利用することで、依存関係を自動的に解決できます。

:::

::::

## DockerでEMQXを起動した際に「Permission denied」と表示され起動に失敗する場合の対処法

EMQXのデータ永続化のためにディレクトリをマウントして起動する場合：

```
sudo docker run -d --name emqx -p 18083:18083 -p 1883:1883 -v /emqx/data:/opt/emqx/data -v /emqx/log:/opt/emqx/log emqx:latest
```

以下のようなエラーでコンテナ起動に失敗することがあります。

```
mkdir: cannot create directory '/opt/emqx/data/configs': Permission denied
```

これはコンテナ内のEMQXがLinuxユーザー `emqx` として動作しているのに対し、ホスト側のディレクトリが `root` ユーザーで作成されているため、EMQXがディレクトリやファイルを作成できないことが原因です。

解決策として、ホスト側に `emqx` ユーザーを作成し、そのユーザーでマウント対象ディレクトリを作成するか、作成済みのデータ・ログディレクトリの権限を777に変更してください。

なお、最も推奨されるデータ永続化方法は名前付きボリュームを使用することで、権限問題を気にせずに済みます。

```
sudo docker volume create --name emqx-data
sudo docker volume create --name emqx-log
sudo docker run -d --name emqx -p 18083:18083 -p 1883:1883 -v emqx-data:/opt/emqx/data -v emqx-log:/opt/emqx/log emqx:latest
```

## EMQX起動時に「ポートが使用中（eaddrinuse）」と表示された場合の対処法は？

EMQXは起動時に以下の7つのポートを使用します。

1. ポート1883：MQTTのTCPリスナー。設定で変更可能。
2. ポート8883：MQTTのSSL/TLSリスナー。設定で変更可能。
3. ポート8083：MQTTのWebSocketリスナー。設定で変更可能。
4. ポート8084：MQTTのWSS（WebSocket over SSL）リスナー。設定で変更可能。
5. ポート18083：HTTP APIサービスのデフォルトリスニングポート。ダッシュボードもこのポートを利用。設定で変更可能。
6. ポート4370：EMQX分散クラスターのリモート関数呼び出しおよびMnesiaデータ同期に使用。クラスター未構成でもデフォルトで占有。リスニングポートは `BasePort (4370) + Offset` で決まり、4370は固定、Offsetはノード名の数値サフィックスに依存。数値サフィックスがなければ0。例：`emqx@127.0.0.1` のOffsetは0、`emqx1@127.0.0.1` のOffsetは1。
7. ポート5370：クラスターRPCポートで負荷分散に使用。主にノード間のMQTTメッセージ転送に利用。ポート4370と同様にクラスター未構成でもデフォルトで占有。実際のリスニングポートは `BasePort (5370) + Offset` で決まり、5370は固定、Offsetはノード名のName部分の数値サフィックスに依存。数値サフィックスがなければ0。

これらのポートが他のプロセスで使用されている場合、EMQXは起動に失敗します。競合を避けるため、設定ファイルでポート番号を変更するか、競合しているプロセスを停止してください。

## EMQX起動時に「WARNING: Default (insecure) Erlang cookie is in use.」と表示される理由は？

警告ログ全文は以下の通りです。

```
WARNING: Default (insecure) Erlang cookie is in use.
WARNING: Configure node.cookie in /usr/lib/emqx/etc/emqx.conf or override from environment variable EMQX_NODE__COOKIE
WARNING: NOTE: Use the same cookie for all nodes in the cluster.
```

クラスターを形成するには、EMQXノードは同一のcookieを使用する必要があります。cookieはクラスター通信のセキュリティを保証するものではありませんが、意図しないノードの接続を防止します。デフォルトでは全ノードが `emqxsecretcookie` を使用していますが、クラスター構築時にはセキュリティ強化のためcookie値を変更することを推奨します。

2番目の警告は、cookieを変更する方法として、`emqx.conf` の `node.cookie` を編集するか、環境変数 `EMQX_NODE__COOKIE` を設定する2通りがあることを示しています。

## EMQX Dockerコンテナを再起動すると、設定したルールやリソースなどのデータが消失する理由は？

EMQXのランタイムデータは `/opt/emqx/data` ディレクトリに保存されており、設定ルール、リソース、保持メッセージなどが含まれます。コンテナ再起動時にデータを保持するには、このディレクトリをホストのローカルディレクトリやデータボリュームにマウントする必要があります。

しかし、マウントしていても再起動後にデータが消失する場合があります。これは、EMQXのランタイムデータが `/opt/emqx/data/mnesia/${Node Name}` に保存され、コンテナ再起動時にEMQXのノード名が変わることで新しいストレージディレクトリが作成されるためです。

EMQXのノード名はNameとHostで構成され、HostはデフォルトでコンテナのIPアドレスから取得されます。デフォルトネットワーク設定ではコンテナのIPが再起動時に変わるため、固定IPを維持する必要があります。

この問題を解決するため、EMQXは環境変数 `EMQX_HOST` を提供しており、ノード名のHost部分を指定できます。ただし、このHost値は他ノードから到達可能である必要があるため、ネットワークエイリアスと併用してください。以下は `EMQX_HOST` とネットワークエイリアスを指定したDockerコンテナ起動例です。

```
docker run -d --name emqx -p 18083:18083 -p 1883:1883 -e EMQX_HOST=alias-for-emqx --network example --network-alias alias-for-emqx --mount type=bind,source=/tmp/emqx,target=/opt/emqx/data emqx:5.8.3
```

## docker-composeで起動したコンテナが正常に起動しダッシュボードにアクセスできるのに、ステータスがunhealthyになる理由は？

```bash
docker-compose ps
NAME      IMAGE                         COMMAND                  SERVICE   CREATED          STATUS                    PORTS
emqx1     emqx/emqx:latest   "/usr/bin/docker-ent…"   emqx     120 seconds ago   Up 110 seconds (unhealthy)   0.0.0.0:1883->1883/tcp, :::1883->1883/tcp, 0.0.0.0:18083->18083/tcp, :::18083->18083/tcp
```

EMQXのヘルスチェックは `./bin/emqx_ctl status` コマンドに依存しています。このコマンドが失敗すると、コンテナはunhealthy状態となります。

```yaml
healthcheck:
      test: ["CMD", "/opt/emqx/bin/emqx_ctl", "status"]
      interval: 60s
      timeout: 15s
      retries: 3
```

手動で `./bin/emqx_ctl status` を実行すると以下のようなエラーが出る場合があります。

```
emqx@docker:/opt/emqx$ emqx_ctl status
Node emqx@docker not responding to pings.
```

これはコマンドがノードに接続できていないことを示します。多くの場合、コンテナ起動時にネットワークがエイリアスを使わず、FQDN形式でないためノードが正しく特定できないことが原因です。

解決策は以下の通りです。

1. Dockerのホスト名をEMQXノード名に合わせる。
2. `docker-compose.yml` にホスト名設定を追加する。

```yaml
# xxx.yyy.zzz(docker.emqx.com) はFQDN形式である必要があります
hostname: docker.emqx.com
environment:
      - EMQX_HOST=docker.emqx.com
```

EMQXはデータを `data/mnesia/<node name>` に保存するため、IPアドレスではなくホスト名やFQDNなど固定の識別子をノード名として使用し、ノード名の変更によるデータ消失を防ぐことが重要です。

より簡単に構成するには、[EMQX Docker Compose Generator](https://docker.emqx.dev/) を利用して、本番環境向けの `docker-compose.yml` を作成することを推奨します。
