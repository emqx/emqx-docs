# デプロイメントFAQ

## EMQXのデプロイに推奨されるオペレーティングシステムは何ですか？

EMQXはさまざまなオペレーティングシステムおよびハードウェアプラットフォームでの動作をサポートしています。エンタープライズレベルの安定性と信頼性を考慮すると、一般的にはCentOS、Ubuntu、DebianなどのLinuxディストリビューションでのデプロイを推奨しています。

## EMQXの推奨デプロイメントプランは何ですか？

EMQXはクラスターでのデプロイを推奨しており、クラスターのフロントエンドにロードバランサー（Nginx、HAProxyなど）を配置して、クライアントの接続を各ノードに均等に分散させる構成を推奨しています。

通信のセキュリティ要件が高いユーザーには、クライアントとのTLS接続を有効にし、TLS接続をロードバランサー側で終端することを推奨します。つまり、クライアントとロードバランサー間はTLS暗号化通信を使用し、ロードバランサーとEMQXノード間はTCP通信を使用します。

EMQXノードはポートをパブリックネットワークに公開しないため、全体のセキュリティは低下しませんが、TLSのオフロードによりEMQXのリソース消費を効果的に節約できます。

## デバイス数やメッセージスループットが少ない場合でもクラスターをデプロイする必要がありますか？

デバイス数が少なくメッセージスループットが低い場合でも、本番環境ではクラスターをデプロイすることに意味があります。

クラスターはシステムの可用性を向上させ、単一障害点の発生を減らします。たとえノードがダウンしても、クラスター内の他の正常なノードがサービスを継続提供できるため、業務に影響を与えません。

## EMQXが起動しない場合のトラブルシューティング方法は？

EMQXが起動しない場合は、[ログディレクトリ](../deploy/install.md#files-and-directories)内の`emqx.log.N`または`erlang.log.N`を確認し、詳細なエラー情報を取得してください。

または、`emqx console`コマンドでコンソールからEMQXを起動すると、エラーログが直接コンソールに出力されます。ログ内容に基づいて本ページの対応策を参照するか、[GitHub](https://github.com/emqx/emqx/issues)に投稿してサポートを受けてください。

## EMQXが「logger: command not found」というログメッセージで起動に失敗する場合

以下の依存関係をインストールしてください。

- `CentOS/Redhat`

```
$ yum install rsyslog
```

- `Ubuntu/Debian`

```
$ apt-get install bsdutils
```

## EMQXが「...{on_load_function_failed,crypto}...」というログメッセージで起動に失敗する場合

セキュリティ向上のため、バージョン4.3以降のEMQXはopenssl-1.1上で動作します。これにより、一部の古いLinuxディストリビューションでEMQXを実行しているユーザーに問題が発生する可能性があります。

EMQXのバージョンがv4.3.10未満、またはEMQX Enterpriseのバージョンがe4.3.5未満の場合、以下のようなエラーメッセージが表示されることがあります。

```bash
{application_start_failure,kernel,{{shutdown,{failed_to_start_child,kernel_safe_sup,{on_load_function_failed,crypto}}}, ..}
```

それ以降のバージョンでは、以下のようなエラーメッセージが表示されることがあります。

```bash
FATAL: Unable to start Erlang.
Please make sure openssl-1.1.1 (libcrypto) and libncurses are installed.
```

これは、EMQXが依存するErlang/OTPの「crypto」アプリケーションが、必要なopensslの動的ライブラリ（.so）が見つからず起動に失敗したことを示しています。対処方法は以下の通りです。

::: warning 重要なお知らせ

以下の解決策はあくまで例示です。

記載されているソースバージョンは現時点の知見に基づいて選択されていますが、古くなっていたり脆弱性を含む可能性があります。

最新のセキュリティアップデートを得るためには、OSのパッケージマネージャーから直接`libcrypto`をインストールすることを推奨します。

:::

:::: tabs

::: tab CentOS

Extra Packages for Enterprise Linux（EPEL）は、Fedora Special Interest GroupによるエンタープライズLinux向けの高品質な追加パッケージセットです。CentOS 7を例に説明します。

1. RPMリポジトリをインストールするには、`yum install epel-release`を実行します。
2. インストールに失敗した場合は、https://docs.fedoraproject.org/en-US/epel/#_el7 の手順に従いyumリポジトリが追加されていることを確認し、再度1を実行してください。
3. `yum install openssl11`を実行してopenssl-1.1をインストールします。

:::

::: tab Linux

EMQXのインストールディレクトリに移動します（パッケージ管理ツールでインストールした場合は、EMQXの`lib`と同階層のディレクトリに入ります）。

```bash
  ## パッケージインストールの場合
$ cd emqx

  ## yumなどのパッケージ管理ツールでインストールした場合は、/lib/emqxにlibディレクトリがあるはずです
$ cd /lib/emqx
```

`crypto`が依存する`.so`動的ライブラリの一覧とメモリ上の位置を確認します。

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

この中の`OPENSSL_1.1.1' not found`は、指定されたOPENSSLバージョンの.soライブラリが正しくインストールされていないことを示しています。

ソースコードからOPENSSL 1.1.1をコンパイルしてインストールし、その.soファイルをシステムが認識するパスに配置します。

```bash
## 最新の1.1.1バージョンをダウンロード
$ wget https://www.openssl.org/source/openssl-1.1.1c.tar.gz

## ct-test-haへアップロード
$ scp openssl-1.1.1c.tar.gz ct-test-ha:~/

## 解凍、コンパイル、インストール
$ tar zxf   openssl-1.1.1c.tar.gz
$ cd openssl-1.1.1c
$ ./config
$ make test   		# テスト実行。PASSが出たら続行
$ make install

## ライブラリ参照を確実にするためのシンボリックリンク作成
$ ln -s /usr/local/lib64/libssl.so.1.1 /usr/lib64/libssl.so.1.1
$ ln -s /usr/local/lib64/libcrypto.so.1.1 /usr/lib64/libcrypto.so.1.1
```

完了後、EMQXのlibレベルディレクトリで`ldd lib/crypto-*/priv/lib/crypto.so`を実行し、`.so`ライブラリが正しく認識されているか確認してください。`not found`がなければ、EMQXを正常に起動できます。

:::

::: tab macOS

EMQXのインストールディレクトリに移動します。

```bash
  ## パッケージインストールの場合
$ cd emqx

  ## brewインストールの場合
$ cd /usr/local/Cellar/emqx/<version>/
```

`crypto`が依存する`.so`動的ライブラリの一覧を確認します。

```bash
$ otool -L lib/crypto-*/priv/lib/crypto.so

lib/crypto-4.4.2.1/priv/lib/crypto.so:
  /usr/local/opt/openssl@1.1/lib/libcrypto.1.1.dylib (compatibility version 1.1.0, current version 1.1.0)
  /usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1252.200.5)
```

`otool`の出力により、OPENSSLが指定ディレクトリに正しくインストールされていることが分かります。

```bash
$ ls /usr/local/opt/openssl@1.1/lib/libcrypto.1.1.dylib
ls: /usr/local/opt/openssl@1.1/lib/libcrypto.1.1.dylib: No such file or directory
```

ファイルが存在しない場合は、`otool`で表示されたバージョンのOPENSSLをインストールする必要があります。ここでは`openssl@1.1`が表示されている例です。

```bash
$ brew install openssl@1.1
```

インストール完了後、EMQXを正常に起動できます。

## EMQXが「libatomic.so.1: cannot open shared object file: No such file or directory」というログメッセージで起動に失敗する場合

このエラーは、システムに依存関係であるlibatomicが不足していることが原因です。以下のコマンドでlibatomicをインストールしてください。

```
# Rocky Linux, CentOSなど
yum install -y libatomic
# Debian, Ubuntuなど
apt install -y libatomic
```

RPMやDEBパッケージを手動でインストールする場合、以下のような依存関係エラーが発生することがあります。

```
$ rpm -ivh emqx-5.7.0-el8-amd64.rpm
error: Failed dependencies:
libatomic is needed by emqx-5.7.0-el8-amd64.rpm
```

この場合も、まずlibatomicを手動でインストールしてください。

もちろん、最も推奨されるインストール方法はパッケージマネージャー（yum、aptなど）を使用することで、必要な依存関係が自動的にインストールされます。

:::

::::

## DockerでEMQXを起動した際に「Permission denied」というログが出て起動に失敗する場合

EMQXのデータを永続化するためにディレクトリをマウントして起動する場合：

```
sudo docker run -d --name emqx -p 18083:18083 -p 1883:1883 -v /emqx/data:/opt/emqx/data -v /emqx/log:/opt/emqx/log emqx:latest
```

以下のようなエラーでコンテナ起動に失敗することがあります。

```
mkdir: cannot create directory '/opt/emqx/data/configs': Permission denied
```

これは、コンテナ内のEMQXがLinuxユーザー`emqx`として動作しているのに対し、ホスト側のディレクトリが`root`ユーザーで作成されているため、EMQXがディレクトリやファイルを作成できないことが原因です。

解決策としては、ホスト側に`emqx`ユーザーを作成し、そのユーザーでマウントするディレクトリを作成するか、作成済みのデータ・ログディレクトリの権限を777に変更してください。

もちろん、最も推奨されるEMQXデータ永続化の方法は、名前付きデータボリュームを使用することで、権限問題を気にせずに済みます。

```
sudo docker volume create --name emqx-data
sudo docker volume create --name emqx-log
sudo docker run -d --name emqx -p 18083:18083 -p 1883:1883 -v emqx-data:/opt/emqx/data -v emqx-log:/opt/emqx/log emqx:latest
```

## EMQX起動時に「ポートが使用中（eaddrinuse）」と表示された場合はどうすればよいですか？

EMQXは起動時にデフォルトで7つのポートを使用します。これらは以下の通りです。

1. ポート1883：TCPによるMQTTリスナー。設定で変更可能。
2. ポート8883：SSL/TLSによるMQTTリスナー。設定で変更可能。
3. ポート8083：WebSocketによるMQTTリスナー。設定で変更可能。
4. ポート8084：SSL/TLS対応WebSocket（WSS）によるMQTTリスナー。設定で変更可能。
5. ポート18083：HTTP APIサービスのデフォルトリスニングポート。ダッシュボードもこのポートを使用。設定で変更可能。
6. ポート4370：EMQX分散クラスターのリモート関数呼び出しおよびMnesiaデータ同期に使用。クラスター未形成でもデフォルトで占有。リスニングポートは`BasePort (4370) + Offset`で決まり、4370は固定で変更不可。Offsetはノード名の数値サフィックス（`Name@Host`）で決定。数値サフィックスがなければ0。例：`emqx@127.0.0.1`のOffsetは0、`emqx1@127.0.0.1`のOffsetは1。
7. ポート5370：クラスターRPCポートで負荷分散に使用。主にノード間のMQTTメッセージ転送に利用。ポート4370と同様に、クラスター未形成でもデフォルトで占有。実際のリスニングポートは`BasePort (5370) + Offset`で決まり、5370は固定で変更不可。Offsetはノード名のName部分の数値サフィックスで決定。数値サフィックスがなければ0。

## EMQX起動時に「WARNING: Default (insecure) Erlang cookie is in use.」というログが出る理由は？

完全なWARNINGログは以下の通りです。

```
WARNING: Default (insecure) Erlang cookie is in use.
WARNING: Configure node.cookie in /usr/lib/emqx/etc/emqx.conf or override from environment variable EMQX_NODE__COOKIE
WARNING: NOTE: Use the same cookie for all nodes in the cluster.
```

同じcookieを使用するEMQXノードのみがクラスターを形成できます。cookieはクラスター通信をセキュアにするものではありませんが、意図しないノードがクラスターに接続するのを防ぎます。デフォルトではEMQXノードはcookie値`emqxsecretcookie`を使用しますが、クラスター構築時にはセキュリティ強化のためcookie値を変更することを推奨します。

2つ目の警告はcookieの変更方法を示しており、`emqx.conf`の`node.cookie`を編集するか、環境変数`EMQX_NODE__COOKIE`を設定する方法があります。

## EMQX Dockerコンテナを再起動すると、設定したルールやリソースなどのデータが消えるのはなぜですか？

EMQXのランタイムデータは`/opt/emqx/data`ディレクトリに保存されており、設定ルール、リソース、保持メッセージなどが含まれます。コンテナ再起動時にデータを保持するには、`/opt/emqx/data`ディレクトリをホストのローカルディレクトリまたはデータボリュームにマウントする必要があります。

しかし、`/opt/emqx/data`ディレクトリを正しくマウントしていても、コンテナ再起動後にデータが消える場合があります。これは、EMQXのランタイムデータが`/opt/emqx/data/mnesia/${Node Name}`ディレクトリに保存されており、コンテナ再起動時にEMQXのノード名が変わるため、新しい保存ディレクトリが作成されることが原因です。

EMQXのノード名はNameとHostで構成され、HostはデフォルトでコンテナのIPアドレスから取得されます。デフォルトのネットワーク設定では、コンテナのIPは再起動時に変わる可能性があるため、コンテナに固定IPを割り当てる必要があります。

この問題に対処するため、EMQXは環境変数`EMQX_HOST`を提供しており、ノード名のHost部分を設定できます。ただし、このHost値は他のノードから到達可能である必要があるため、ネットワークエイリアスと併用してください。以下は`EMQX_HOST`環境変数とネットワークエイリアスを指定してEMQX Dockerコンテナを起動する例です。

```
docker run -d --name emqx -p 18083:18083 -p 1883:1883 -e EMQX_HOST=alias-for-emqx --network example --network-alias alias-for-emqx --mount type=bind,source=/tmp/emqx,target=/opt/emqx/data emqx:5.8.3
```

## docker-composeで正常に起動しDashboardにアクセスできるのにコンテナがunhealthy状態になるのはなぜですか？

```bash
docker-compose ps
NAME      IMAGE                         COMMAND                  SERVICE   CREATED          STATUS                    PORTS
emqx1     emqx/emqx:latest   "/usr/bin/docker-ent…"   emqx     120 seconds ago   Up 110 seconds (unhealthy)   0.0.0.0:1883->1883/tcp, :::1883->1883/tcp, 0.0.0.0:18083->18083/tcp, :::18083->18083/tcp
```

EMQXのヘルスチェックは`./bin/emqx_ctl status`コマンドに依存しています。このコマンドが失敗すると、コンテナはunhealthy状態になります。

```yaml
healthcheck:
      test: ["CMD", "/opt/emqx/bin/emqx_ctl", "status"]
      interval: 60s
      timeout: 15s
      retries: 3
```

手動で`./bin/emqx_ctl status`を実行すると以下のようなエラーが出る場合があります。

```
emqx@docker:/opt/emqx$ emqx_ctl status
Node emqx@docker not responding to pings.
```

このエラーはコマンドがノードに接続できないことを示しています。通常、コンテナ起動時にネットワークがエイリアスを使わず、FQDN形式になっていないため、ノードが正しく特定できないことが原因です。

解決策は以下の通りです。

1. Dockerのホスト名をEMQXノード名に合わせる。
2. `docker-compose.yml`にホスト名設定を追加する。

```yaml
# xxx.yyy.zzz(docker.emqx.com)はFQDN形式である必要があります
hostname: docker.emqx.com
 environment:
      - EMQX_HOST=docker.emqx.com
```

EMQXは`data/mnesia/<node name>`ディレクトリにデータを保存するため、ノード名にIPアドレスではなくホスト名やFQDNのような固定識別子を使うことが重要です。ノード名が変わるとデータ損失のリスクがあります。

より簡単に設定するには、[EMQX Docker Compose Generator](https://docker.emqx.dev/)を利用して、本番環境向けの`docker-compose.yml`ファイルを作成することをおすすめします。
