# Deployment FAQs

## EMQXのデプロイに推奨されるオペレーティングシステムは何ですか？

EMQXはさまざまなオペレーティングシステムおよびハードウェアプラットフォームでの実行をサポートしています。エンタープライズレベルの安定性と信頼性を考慮すると、一般的にはCentOS、Ubuntu、DebianなどのLinuxディストリビューションでのデプロイを推奨しています。

## EMQXの推奨デプロイメントプランは何ですか？

EMQXはクラスターでのデプロイを推奨しており、クラスターのフロントエンドにロードバランサー（Nginx、HAProxyなど）を配置して、接続を各ノードにバランスよく分散させる構成が推奨されます。

通信のセキュリティ要件が高いユーザーには、クライアント側でTLS接続を有効にし、ロードバランサー側でTLS接続を終端することを推奨します。つまり、クライアントとロードバランサー間はTLS暗号化通信を使用し、ロードバランサーとEMQXノード間はTCP通信を使用します。

EMQXノードはポートをパブリックネットワークに公開しないため、全体のセキュリティは低下しませんが、TLSのオフロードによりEMQXのリソース消費を効果的に節約できます。

## デバイス数やメッセージスループットが少なくてもクラスターをデプロイする必要はありますか？

デバイス数が少なくメッセージスループットが低い場合でも、本番環境ではクラスターのデプロイは有効です。

クラスターはシステムの可用性を向上させ、単一障害点の発生を減らします。ノードがダウンしても、クラスター内の他の正常なノードがサービスを継続して提供できるため、業務への影響を防げます。

## EMQXが起動しない場合のトラブルシューティング方法は？

EMQXが起動しない場合は、[ログディレクトリ](./deploy/install.md#files-and-directories)内の`emqx.log.N`または`erlang.log.N`を確認し、詳細なエラー情報を取得してください。

または、`emqx console`コマンドでコンソールからEMQXを起動すると、エラーログが直接コンソールに出力されます。ログ内容に基づき、本ページの対応策を参照するか、[GitHub](https://github.com/emqx/emqx/issues)でサポートを依頼してください。

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

セキュリティ向上のため、バージョン4.3以降のEMQXはopenssl-1.1上で動作します。これにより、一部の古いLinuxディストリビューションで問題が発生する可能性があります。

EMQXのv4.3.10未満およびEMQX Enterpriseのe4.3.5未満のバージョンでは、以下のようなエラーが表示されることがあります。

```bash
{application_start_failure,kernel,{{shutdown,{failed_to_start_child,kernel_safe_sup,{on_load_function_failed,crypto}}}, ..}
```

それ以降のバージョンでは、以下のようなエラーが表示されます。

```bash
FATAL: Unable to start Erlang.
Please make sure openssl-1.1.1 (libcrypto) and libncurses are installed.
```

これは、EMQXが依存するErlang/OTPの「crypto」アプリケーションが、必要なopensslの動的ライブラリ（.so）が見つからず起動に失敗したことを示しています。対処法は以下の通りです。

::: warning 重要なお知らせ

以下の解決策はあくまで例示です。

記載されているソースバージョンは現時点の知見に基づいて選定していますが、古くなっていたり脆弱性を含む可能性があります。

最新のセキュリティアップデートを得るためには、OSのパッケージマネージャーから`libcrypto`を直接インストールすることを推奨します。

:::

:::: tabs

::: tab CentOS

Extra Packages for Enterprise Linux（EPEL）は、FedoraのSpecial Interest Groupであり、Enterprise Linux向けの高品質な追加パッケージ群を作成・管理しています。CentOS 7を例に説明します。

1. RPMリポジトリをインストールするには、`yum install epel-release`を実行します。
2. インストールに失敗した場合は、https://docs.fedoraproject.org/en-US/epel の手順に従いyumリポジトリを追加し、再度1を実行してください。
3. `yum install openssl11`を実行してopenssl-1.1をインストールします。

:::

::: tab Linux

EMQXのインストールディレクトリに移動します（パッケージ管理ツールでインストールした場合は、EMQXの`lib`と同階層のディレクトリに入ります）。

```bash
  ## パッケージインストールの場合
$ cd emqx

  ## yumなどのパッケージマネージャーでインストールした場合、libディレクトリは通常 /lib/emqx にあります
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

`OPENSSL_1.1.1' not found` は指定されたOPENSSLバージョンの.soライブラリが正しくインストールされていないことを示します。

ソースコードからOPENSSL 1.1.1をコンパイル・インストールし、システムが認識するパスに.soファイルを配置してください。

```bash
## 最新の1.1.1バージョンをダウンロード
$ wget https://www.openssl.org/source/openssl-1.1.1c.tar.gz

## ct-test-haへアップロード
$ scp openssl-1.1.1c.tar.gz ct-test-ha:~/

## 解凍、コンパイル、インストール
$ tar zxf   openssl-1.1.1c.tar.gz
$ cd openssl-1.1.1c
$ ./config
$ make test   		# テストを実行し、PASSが出たら続行
$ make install

## ライブラリ参照の確認
$ ln -s /usr/local/lib64/libssl.so.1.1 /usr/lib64/libssl.so.1.1
$ ln -s /usr/local/lib64/libcrypto.so.1.1 /usr/lib64/libcrypto.so.1.1
```

完了後、EMQXのlibディレクトリ階層で`ldd lib/crypto-*/priv/lib/crypto.so`を実行し、`.so`ライブラリが正しく認識されているか確認してください。`not found`がなければ正常に起動可能です。

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

以下のコマンドで、OPENSSLが指定ディレクトリに正しくインストールされているか確認します。

```bash
$ ls /usr/local/opt/openssl@1.1/lib/libcrypto.1.1.dylib
ls: /usr/local/opt/openssl@1.1/lib/libcrypto.1.1.dylib: No such file or directory
```

ファイルが存在しない場合は、`otool`で表示されたバージョンのOPENSSLをインストールしてください。ここでは`openssl@1.1`です。

```bash
$ brew install openssl@1.1
```

インストール完了後、EMQXを通常通り起動できます。

## EMQXが「libatomic.so.1: cannot open shared object file: No such file or directory」というログメッセージで起動に失敗する場合

このエラーはシステムにlibatomic依存関係が不足しているため発生します。以下のコマンドで依存関係をインストールしてください。

```
# Rocky Linux, CentOSなど
yum install -y libatomic
# Debian, Ubuntuなど
apt install -y libatomic
```

RPMまたはDEBパッケージを手動でインストールする場合、以下のような依存関係エラーが発生することがあります。

```
$ rpm -ivh emqx-5.7.0-el8-amd64.rpm
error: Failed dependencies:
libatomic is needed by emqx-5.7.0-el8-amd64.rpm
```

この場合も、まずlibatomicを手動でインストールしてください。

なお、最も推奨されるインストール方法はパッケージマネージャー（yum、aptなど）を使用することで、依存関係は自動的にインストールされます。

:::

::::

## DockerでEMQXを起動した際に「Permission denied」というログが出て起動に失敗する場合

EMQXのデータ永続化のためにディレクトリをマウントして起動する際、

```
sudo docker run -d --name emqx -p 18083:18083 -p 1883:1883 -v /emqx/data:/opt/emqx/data -v /emqx/log:/opt/emqx/log emqx:latest
```

以下のようなエラーでコンテナが起動しないことがあります。

```
mkdir: cannot create directory '/opt/emqx/data/configs': Permission denied
```

これは、コンテナ内のEMQXがLinuxユーザー`emqx`として動作しているのに対し、ホスト側のディレクトリが`root`ユーザーで作成されているため、EMQXがディレクトリやファイルを作成できないためです。

解決策として、ホスト側に`emqx`ユーザーを作成し、そのユーザーでマウントするディレクトリを作成するか、作成済みのデータ・ログディレクトリのパーミッションを777に変更してください。

ただし、最も推奨されるデータ永続化方法は名前付きデータボリュームを使用することで、パーミッション問題を気にせずに済みます。

```
sudo docker volume create --name emqx-data
sudo docker volume create --name emqx-log
sudo docker run -d --name emqx -p 18083:18083 -p 1883:1883 -v emqx-data:/opt/emqx/data -v emqx-log:/opt/emqx/log emqx:latest
```

## EMQX起動時に「ポートが使用中（eaddrinuse）」と表示された場合の対処法は？

EMQXは起動時にデフォルトで7つのポートを使用します。これらは以下の通りです。

1. ポート1883：TCPによるMQTTリスナー。設定で変更可能。
2. ポート8883：SSL/TLSによるMQTTリスナー。設定で変更可能。
3. ポート8083：WebSocketによるMQTTリスナー。設定で変更可能。
4. ポート8084：SSL対応WebSocket（WSS）によるMQTTリスナー。設定で変更可能。
5. ポート18083：HTTP APIサービスのデフォルトリスニングポート。ダッシュボードもこのポートを使用。設定で変更可能。
6. ポート4370：EMQX分散クラスターのリモート関数呼び出しおよびMnesiaデータ同期に使用。クラスター未形成でもデフォルトで占有。リスニングポートは`BasePort(4370) + Offset`で決まり、4370は固定で変更不可。Offsetはノード名（`Name@Host`）の数値サフィックスにより決定。サフィックスがなければ0。例：`emqx@127.0.0.1`のOffsetは0、`emqx1@127.0.0.1`のOffsetは1。
7. ポート5370：クラスターRPCポートで負荷分散に使用。主にノード間のMQTTメッセージ転送に利用。ポート4370と同様にクラスター未形成でもデフォルトで占有。実際のリスニングポートは`BasePort(5370) + Offset`で決まり、5370は固定で変更不可。Offsetはノード名のName部分の数値サフィックスにより決定。サフィックスがなければ0。

## EMQX起動時に「WARNING: Default (insecure) Erlang cookie is in use.」とログに出る理由は？

警告ログ全文は以下の通りです。

```
WARNING: Default (insecure) Erlang cookie is in use.
WARNING: Configure node.cookie in /usr/lib/emqx/etc/emqx.conf or override from environment variable EMQX_NODE__COOKIE
WARNING: NOTE: Use the same cookie for all nodes in the cluster.
```

同じcookieを使用するEMQXノードのみがクラスターを形成できます。cookieはクラスター通信のセキュリティを保証するものではありませんが、意図しないノードの接続を防止します。デフォルトではEMQXノードはすべて`emqxsecretcookie`というcookie値を使用していますが、クラスター構築時にはセキュリティ強化のためcookie値の変更を推奨します。

2つ目の警告はcookieの設定方法を示しており、`emqx.conf`の`node.cookie`に設定するか、環境変数`EMQX_NODE__COOKIE`で上書きします。EMQX 6.3.0以降は両者とも`file://`を受け付けるため、cookie値を設定ファイルや環境変数に直接記載する必要はありません。詳細は[Load the Node Cookie from a File](../guides/configuration/secret-from-file.md#load-the-node-cookie-from-a-file)を参照してください。

## EMQX Dockerコンテナを再起動すると、設定したルールやリソースなどのデータが消失する理由は？

EMQXのランタイムデータは`/opt/emqx/data`ディレクトリに保存されており、設定ルールやリソース、保持メッセージなどが含まれます。コンテナ再起動時にデータを保持するには、`/opt/emqx/data`ディレクトリをホストのディレクトリまたはデータボリュームにマウントする必要があります。

しかし、`/opt/emqx/data`を正しくマウントしていても、コンテナ再起動後にデータ消失が発生することがあります。これは、EMQXのランタイムデータが`/opt/emqx/data/mnesia/${Node Name}`ディレクトリに保存されており、コンテナ再起動時にEMQXのノード名が変わるため、新たなストレージディレクトリが作成されるためです。

EMQXのノード名はNameとHostで構成され、HostはデフォルトでコンテナのIPアドレスから取得されます。デフォルトのネットワーク設定ではコンテナのIPが再起動時に変わるため、固定IPを維持する必要があります。

この問題に対処するため、EMQXは環境変数`EMQX_HOST`を提供しており、ノード名のHost部分を設定可能です。ただし、このHost値は他のノードから到達可能である必要があるため、ネットワークエイリアスと併用してください。以下は`EMQX_HOST`環境変数とネットワークエイリアスを指定してEMQX Dockerコンテナを起動する例です。

```
docker run -d --name emqx -p 18083:18083 -p 1883:1883 -e EMQX_HOST=alias-for-emqx --network example --network-alias alias-for-emqx --mount type=bind,source=/tmp/emqx,target=/opt/emqx/data emqx:5.8.3
```

## `docker-compose`で起動し、Dashboardにアクセス可能でもコンテナがunhealthy状態になる理由は？

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

このエラーはコマンドがノードに接続できないことを示します。原因は、コンテナ起動時にネットワークがエイリアスを使用せず、FQDN形式でないためノードを正しく特定できないことが多いです。

解決策は以下の通りです。

1. Dockerのホスト名をEMQXノード名に合わせる。
2. `docker-compose.yml`にホスト名設定を追加する。

```yaml
# xxx.yyy.zzz(docker.emqx.com)はFQDN形式である必要があります
hostname: docker.emqx.com
 environment:
      - EMQX_HOST=docker.emqx.com
```

EMQXは`data/mnesia/<node name>`にデータを保存するため、ノード名が変わるとデータが失われます。ノード名にはIPアドレスではなく、FQDNの安定した名前を使用してください。EMQXはErlangノードをlong-nameモードで動作させているため、ドットなしの短いホスト名は使用できません。

より簡単に設定するために、[EMQX Docker Compose Generator](https://docker.emqx.dev/)を利用して本番環境向けの`docker-compose.yml`ファイルを作成することを検討してください。
