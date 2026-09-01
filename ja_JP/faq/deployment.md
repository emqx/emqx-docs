# デプロイメントFAQ

## EMQXのデプロイに推奨されるオペレーティングシステムは何ですか？

EMQXはさまざまなオペレーティングシステムやハードウェアプラットフォームで動作します。エンタープライズレベルの安定性と信頼性を考慮すると、一般的にはCentOS、Ubuntu、DebianなどのLinuxディストリビューションでのデプロイを推奨しています。

## EMQXの推奨デプロイメントプランは何ですか？

EMQXはクラスターでのデプロイを推奨しており、クラスターのフロントエンドにロードバランサー（Nginx、HAProxyなど）を配置して、接続を各ノードに均等に振り分ける構成が望ましいです。

通信のセキュリティ要件が高いユーザーには、クライアント側でTLS接続を有効にし、ロードバランサー側でTLS接続を終端することを推奨します。つまり、クライアントとロードバランサー間はTLS暗号化通信を使用し、ロードバランサーとEMQXノード間はTCP通信を使用します。

EMQXノードはポートをパブリックネットワークに公開しないため、全体のセキュリティは低下しませんが、TLSのオフロードによりEMQXのリソース消費を効果的に節約できます。

## デバイス数やメッセージスループットが多くない場合でもクラスターをデプロイする必要はありますか？

デバイス数が少なくメッセージスループットが低い場合でも、本番環境ではクラスターをデプロイすることに意味があります。

クラスターはシステムの可用性を向上させ、単一障害点の発生を減らします。ノードがダウンしても、クラスター内の他の正常なノードがサービスを継続提供できるため、業務への影響を防げます。

## EMQXが起動しない場合のトラブルシューティング方法は？

EMQXが起動しない場合は、[ログディレクトリ](../deploy/install.md#files-and-directories)内の`emqx.log.N`や`erlang.log.N`を確認し、詳細なエラー内容を取得してください。

または、`emqx console`コマンドでコンソールからEMQXを起動すると、エラーログが直接コンソールに出力されます。ログ内容に基づき本ページの対応策を参照するか、[GitHub](https://github.com/emqx/emqx/issues)に投稿してサポートを受けてください。

## EMQX起動時に「logger: command not found」というログが出る場合の対処法は？

以下の依存パッケージをインストールしてください。

- `CentOS/Redhat`

```
$ yum install rsyslog
```

- `Ubuntu/Debian`

```
$ apt-get install bsdutils
```

## EMQX起動時に「...{on_load_function_failed,crypto}...」というログが出る場合の対処法は？

セキュリティ向上のため、バージョン4.3以降のEMQXはopenssl-1.1上で動作します。これにより、一部の古いLinuxディストリビューションで問題が発生する場合があります。

EMQXバージョン4.3.10未満およびEMQX Enterpriseバージョンe4.3.5未満では、以下のようなエラーメッセージが表示されることがあります。

```bash
{application_start_failure,kernel,{{shutdown,{failed_to_start_child,kernel_safe_sup,{on_load_function_failed,crypto}}}, ..}
```

それ以降のバージョンでは、以下のようなエラーメッセージが表示されることがあります。

```bash
FATAL: Unable to start Erlang.
Please make sure openssl-1.1.1 (libcrypto) and libncurses are installed.
```

これは、EMQXが依存するErlang/OTPの"crypto"アプリケーションが、必要なopenssl動的ライブラリ（.so）が見つからず起動に失敗したことを示しています。対処方法は以下の通りです。

::: warning 重要なお知らせ

以下の解決策はあくまで一例です。

記載のソースバージョンは現時点の知見に基づいて選択していますが、古くなっていたり脆弱性が存在する可能性があります。

最新のセキュリティアップデートを得るためには、OSのパッケージマネージャーから`libcrypto`を直接インストールすることを推奨します。

:::

:::: tabs

::: tab CentOS

Extra Packages for Enterprise Linux（EPEL）はFedoraのSpecial Interest Groupで、Enterprise Linux向けの高品質な追加パッケージを管理しています。CentOS 7を例に説明します。

1. RPMリポジトリをインストールするには、`yum install epel-release`を実行します。
2. インストールに失敗した場合は、https://docs.fedoraproject.org/en-US/epel の指示に従いyumリポジトリを追加し、再度1を試してください。
3. `yum install openssl11`を実行してopenssl-1.1をインストールします。

:::

::: tab Linux

EMQXのインストールディレクトリに移動します（パッケージ管理ツールでインストールした場合は、EMQXの`lib`と同じ階層のディレクトリに入ります）。

```bash
  ## パッケージインストールの場合
$ cd emqx

  ## yumなどのパッケージマネージャーインストールの場合、libディレクトリは通常 /lib/emqx にあります
$ cd /lib/emqx
```

`crypto`が依存する`.so`動的ライブラリのリストとメモリ上の場所を確認します。

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

`OPENSSL_1.1.1' not found`は指定されたOPENSSLバージョンの.soライブラリが正しくインストールされていないことを示します。

OPENSSL 1.1.1をソースからコンパイル・インストールし、システムが認識するパスに.soファイルを配置してください。

```bash
## 最新の1.1.1バージョンをダウンロード
$ wget https://www.openssl.org/source/openssl-1.1.1c.tar.gz

## ct-test-haへアップロード
$ scp openssl-1.1.1c.tar.gz ct-test-ha:~/

## 解凍、コンパイル、インストール
$ tar zxf openssl-1.1.1c.tar.gz
$ cd openssl-1.1.1c
$ ./config
$ make test   		# テストを実行し、PASSが出たら続行
$ make install

## ライブラリ参照を確実にするためのシンボリックリンク作成
$ ln -s /usr/local/lib64/libssl.so.1.1 /usr/lib64/libssl.so.1.1
$ ln -s /usr/local/lib64/libcrypto.so.1.1 /usr/lib64/libcrypto.so.1.1
```

完了後、EMQXのlib階層ディレクトリで`ldd lib/crypto-*/priv/lib/crypto.so`を実行し、`.so`ライブラリが`not found`なしで認識されているか確認してください。問題なければEMQXを正常に起動できます。

:::

::: tab macOS

EMQXのインストールディレクトリに移動します。

```bash
  ## パッケージインストールの場合
$ cd emqx

  ## brewインストールの場合
$ cd /usr/local/Cellar/emqx/<version>/
```

`crypto`が依存する`.so`動的ライブラリのリストを確認します。

```bash
$ otool -L lib/crypto-*/priv/lib/crypto.so

lib/crypto-4.4.2.1/priv/lib/crypto.so:
  /usr/local/opt/openssl@1.1/lib/libcrypto.1.1.dylib (compatibility version 1.1.0, current version 1.1.0)
  /usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1252.200.5)
```

以下のコマンドで指定ディレクトリにOPENSSLが正しくインストールされているか確認します。

```bash
$ ls /usr/local/opt/openssl@1.1/lib/libcrypto.1.1.dylib
ls: /usr/local/opt/openssl@1.1/lib/libcrypto.1.1.dylib: No such file or directory
```

ファイルが存在しない場合は、`otool`で表示されたバージョンのOPENSSLをインストールしてください。ここでは`openssl@1.1`です。

```bash
$ brew install openssl@1.1
```

インストール完了後、EMQXを正常に起動できます。

:::

::::

## EMQX起動時に「libatomic.so.1: cannot open shared object file: No such file or directory」というログが出る場合の対処法

このエラーはシステムにlibatomic依存パッケージが不足しているため発生します。以下のコマンドで依存パッケージをインストールしてください。

```
# Rocky Linux, CentOSなど
yum install -y libatomic
# Debian, Ubuntuなど
apt install -y libatomic
```

RPMやDEBパッケージを手動でインストールする場合、以下のような依存関係エラーが出ることがあります。

```
$ rpm -ivh emqx-5.7.0-el8-amd64.rpm
error: Failed dependencies:
libatomic is needed by emqx-5.7.0-el8-amd64.rpm
```

この場合も、まず手動でlibatomicをインストールしてください。

なお、最も推奨されるインストール方法はパッケージマネージャー（yum、aptなど）を使用することで、必要な依存関係が自動的にインストールされます。

:::

::::

## DockerでEMQXを起動した際に「Permission denied」というログが出て起動に失敗する場合の対処法

EMQXのデータを永続化するためにディレクトリをマウントして起動する場合、

```
sudo docker run -d --name emqx -p 18083:18083 -p 1883:1883 -v /emqx/data:/opt/emqx/data -v /emqx/log:/opt/emqx/log emqx:latest
```

以下のようなエラーでコンテナ起動に失敗することがあります。

```
mkdir: cannot create directory '/opt/emqx/data/configs': Permission denied
```

これはEMQXがコンテナ内でLinuxユーザー`emqx`として動作している一方、ホスト側のディレクトリが`root`ユーザーで作成されているため、EMQXがディレクトリやファイルを作成できないことが原因です。

解決策として、ホスト側に`emqx`ユーザーを作成し、そのユーザーでマウントするディレクトリを作成するか、作成済みのデータ・ログディレクトリの権限を777に変更してください。

ただし、最も推奨されるデータ永続化方法は名前付きデータボリュームを使用することで、権限問題を気にせずに済みます。

```
sudo docker volume create --name emqx-data
sudo docker volume create --name emqx-log
sudo docker run -d --name emqx -p 18083:18083 -p 1883:1883 -v emqx-data:/opt/emqx/data -v emqx-log:/opt/emqx/log emqx:latest
```

## EMQX起動時に「ポートが使用中（eaddrinuse）」と表示された場合の対処法は？

EMQXは起動時にデフォルトで7つのポートを使用します。これらは以下の通りです。

1. ポート1883：MQTTのTCPリスナー用。設定で変更可能。
2. ポート8883：MQTTのSSL/TLSリスナー用。設定で変更可能。
3. ポート8083：MQTTのWebSocketリスナー用。設定で変更可能。
4. ポート8084：MQTTのWSS（WebSocket over SSL）リスナー用。設定で変更可能。
5. ポート18083：HTTP APIサービスのデフォルトリスニングポート。ダッシュボードもこのポートを使用。設定で変更可能。
6. ポート4370：EMQX分散クラスターのリモート関数呼び出しおよびMnesiaデータ同期用。クラスター未形成時でもデフォルトで占有。リスニングポートは`BasePort(4370) + Offset`で決まり、4370は固定で変更不可。Offsetはノード名の数値サフィックス（`Name@Host`）で決まる。数値サフィックスがない場合は0。例：`emqx@127.0.0.1`のOffsetは0、`emqx1@127.0.0.1`のOffsetは1。
7. ポート5370：クラスターRPCポートで負荷分散用。主にノード間のMQTTメッセージ転送に使用。ポート4370と同様にクラスター未形成時でも占有。実際のリスニングポートは`BasePort(5370) + Offset`で決まり、5370は固定で変更不可。Offsetはノード名のName部分の数値サフィックスで決まる。数値サフィックスがない場合は0。

## EMQX起動時に「WARNING: Default (insecure) Erlang cookie is in use.」というログが出る理由は？

警告ログ全文は以下の通りです。

```
WARNING: Default (insecure) Erlang cookie is in use.
WARNING: Configure node.cookie in /usr/lib/emqx/etc/emqx.conf or override from environment variable EMQX_NODE__COOKIE
WARNING: NOTE: Use the same cookie for all nodes in the cluster.
```

同じcookieを使用するEMQXノードのみがクラスターを形成できます。cookieはクラスター通信を保護するものではありませんが、意図しないクラスターへの接続を防止します。デフォルトではEMQXノードは統一して`emqxsecretcookie`というcookie値を使用していますが、クラスター構築時にはセキュリティ向上のためcookie値を変更することを推奨します。

2つ目の警告はcookieの変更方法を示しており、`emqx.conf`の`node.cookie`を編集するか、環境変数`EMQX_NODE__COOKIE`を設定する方法があります。

## EMQX Dockerコンテナを再起動すると、設定したルールやリソースなどのデータが消失する理由は？

EMQXのランタイムデータは`/opt/emqx/data`ディレクトリに保存されており、設定ルール、リソース、保持メッセージなどが含まれます。コンテナ再起動時にデータを永続化するには、`/opt/emqx/data`ディレクトリをホストのローカルディレクトリやデータボリュームにマウントする必要があります。

しかし、`/opt/emqx/data`を正しくマウントしていても、コンテナ再起動後にデータが消失することがあります。これはEMQXのランタイムデータが`/opt/emqx/data/mnesia/${Node Name}`ディレクトリに保存されており、コンテナ再起動時にEMQXのノード名が変わるため新しい保存ディレクトリが作成されるためです。

EMQXのノード名はNameとHostで構成され、HostはデフォルトでコンテナのIPアドレスから取得されます。デフォルトのネットワーク構成ではコンテナのIPが再起動時に変わるため、固定IPを維持する必要があります。

この問題に対処するため、EMQXは環境変数`EMQX_HOST`を提供しており、ノード名のHost部分を設定できます。ただし、このHost値は他のノードから到達可能である必要があるため、ネットワークエイリアスと併用してください。以下は`EMQX_HOST`環境変数とネットワークエイリアスを指定したEMQX Dockerコンテナ起動例です。

```
docker run -d --name emqx -p 18083:18083 -p 1883:1883 -e EMQX_HOST=alias-for-emqx --network example --network-alias alias-for-emqx --mount type=bind,source=/tmp/emqx,target=/opt/emqx/data emqx:5.8.3
```

## docker-composeで起動したコンテナが正常に起動し、ダッシュボードにアクセスできるにもかかわらず、コンテナのステータスがunhealthyになる理由は？

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

手動で`./bin/emqx_ctl status`コマンドを実行すると、

```
emqx@docker:/opt/emqx$ emqx_ctl status
Node emqx@docker not responding to pings.
```

と表示され、ノードに接続できないことを示します。この問題は通常、コンテナ起動時にネットワークがエイリアスを使用せず、FQDN形式でないためにノードが正しく特定できないことが原因です。

### 解決策

1. Dockerのホスト名をEMQXノード名に合わせて変更する。
2. `docker-compose.yml`にホスト名設定を追加する。

```yaml
# xxx.yyy.zzz(docker.emqx.com)はFQDN形式である必要があります
hostname: docker.emqx.com
environment:
      - EMQX_HOST=docker.emqx.com
```

EMQXはデータを`data/mnesia/<node name>`に保存するため、ノード名が変わるとデータが失われます。ノード名はIPアドレスではなくFQDNの安定した名前を使用してください。EMQXはErlangノードをロングネームモードで動作させるため、ドットなしの短いホスト名は使用できません。

より簡単に設定したい場合は、[EMQX Docker Compose Generator](https://docker.emqx.dev/)を利用して本番環境向けの`docker-compose.yml`ファイルを作成することをおすすめします。
