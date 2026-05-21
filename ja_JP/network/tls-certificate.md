# SSL/TLS証明書の取得

SSL/TLS証明書は以下の2つの方法で取得できます。

1. 自己署名証明書：自身で発行する証明書を使用することを指します。ただし、自己署名証明書は多くのセキュリティリスクがあり、テストや検証環境での利用のみ推奨されます。
<<<<<<< HEAD
2. 証明書の申請または購入：無料証明書は[Let's Encrypt](https://letsencrypt.org)やHuawei Cloud、Tencent Cloudなどのクラウドベンダーから申請できます。また、有料証明書は[DigiCert](https://www.digicert.com/)などの組織から購入可能です。企業ユーザーの場合、より高いセキュリティ保護を得るために、有料のOV以上の証明書の申請を一般的に推奨します。
=======
2. 証明書の申請または購入：[Let's Encrypt](https://letsencrypt.org)やHuawei Cloud、Tencent Cloudなどのクラウドベンダーから無料の証明書を申請するか、[DigiCert](https://www.digicert.com/)などの組織から有料の証明書を購入できます。企業ユーザーの場合、より高いセキュリティ保護を得るために、有料のOV以上の証明書を申請することが一般的に推奨されます。
>>>>>>> origin/release-5.10

本ページでは、自己署名の認証局（CA）証明書を作成し、サーバーおよびクライアント証明書を発行する方法を紹介します。

## セルフサインCA証明書の作成

::: tip 前提条件

[OpenSSL](https://www.openssl.org/)がインストールされていること。

:::

<<<<<<< HEAD
1. 以下のコマンドを実行して鍵ペアを生成します。コマンド実行時に鍵を保護するためのパスワードの入力を求められます。このパスワードは証明書の生成、発行、検証時に必要となるため、鍵とパスワードは安全に保管してください。
=======
1. 以下のコマンドを実行して鍵ペアを生成します。コマンド実行時に鍵を保護するためのパスワードの入力を求められます。このパスワードは証明書の生成、発行、検証時に必要となるため、鍵とともに安全に保管してください。
>>>>>>> origin/release-5.10

   ```bash
   openssl genrsa -des3 -out rootCA.key 2048
   ```

<<<<<<< HEAD
2. 次に、鍵ペアの秘密鍵を使ってCA証明書を生成します。コマンド実行時に証明書の識別名（Distinguished Name, DN）を設定するよう求められます。
=======
2. 次に、鍵ペアの秘密鍵を使ってCA証明書を生成します。コマンド実行時に証明書の識別名（DN）の設定を求められます。
>>>>>>> origin/release-5.10

   ```bash
   openssl req -x509 -new -nodes -key rootCA.key -sha256 -days 3650 -out rootCA.crt
   ```

## サーバー証明書の発行

<<<<<<< HEAD
先ほど作成したCA証明書を使って、サーバー証明書を発行します。サーバー証明書はサーバー所有者の身元を検証するために使用され、通常はホスト名、サーバー名、またはドメイン名（例：[www.emqx.com](https://www.emqx.com/en)）に発行されます。サーバー証明書の生成には、CA鍵（rootCA.key）、CA証明書（rootCA.crt）、およびサーバーの証明書署名要求（CSR）（server.csr）が必要です。
=======
先ほど生成したCA証明書を使って、サーバー証明書を発行します。サーバー証明書はサーバー所有者の身元を検証するために使用され、通常はホスト名、サーバー名、またはドメイン名（例：[www.emqx.com](https://www.emqx.com/en)）に発行されます。サーバー証明書の生成には、CAの秘密鍵（rootCA.key）、CA証明書（rootCA.crt）、およびサーバーの証明書署名要求（CSR）（server.csr）が必要です。
>>>>>>> origin/release-5.10

1. サーバー証明書用の鍵ペアを生成するため、以下のコマンドを実行します。

   ```bash
   openssl genrsa -out server.key 2048
   ```

<<<<<<< HEAD
2. サーバー鍵ペアを使ってCSRを作成します。CSRはCAのルート証明書の秘密鍵で署名されることで、証明書の公開鍵ファイルが生成されユーザーに発行されます。コマンド実行時に証明書の識別名（DN）を設定するよう求められます。
=======
2. サーバー鍵ペアを使ってCSRを作成します。CSRはCAのルート証明書の秘密鍵で署名されることで、証明書の公開鍵ファイルが生成され、ユーザーに発行されます。このコマンド実行時にも識別名（DN）の設定を求められます。
>>>>>>> origin/release-5.10

   ```bash
   openssl req -new -key server.key -out server.csr
   ```

<<<<<<< HEAD
   以下の情報入力を求められます。各項目の意味は以下の通りです。
=======
   以下のような情報の入力を求められます。各項目の意味は次の通りです。
>>>>>>> origin/release-5.10

   ```bash
   You are about to be asked to enter information that will be incorporated
   into your certificate request.
   What you are about to enter is what is called a Distinguished Name or a DN.
   There are quite a few fields but you can leave some blank
   For some fields there will be a default value,
   If you enter '.', the field will be left blank.
   -----
<<<<<<< HEAD
   Country Name (2 letter code) [AU]: # 国/地域
   State or Province Name (full name) [Some-State]: # 州/県名
   Locality Name (eg, city) []: # 市区町村名
   Organization Name (eg, company) [Internet Widgets Pty Ltd]: # 組織名（会社名）、例：EMQ
   Organizational Unit Name (eg, section) []: # 部署名、例：EMQX
   Common Name (e.g. server FQDN or YOUR name) []: # サーバーの完全修飾ドメイン名（FQDN）、例：mqtt.emqx.com
=======
   Country Name (2 letter code) [AU]: # 国・地域
   State or Province Name (full name) [Some-State]: # 州または県名
   Locality Name (eg, city) []: # 市区町村名
   Organization Name (eg, company) [Internet Widgets Pty Ltd]: # 組織名（会社名）、例：EMQ
   Organizational Unit Name (eg, section) []: # 部署名、例：EMQX
   Common Name (e.g. server FQDN or YOUR name) []: # 証明書を使用するサーバーの完全修飾ドメイン名（FQDN）、例：mqtt.emqx.com
>>>>>>> origin/release-5.10
   ...
   ```

3. サーバーCSRを使ってサーバー証明書を生成し、有効期限を指定します。ここでは365日に設定しています。

   ```bash
   openssl x509 -req -in server.csr -CA rootCA.crt -CAkey rootCA.key -CAcreateserial -out server.crt -days 365
   ```

<<<<<<< HEAD
   これで証明書一式が生成されます。
=======
   これで以下の証明書一式が作成されます。
>>>>>>> origin/release-5.10

   ```bash
   .
   ├── rootCA.crt
   ├── rootCA.key
   ├── rootCA.srl
   ├── server.crt
   ├── server.csr
   └── server.key
   ```

## クライアント証明書の発行

<<<<<<< HEAD
クライアント証明書の発行手順はサーバー証明書の発行とほぼ同様です。唯一の違いはCSR作成時に、Common Nameをクライアントのユーザー名やクライアントIDなどの一意の識別子に設定する点です。

クライアント証明書はサーバー証明書と同じCA証明書で署名されるため、上記のCA証明書でクライアント証明書も署名可能です。
=======
クライアント証明書の発行手順はサーバー証明書の発行とほぼ同じです。唯一の違いは、CSR作成時にCommon Nameをクライアントのユーザー名やクライアントIDなどの一意の識別子に設定する点です。

クライアント証明書もサーバー証明書と同じCA証明書で署名されます。そのため、上記のCA証明書を使ってクライアント証明書を署名できます。
>>>>>>> origin/release-5.10

## 次のステップ

SSL/TLS証明書を取得したら、クライアントのSSL/TLS接続を有効化できます。また、証明書の有効期限が切れた際には更新も可能です。

- [SSL/TLS接続の有効化](./emqx-mqtt-tls.md)
- [証明書の更新](./emqx-mqtt-tls.md#update-ssl-tls-certificates)
