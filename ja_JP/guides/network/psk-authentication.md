# PSK認証

事前共有鍵（PSK）認証は、事前に共有された鍵を用いて本人確認を行う認証方式です。PSK認証方式を使用する場合、クライアントとEMQXはセキュアな接続を確立する前に同じ鍵を事前に共有しておく必要があります。事前共有鍵は、クライアントとEMQX間のTLS接続の確立およびその後の通信において、データの暗号化および復号に使用されます。PSK認証を有効にすると、クライアントとEMQXは証明書や認証局を必要とせずに相互認証を行い、安全な接続を確立できます。

本ページでは、EMQXでPSK認証を有効にする方法を紹介します。

1. 任意のディレクトリに `data/psk_file.txt` ファイルを作成し、事前共有鍵のIDと秘密値を記述します。

   ::: tip

   秘密値は任意の文字列で構いませんが、長さは選択した暗号スイートに対応している必要があります。例えば、暗号スイートが `TLS_PSK_WITH_AES_128_CBC_SHA` の場合、秘密値は128ビットの長さでなければなりません。

   :::

   ```bash
   # 1行に1つのデータを、PSKIdentity:SharedSecretの形式で記述
   emqx_c:BA0DB2A3448345A3A13A91C2ADA44778
   emqx_a:A6FC9EDF62864125AAE7658BEAE6170C
   ```

2. 設定ファイルに `psk_authentication` 設定グループを追加します。

   ```bash
   psk_authentication {
     enable = true
     init_file = "data/psk_file.txt"
   }
   ```
   
3. 設定ファイルでSSLリスナーを設定します。`listeners.ssl.default` グループを修正し、以下のオプションを追加してください。

   - `ssl_options.versions`: PSK暗号スイートは `tlsv1.3` でサポートされないため、`tlsv1.3` のサポートを削除します。
   - `ssl_options.ciphers`: PSK暗号スイートを使用するように設定します。

   ::: tip

   `RSA-PSK` 暗号スイートを使用する場合は、`RSA` 証明書が依然として必要です。詳細は [RFC4279](https://www.rfc-editor.org/rfc/rfc4279#section-4) を参照してください。

   :::

   ```bash
   listeners.ssl.default {
     acceptors = 4
     bind = 8883
     ssl_options {
       ciphers = ["RSA-PSK-AES256-GCM-SHA384","RSA-PSK-AES256-CBC-SHA384","RSA-PSK-AES128-GCM-SHA256","RSA-PSK-AES128-CBC-SHA256","RSA-PSK-AES256-CBC-SHA","RSA-PSK-AES128-CBC-SHA"]
       versions = ["tlsv1.2", "tlsv1.1", "tlsv1"]
     }
   }
   ```
