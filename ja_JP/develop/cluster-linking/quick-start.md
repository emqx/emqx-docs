# クラスターリンク クイックスタート

このページでは、離れた2つのEMQXクラスター間でクラスターリンクを設定するためのクイックスタートガイドを提供します。

## 前提条件

異なる2つのリージョンにそれぞれEMQXクラスターをホストするコンピュートリソースがあることを確認してください。この例では、`us-east` と `eu-west` リージョンを使用し、それぞれのクラスター名を `cluster-us-east` と `cluster-eu-west` とします。

### 要件

- EMQX バージョン5.8.0以降
- 一意のクラスター名
- クラスター間のネットワーク通信

クラスターリンクでは、各クラスターのMQTTリスナーが他方のクラスターのネットワークから到達可能である必要があります。トラフィックを均等に分散するために、これらのMQTTリスナーはロードバランサーの背後に配置することを推奨します。セキュリティのために、パブリックインターネットを使用する場合は、[TLS](./configuration.md)および厳格な[TLSまたはMQTTクライアント認証](../../guides/access-control/authn/authn.md)を用いてクラスター間通信を保護してください。

本番環境に移行する前に、専用の[セキュアクラスターリンク](./security.md)ガイドに従い、各クラスターの認証、認可、および `$LINK/` トピックネームスペースを強化してください。

## 最初のクラスターのセットアップ（cluster-us-east）

最初のクラスター `cluster-us-east` を、クラスターの設定ファイルに以下の設定スニペットを用いてセットアップします。

```bash
# クラスターリンク設定
cluster {
  # このクラスターの名前
  name = "cluster-us-east"
  links = [
    {
      # 2番目のクラスターの名前
      name = "cluster-eu-west"
      # 2番目のクラスターのMQTTリスナーのエンドポイント
      server = "emqx.us-east.myinfra.net:11883"
      clientid = "clink-us-east"
      topics = ["#"]
    }
  ]
}

# クラスターリンク接続専用リスナー
listeners {
  tcp.clink {
    bind = 11883
  }
}
```

この設定は以下を指定しています：
- リモートクラスター名は `cluster-eu-west`
- クラスターは `emqx.us-east.myinfra.net:11883` でアクセス可能
- クラスターリンクMQTT接続のクライアントIDプレフィックスは `clink-us-east`
- ワイルドカードトピック `#` にマッチするすべてのメッセージがローカルクラスターに転送される
- ポート `11883` でクラスターリンク接続専用のリスナーが有効化されている

## 2番目のクラスターのセットアップ（cluster-eu-west）

2番目のクラスター `cluster-eu-west` を、クラスターの設定ファイルに以下の設定スニペットを用いてセットアップします。

```bash
# クラスターリンク設定
cluster {
  name = "cluster-eu-west"
  links = [
    {
      name = "cluster-us-east"
      server = "emqx.eu-west.myinfra.net:11883"
      clientid = "clink-eu-west"
      topics = ["#"]
    }
  ]
}

# クラスターリンク接続専用リスナー
listeners {
  tcp.clink {
    bind = 11883
  }
}
```

この設定は最初のクラスターの設定と対称的です。両方の設定が適用され、クラスターが稼働すると、2つのクラスター間に対称的な双方向クラスターリンクが確立されます。非対称リンクの作成も可能であり、後述します。

## クラスターリンクの確認

異なるクラスターに接続されたクライアントが標準のMQTTメカニズムを使って通信できることを確認するために、[MQTTX CLI](https://mqttx.app/cli)ツールを使って、一方のクラスターからメッセージをパブリッシュし、もう一方でサブスクライブしてみます。

1. `cluster-us-east` でサブスクライバーを起動します：

   ```bash
   mqttx sub -h emqx.us-east.myinfra.net --topic linked/# --qos 1 --verbose
   [6/4/2024] [3:53:32 PM] › …  接続中...
   [6/4/2024] [3:53:32 PM] › ✔  接続完了
   [6/4/2024] [3:53:32 PM] › …  linked/# にサブスクライブ中...
   [6/4/2024] [3:53:32 PM] › ✔  linked/# にサブスクライブ完了
   ```

2. `cluster-eu-west` からメッセージをパブリッシュします：

   ```bash
   mqttx pub -h emqx.eu-west.myinfra.net --topic linked/42 --message "Hello from the other side!"
   [6/4/2024] [3:53:35 PM] › …  接続中...
   [6/4/2024] [3:53:35 PM] › ✔  接続完了
   [6/4/2024] [3:53:35 PM] › …  メッセージをパブリッシュ中...
   [6/4/2024] [3:53:35 PM] › ✔  メッセージパブリッシュ完了
   ```

3. サブスクライバーでメッセージが受信されることを確認します：

   ```bash
   [6/4/2024] [3:53:35 PM] › topic: linked/42
   payload: Hello from the other side!
   ```

4. 逆方向でも同様に繰り返します：

   - `cluster-eu-west` でサブスクライバーを起動：

     ```bash
     mqttx sub -h emqx.eu-west.myinfra.net --topic linked/# --qos 1 --verbose
     [6/4/2024] [3:54:12 PM] › …  接続中...
     [6/4/2024] [3:54:12 PM] › ✔  接続完了
     [6/4/2024] [3:54:12 PM] › …  linked/# にサブスクライブ中...
     [6/4/2024] [3:54:12 PM] › ✔  linked/# にサブスクライブ完了
     ```

   - `cluster-us-east` からメッセージをパブリッシュ：

     ```bash
     mqttx pub -h emqx.us-east.myinfra.net --topic linked/1 --message "Hello from US!"
     [6/4/2024] [3:54:15 PM] › …  接続中...
     [6/4/2024] [3:54:15 PM] › ✔  接続完了
     [6/4/2024] [3:54:15 PM] › …  メッセージをパブリッシュ中...
     [6/4/2024] [3:54:15 PM] › ✔  メッセージパブリッシュ完了
     ```

   - サブスクライバーでメッセージが受信されることを確認：

     ```bash
     [6/4/2024] [3:54:15 PM] › topic: linked/1
     payload: Hello from US!
     ```

完璧です！クラスターリンクは正常に動作しています。

::: tip

クラスターリンクはクラスター間でサブスクリプション情報を伝播させる非同期プロセスです。通常は数ミリ秒で完了しますが、サブスクライブ直後にメッセージをパブリッシュすると、メッセージ配信にわずかな遅延が発生する場合があります。

:::

## 非対称リンクのセットアップ

非対称リンクを作成するには、`cluster-eu-west` の設定を少し変更します。

```bash
cluster {
  name = "cluster-eu-west"
  links = [
    {
      name = "cluster-us-east"
      server = "emqx.eu-west.myinfra.net:11883"
      clientid = "clink-eu-west"
      topics = []
    }
  ]
}
```

ご覧の通り、設定はほぼ前述のものと同じですが、`topics` フィールドが空になっています。これは `cluster-eu-west` が `cluster-us-east` からのいかなるメッセージも受け取らないことを意味します。これによりクラスターリンクは非対称となり、クラスター間の一方向メッセージ転送に有用です。

上記のパブリッシュおよびサブスクライブの手順を繰り返すと、`cluster-us-east` からパブリッシュされたメッセージは `cluster-eu-west` のサブスクライバーに届かないことが確認できます。
