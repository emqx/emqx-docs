# クラスターリンクのクイックスタート

このページでは、2つの離れたEMQXクラスター間でクラスターリンクを設定するためのクイックスタートガイドを提供します。

## 前提条件

異なる2つのリージョンにそれぞれEMQXクラスターをホストするためのコンピュートリソースを用意してください。この例では、`us-east` と `eu-west` リージョンを使用し、クラスター名をそれぞれ `cluster-us-east` と `cluster-eu-west` とします。

### 要件

- EMQXバージョン5.8.0以降
- 一意のクラスター名
- クラスター間のネットワーク通信

クラスターリンクでは、各クラスターのMQTTリスナーが相手クラスターのネットワークから到達可能である必要があります。トラフィックを均等に分散するために、これらのMQTTリスナーはロードバランサーの背後に配置することを推奨します。セキュリティのため、パブリックインターネットを使用する場合は、[TLS](./configuration.md)および厳格な[TLSまたはMQTTクライアント認証](../../guides/access-control/authn/authn.md)を用いてクラスター間通信を保護してください。

本番環境に移行する前に、専用の[セキュアクラスターリンク](./security.md)ガイドに従い、各クラスターの認証、認可、および`$LINK/`トピックネームスペースの強化を行ってください。

## 最初のクラスター（cluster-us-east）のセットアップ

最初のクラスター `cluster-us-east` を、クラスターの設定ファイルに以下の設定スニペットを追加してセットアップします。

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
- すべてのメッセージ（`#` ワイルドカードトピックにマッチ）がローカルクラスターに転送される
- ポート `11883` でクラスターリンク接続専用のリスナーが有効化されている

## 2番目のクラスター（cluster-eu-west）のセットアップ

2番目のクラスター `cluster-eu-west` を、クラスターの設定ファイルに以下の設定スニペットを追加してセットアップします。

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

この設定は最初のクラスターの設定と対称的です。両方の設定が完了し、クラスターが稼働すると、2つのクラスター間に対称的かつ双方向のクラスターリンクが確立されます。非対称リンクの作成も可能であり、後述します。

## クラスターリンクの動作確認

異なるクラスターに接続されたクライアント同士が標準のMQTT機構で通信できることを確認するために、[MQTTX CLI](https://mqttx.app/cli)ツールを使って一方のクラスターからメッセージをパブリッシュし、もう一方でサブスクライブしてみます。

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

3. サブスクライバーでメッセージを受信していることを確認します：

   ```bash
   [6/4/2024] [3:53:35 PM] › topic: linked/42
   payload: Hello from the other side!
   ```

4. 逆方向でも同様の手順を繰り返します：

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

   - サブスクライバーでメッセージを受信していることを確認：

     ```bash
     [6/4/2024] [3:54:15 PM] › topic: linked/1
     payload: Hello from US!
     ```

完璧です！クラスターリンクは正常に動作しています。

::: tip

クラスターリンクはクラスター間でサブスクリプション情報を伝播する非同期プロセスを含みます。通常は数ミリ秒程度ですが、サブスクライブ直後にメッセージをパブリッシュすると、メッセージ配信に若干の遅延が発生する場合があります。

:::

## 非対称リンクの設定

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

ご覧の通り、前回の設定とほぼ同じですが、`topics` フィールドが空になっています。これは `cluster-eu-west` が `cluster-us-east` からのメッセージを一切受け取らないことを意味します。これによりクラスターリンクは非対称となり、クラスター間の一方向メッセージ転送に適しています。

上記のメッセージパブリッシュおよびサブスクライブの手順を繰り返すと、`cluster-us-east` からパブリッシュされたメッセージが `cluster-eu-west` のサブスクライバーに届かないことが確認できます。
