# クラスターリンクのクイックスタート

このページでは、離れた2つのEMQXクラスター間でクラスターリンクを設定するためのクイックスタートガイドを提供します。

## 前提条件

2つの異なるリージョンにそれぞれEMQXクラスターをホストするコンピュートリソースがあることを確認してください。例として、`us-east` と `eu-west` のリージョンを使用し、それぞれのクラスター名を `cluster-us-east` と `cluster-eu-west` とします。

### 要件

- EMQX バージョン 5.8.0 以降
- 一意のクラスター名
- クラスター間のネットワーク通信

クラスターリンクでは、各クラスターのMQTTリスナーが他方のクラスターのネットワークから到達可能である必要があります。トラフィックを均等に分散するために、これらのMQTTリスナーはロードバランサーの背後に配置することを推奨します。セキュリティのため、パブリックインターネットを使用する場合は、[TLS](./configuration.md)および厳格な[TLSまたはMQTTクライアント認証](../../operate/access-control/authn/authn.md)によってクラスター間通信を保護してください。

## 1つ目のクラスター（cluster-us-east）の設定

1つ目のクラスター `cluster-us-east` は、以下の設定スニペットをクラスターの設定ファイルに追加してセットアップします。

```bash
# クラスターリンク設定
cluster {
  # このクラスターの名前
  name = "cluster-us-east"
  links = [
    {
      # 2つ目のクラスター名
      name = "cluster-eu-west"
      # 2つ目のクラスターのMQTTリスナーのエンドポイント
      server = "emqx.us-east.myinfra.net:11883"
      clientid = "clink-us-east"
      topics = ["#"]
    }
  ]
}

# クラスターリンク接続用の専用リスナー
listeners {
  tcp.clink {
    bind = 11883
  }
}
```

この設定は以下を指定しています：
- リモートクラスター名は `cluster-eu-west`
- クラスターは `emqx.us-east.myinfra.net:11883` でアクセス可能
- クラスターリンクMQTT接続のClient IDプレフィックスは `clink-us-east`
- `#` ワイルドカードトピックにマッチするすべてのメッセージをローカルクラスターに転送
- ポート `11883` でクラスターリンク接続用の専用リスナーを有効化

## 2つ目のクラスター（cluster-eu-west）の設定

2つ目のクラスター `cluster-eu-west` は、以下の設定スニペットをクラスターの設定ファイルに追加してセットアップします。

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

# クラスターリンク接続用の専用リスナー
listeners {
  tcp.clink {
    bind = 11883
  }
}
```

この設定は1つ目のクラスターの設定と対称的です。両方の設定が完了し、クラスターが稼働すると、2つのクラスター間で対称的な双方向クラスターリンクが確立されます。非対称リンクの作成も可能で、後述します。

## クラスターリンクの確認

異なるクラスターに接続されたクライアント同士が標準のMQTTメカニズムで通信できることを確認するために、[MQTTX CLI](https://mqttx.app/cli) ツールを使って一方のクラスターからメッセージをパブリッシュし、もう一方でサブスクライブしてみます。

1. `cluster-us-east` でサブスクライバーを起動：

   ```bash
   mqttx sub -h emqx.us-east.myinfra.net --topic linked/# --qos 1 --verbose
   [6/4/2024] [3:53:32 PM] › …  Connecting...
   [6/4/2024] [3:53:32 PM] › ✔  Connected
   [6/4/2024] [3:53:32 PM] › …  Subscribing to linked/#...
   [6/4/2024] [3:53:32 PM] › ✔  Subscribed to linked/#
   ```

2. `cluster-eu-west` からメッセージをパブリッシュ：

   ```bash
   mqttx pub -h emqx.eu-west.myinfra.net --topic linked/42 --message "Hello from the other side!"
   [6/4/2024] [3:53:35 PM] › …  Connecting...
   [6/4/2024] [3:53:35 PM] › ✔  Connected
   [6/4/2024] [3:53:35 PM] › …  Message publishing...
   [6/4/2024] [3:53:35 PM] › ✔  Message published
   ```

3. サブスクライバーでメッセージを受信：

   ```bash
   [6/4/2024] [3:53:35 PM] › topic: linked/42
   payload: Hello from the other side!
   ```

4. 逆方向でも同様に実施：

   - `cluster-eu-west` でサブスクライバーを起動：

     ```bash
     mqttx sub -h emqx.eu-west.myinfra.net --topic linked/# --qos 1 --verbose
     [6/4/2024] [3:54:12 PM] › …  Connecting...
     [6/4/2024] [3:54:12 PM] › ✔  Connected
     [6/4/2024] [3:54:12 PM] › …  Subscribing to linked/#...
     [6/4/2024] [3:54:12 PM] › ✔  Subscribed to linked/#
     ```

   - `cluster-us-east` からメッセージをパブリッシュ：

     ```bash
     mqttx pub -h emqx.us-east.myinfra.net --topic linked/1 --message "Hello from US!"
     [6/4/2024] [3:54:15 PM] › …  Connecting...
     [6/4/2024] [3:54:15 PM] › ✔  Connected
     [6/4/2024] [3:54:15 PM] › …  Message publishing...
     [6/4/2024] [3:54:15 PM] › ✔  Message published
     ```

   - サブスクライバーでメッセージを受信：

     ```bash
     [6/4/2024] [3:54:15 PM] › topic: linked/1
     payload: Hello from US!
     ```

完璧です！クラスターリンクは正常に動作しています。

::: tip

クラスターリンクはクラスター間でサブスクリプション情報を伝播する非同期プロセスを含みます。通常は数ミリ秒で完了しますが、サブスクライブ直後にメッセージをパブリッシュすると、メッセージ配信にわずかな遅延が発生する場合があります。

:::

## 非対称リンクの設定

非対称リンクを作成するには、`cluster-eu-west` の設定を以下のように少し変更します。

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

上記のメッセージパブリッシュとサブスクライブの手順を繰り返すと、`cluster-us-east` からパブリッシュされたメッセージが `cluster-eu-west` のサブスクライバーに届かないことが確認できます。
