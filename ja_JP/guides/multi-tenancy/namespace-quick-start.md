# クイックスタート：ネームスペースの体験

このセクションでは、[MQTTXクライアント](https://mqttx.app)を使用してEMQXに接続し、ネームスペース機能のコア機能であるテナント識別、クライアント分離、トピック分離を素早く体験する方法を案内します。

## ネームスペース識別のための `tns` 属性を有効化する

1. まず、`base.hocon` にクライアント属性を設定し、ユーザー名からネームスペース（テナント識別子）を抽出します。

   ```
   mqtt.client_attrs_init = [{expression = "nth(1, tokens(username, '-'))", set_as_attr = tns}]
   ```

   > 例：クライアントがユーザー名 `tenantA-user1` で接続すると、EMQXは `tenantA` をネームスペース（`tns`）として抽出します。

   または、ダッシュボードで以下のように設定することも可能です。

   <img src="./assets/enable_namespace.png" alt="ネームスペース有効化" style="zoom:67%;" />

2. MQTTXを使って、テナント `tenantA` を模擬したMQTTクライアント接続を作成し、ユーザー名を `tenantA-user1` に設定してEMQXに接続します。

3. ダッシュボードの **Namespace** ページに移動し、**View Explicitly Created Namespace Only** トグルをオフにします。自動的に作成されたネームスペース `tenantA` が表示されるはずです。

   **Actions** 列の **Clients** をクリックすると、このネームスペースに接続しているクライアントを確認できます。

   ![namespace_client](./assets/namespace_client.png)

## ネームスペース分離の設定と検証

1. ネームスペース間でクライアントIDとトピックを分離するために、`base.hocon` に以下の設定を追加します。

   ```
   mqtt.clientid_override = "concat([client_attrs.tns, '-', clientid])"
   listener.tcp.default.mountpoint = "${client_attrs.tns}/"
   ```

   この設定により、

   - クライアントIDにテナントのプレフィックスが自動的に付加され、IDの競合を回避します。
   - トピック名にネームスペースのプレフィックスが自動的に付加され、テナント間でトピックレベルの分離を実現します。

   ダッシュボード上でも同様の設定が可能です。

   <img src="./assets/clientid_override.png" alt="クライアントIDオーバーライド" style="zoom:67%;" />
   
   <img src="./assets/listener_mountpoint.png" alt="リスナーのマウントポイント設定" style="zoom:67%;" />

2. MQTTXを使い、2つのMQTTクライアント接続を作成して2つのテナント `tenantA` と `tenantB` を模擬します。

   **クライアントA（テナント：tenantA）**:

   | パラメーター | 値              |
   | ------------ | --------------- |
   | Client ID    | `client1`       |
   | Username     | `tenantA-user1` |
   | Subscribe    | `test/topic`    |

   **クライアントB（テナント：tenantB）**:

   | パラメーター | 値              |
   | ------------ | --------------- |
   | Client ID    | `client1`       |
   | Username     | `tenantB-user2` |
   | Publish     | `test/topic`    |

3. クライアントBでメッセージをパブリッシュし、MQTTXおよびEMQXダッシュボードで結果を確認します。

   - 両クライアントは同じクライアントID（`client1`）を使用していますが、プレフィックスルールにより `tenantA-client1` と `tenantB-client1` として接続され、競合を回避しています。
   - 両クライアントは同じトピック（`test/topic`）を使用していますが、ネームスペースで分離されているため、クライアントAはクライアントBがパブリッシュしたメッセージを**受信しません**。
   - ダッシュボードの **Monitoring** -> **Clients** ページでは、
     - クライアントAのサブスクライブしているトピックは `tenantA/test/topic` と表示されます。
     - クライアントBのパブリッシュしているトピックは `tenantB/test/topic` と表示されます。
