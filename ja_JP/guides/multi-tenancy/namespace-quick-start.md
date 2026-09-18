# クイックスタート：ネームスペースの体験

このセクションでは、[MQTTXクライアント](https://mqttx.app)を使用してEMQXに接続し、ネームスペース機能のコア機能であるテナント識別、クライアント分離、トピック分離を素早く体験する方法を案内します。

## ネームスペース識別のために `tns` 属性を有効化する

1. まず、`base.hocon` にクライアント属性を設定し、ユーザー名からネームスペース（テナント識別子）を抽出するように構成します。

   ```
   mqtt.client_attrs_init = [{expression = "nth(1, tokens(username, '-'))", set_as_attr = tns}]
   ```

   > 例：クライアントがユーザー名 `tenantA-user1` で接続すると、EMQXは `tenantA` をネームスペース（`tns`）として抽出します。

   または、ダッシュボードで以下のように設定することも可能です。

   <img src="./assets/enable_namespace.png" alt="ネームスペースを有効化" style="zoom:67%;" />

2. MQTTXを使ってMQTTクライアント接続を作成し、テナント `tenantA` をシミュレートして、ユーザー名を `tenantA-user1` に設定します。クライアントをEMQXに接続してください。

3. ダッシュボードの **ネームスペース** ページに移動し、**明示的に作成されたネームスペースのみ表示** のトグルをオフにします。自動的に作成されたネームスペース `tenantA` が表示されるはずです。

   **操作** 列の **クライアント** をクリックすると、このネームスペースに接続されているクライアントを確認できます。

   ![namespace_client](./assets/namespace_client.png)

## ネームスペース分離の設定と検証

1. ネームスペース間でクライアントIDとトピックを分離するには、`base.hocon` に以下の設定を追加します。

   ```
   mqtt.clientid_override = "concat([client_attrs.tns, '-', clientid])"
   listener.tcp.default.mountpoint = "${client_attrs.tns}/"
   ```

   この設定により：

   - クライアントIDの先頭にテナントプレフィックスが自動的に付与され、競合を回避します。
   - トピック名の先頭にネームスペースプレフィックスが自動的に付与され、テナント間のトピックレベルの分離を実現します。

   ダッシュボードでも同様の設定が可能です。

   <img src="./assets/clientid_override.png" alt="クライアントIDオーバーライド設定" style="zoom:67%;" />

   <img src="./assets/listener_mountpoint.png" alt="リスナーのマウントポイント設定" style="zoom:67%;" />

2. MQTTXを使って、2つのMQTTクライアント接続を作成し、2つのテナント `tenantA` と `tenantB` をシミュレートします。

   **クライアントA（テナント：tenantA）**：

   | パラメータ | 値               |
   | ---------- | ---------------- |
   | クライアントID | `client1`       |
   | ユーザー名     | `tenantA-user1` |
   | サブスクライブ | `test/topic`    |

   **クライアントB（テナント：tenantB）**：

   | パラメータ | 値               |
   | ---------- | ---------------- |
   | クライアントID | `client1`       |
   | ユーザー名     | `tenantB-user2` |
   | パブリッシュ   | `test/topic`    |

3. クライアントBを使ってメッセージをパブリッシュします。MQTTXとEMQXダッシュボードで結果を確認してください。

   - 両クライアントは同じクライアントID（`client1`）を使用していますが、プレフィックスルールにより `tenantA-client1` と `tenantB-client1` として接続され、競合を回避しています。
   - 両クライアントは同じトピック（`test/topic`）を使用していますが、ネームスペースによって分離されているため、クライアントAはクライアントBがパブリッシュしたメッセージを**受信しません**。
   - **モニタリング** -> **クライアント** ページでは：
     - クライアントAのサブスクライブしたトピックは `tenantA/test/topic` として表示されます。
     - クライアントBのパブリッシュしたトピックは `tenantB/test/topic` として表示されます。
