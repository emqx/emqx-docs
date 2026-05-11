# Namespace

EMQX 5.9.0以降、Namespace機能により、MQTTクライアントを論理的にグループ化し、単一のEMQXクラスター内でトラフィック制限を適用できます。この機能は、複数のクライアントグループ（事業部門、アプリケーション、顧客など）が同じインフラを共有しつつ、論理的に分離されたスケーラブルなデプロイメントを可能にします。

::: tip 注意

この機能はEMQX 5.9ではNamespaceと呼ばれていますが、マルチテナンシー設計原則に従っています。

:::

## Namespaceとは

EMQX EnterpriseのNamespaceは、MQTTクライアントの論理的な分離およびリソース管理のための仕組みです。異なる事業やテナントのクライアントを共有されたEMQXクラスター内の別々のNamespaceに分割し、接続、メッセージ、クォータなどの分離を実現します。

Namespaceは特別なクライアント属性 `tns`（テナントNamespace）で識別され、クライアントの接続メタデータ（ユーザー名やServer Name Indication（SNI）など）から抽出されます。

> **典型的な利用例**：企業内の複数事業部門がクラスターを共有、テナント単位のリソース分離管理、集中アクセス制御など。

### Namespaceで実現できること

- **クライアントとメッセージの論理的分離**

  Namespaceにより、異なるテナント間でクライアントIDやトピックスペースを論理的に分離できます。

  ::: tip 注意

  Namespaceを有効にしても、クライアントIDの上書きやトピックプレフィックスは自動的には適用されません。これらは手動で設定する必要があります。詳細は[Isolation Mechanisms](#isolation-mechanisms)を参照してください。

  :::

- **テナント単位のクォータおよび接続制御**

  各Namespaceごとに同時接続数やメッセージパブリッシュレートの上限を設定でき、公平な利用とシステム安定性を確保します。

- **強化されたログと運用可視化**

  ログには自動的にNamespace識別子（`tns`）が含まれ、クライアントの活動追跡や問題検出、テナント単位の診断が容易になります。

- **Namespaceベースのリソース監視**

  Namespaceごとの接続数やメッセージスループットなどのメトリクス収集に明確な境界を提供し、容量計画や運用インサイトに役立ちます。

### Isolation Mechanisms

EMQXは高い柔軟性を持ち、Namespace機能導入前から様々な分離手法をサポートしてきました。Namespace機能は統一されたテナント識別フィールド（`client_attrs.tns`）を提供し、クライアントIDやトピックのマウントポイントなどの設定を統一テナント情報に基づいて整理・管理できます。

ただし、分離戦略はビジネス要件に応じてユーザーが**手動で設定**する必要があり、クライアントIDやトピックの分離機能が自動的に有効になるわけではありません。

- **クライアントIDの上書き**

  異なるNamespaceのクライアントが同じクライアントIDでEMQXに接続したい場合、クライアントID上書きルールを設定できます。例：

  ```
  mqtt.clientid_override = "concat([client_attrs.tns, '-', clientid])"
  ```

  このルールはNamespaceをクライアントIDのプレフィックスとして付加し、競合を回避します。

- **マウントポイントによるトピック分離**

  異なるNamespaceのクライアントが同じトピック名でパブリッシュやサブスクライブしても互いに影響しないように、マウントポイントを使ってNamespaceプレフィックスを自動付加できます。

  ```
  listener.{TYPE}.{NAME}.mountpoint = "${client_attrs.tns}/"
  ```

  この設定により、トピック名にNamespaceプレフィックスが追加されます。

バージョン5.9時点では、NamespaceはMQTTクライアントにのみ適用され、ダッシュボードやREST APIはまだNamespaceによる分離がされていません。EMQXは将来的に管理用NamespaceとMQTT Namespaceの統合管理を実装する予定です。詳細は[Multi-Tenancy Roadmap](#multi-tenancy-roadmap)を参照してください。

## Namespaceの有効化

Namespace機能を有効にするには、まずクライアントがどのNamespaceに属するかをEMQXに伝える必要があります。これは特別なクライアント属性 `tns`（テナントNamespace）を設定することで行います。

### 設定ファイルでNamespaceを有効化

`tns`属性は、クライアントの接続メタデータ（ユーザー名、SNI、その他のフィールド）から抽出可能です。

例えば、クライアントのユーザー名をNamespace識別子として使う場合、以下の設定を適用します。

```
mqtt.client_attrs_init = [{expression = username, set_as_attr = tns}]
```

### ダッシュボードでNamespaceを有効化

EMQXダッシュボードからもNamespaceを有効化できます。

1. **Management** -> **MQTT Settings** -> **General**タブに移動し、**Client Attributes**セクションを探します。
2. **Add**をクリックし、以下の情報を入力します：
   - **Attribute**: `tns`
   - **Attribute Expression**: 例えばクライアントのユーザー名をNamespace識別子にしたい場合は `username` と入力します。他の変数も利用可能です。属性式の詳細は[Set Client Attributes](../client-attributes/client-attributes.md#set-client-attributes)を参照してください。
3. **Save Changes**をクリックします。

## マルチテナンシーロードマップ

- 管理用NamespaceとMQTT Namespaceの統合
- ルール、アクション/データソース、コネクターの分離実装
- 組み込みデータベース認証の分離実装
- 組み込みデータベース認可の分離実装
- 保持メッセージのクォータ分離実装
- Prometheusメトリクスの分離実装

## 次のステップ

Namespaceの概要と実現可能なことを理解したところで、EMQXでの利用を開始するための次のステップは以下の通りです：

- **[Namespaceの作成](./create-namespace.md)**  
  ダッシュボードやREST APIを使ってNamespaceを明示的に作成する方法や、クライアントメタデータに基づき自動作成する方法を学べます。
- **[Namespaceの設定と管理](./configure-manage-namespace.md)**  
  ダッシュボードやREST APIでレート制限やセッションクォータを設定・管理する方法を解説します。
- **[クイックスタート：Namespaceを体験する](./namespace-quick-start.md)**  
  MQTTXを使った実践的なガイドで、Namespaceベースのクライアント・トピック分離を素早く試せます。
