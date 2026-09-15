# グローバルネームスペース設定

EMQX 6.1では、個別のネームスペースインスタンスを設定するだけでなく、ネームスペースの識別方法、分離動作の適用方法、トピックおよび認可の取り扱いを制御するグローバルネームスペース設定が利用可能です。

これらの設定はクラスター全体に適用され、すべてのネームスペースおよびクライアント接続に影響します。通常、ネームスペース関連機能を有効化して使用する前に設定します。

グローバルネームスペース設定はダッシュボードの **Management** -> **Namespace** -> **Settings** から管理できます。

::: tip 注意事項

互換性維持のため、EMQX 6.1の多くのグローバルネームスペース設定（Client ID Isolation、Namespace as Mountpoint、Mount Prefix for Authorizationなど）はデフォルトで無効になっています。

対応する分離機能を有効にするには、**Namespace Related Configurations** で明示的にオンにする必要があります。

:::

![拒否されるネームスペース名を含むグローバルネームスペース設定](./assets/namespace_global_settings.png)

## 明示的に作成されたネームスペースのみ許可

この設定は、クライアントが明示的に作成されたネームスペースにのみ接続を許可するかどうかを制御します。設定ファイルの `multi_tenancy.allow_only_managed_namespaces` に対応しています。

この設定を有効にすると、EMQXは接続時にクライアントのネームスペースを検証し、接続を許可するか拒否するかを判断します。

- **有効**:
  - ダッシュボードやREST APIで明示的に作成されていないネームスペースに解決されたクライアントは接続を拒否されます。
  - ネームスペースが解決できないクライアント（例えば、ネームスペースソースが未設定、または有効な値を生成しない場合）も接続拒否されます。
- **無効**:
  - 明示的に作成されていないネームスペースへの接続も許可されます。
  - ネームスペースソースが設定されている場合は、必要に応じてEMQXが自動的にネームスペースを作成することがあります。

::: tip 注意事項

この設定を有効にする前に、**Take Namespace From** が適切に設定されており、すべての有効なクライアントが明示的に作成されたネームスペースを正常に解決できることを確認してください。

そうでないと、ネームスペースが解決できないか明示的に作成されていないためにクライアントが拒否される可能性があります。

**When to Resolve Namespace** で **After Authentication** モードを選択した場合、認証前のネームスペースチェックはスキップされ、明示的に作成されたネームスペースのチェックは認証完了後に実行されます。

:::

## デフォルト最大セッション数

この設定は、新規作成されたネームスペースに対するデフォルトの同時セッション最大数を定義します。

- **有効**:
  - 新規作成されたネームスペースはこの最大セッション数制限を自動的に継承します。
- **無効**:
  - 新規作成されたネームスペースはデフォルトでセッション制限なし（`infinity`）となります。

この設定は設定適用後に作成されたネームスペースにのみ適用されます。既存のネームスペースには影響せず、必要に応じて個別に更新する必要があります。

## 拒否されるネームスペース名

EMQX 6.3.0以降、`multi_tenancy.deny_namespaces` はネームスペース識別子として使用できない名前を指定します。この制限はダッシュボードのユーザーロール、APIキー、管理APIによるネームスペース作成や一括インポート、`client_attrs.tns` を通じたクライアントのネームスペース割り当てに適用されます。

デフォルトリストは `["global", "undefined", "null", "none"]` です。これらの名前はログやダッシュボード出力の内部識別子と混同されやすいため制限されています。

ダッシュボードでリストを編集するには：

1. **Management** -> **Namespace** -> **Settings** に移動します。
2. **Denied Namespace Names** に必要に応じて名前を追加または削除します。すべてのエントリをクリアすると名前制限が無効になります。
3. **Confirm** をクリックして変更を適用します。

`etc/base.hocon` でもリストを設定可能です。以下はデフォルト値の例です：

```hocon
multi_tenancy.deny_namespaces = ["global", "undefined", "null", "none"]
```

カスタムリストはデフォルトリストを置き換えます。引き続き拒否したいデフォルト名を含めてください。名前制限を無効にするには `multi_tenancy.deny_namespaces = []` と設定します。設定ファイルの優先順位については [Config Override Rules](../configuration/configuration.md#config-override-rules) を参照してください。

**Allow Only Explicitly Created Namespaces** が無効でも、解決されたネームスペースがリストに含まれる場合はEMQXが `not_authorized` で接続を拒否します。ただし、ネームスペースを持たないクライアントの接続は `multi_tenancy.allow_only_managed_namespaces = false` の場合は制限されません。

::: warning 重要なお知らせ

デフォルトリストはEMQX 6.3.0以前に許可されていた名前を拒否します。EMQXはこれらの名前を使用しているネームスペースを自動的に移行しません。アップグレード前に該当するネームスペース名を変更するか、`multi_tenancy.deny_namespaces` を調整して許可してください。

:::

## ネームスペースを解決するタイミング

この設定は、接続ライフサイクルのどの時点でEMQXがクライアントのネームスペース識別子を解決するかを制御します。

EMQXはダッシュボードの **When to Resolve Namespace** ラジオボタンで選択可能な2つのモードをサポートしています：

- **Before Authentication**（デフォルト）：認証チェーン実行前にネームスペース式を評価します。この時点で利用可能な接続メタデータ（`username`、`clientid`、`cert_common_name`など）のみを使用します。設定ファイルの `mqtt.client_attrs_init` 経由で `tns` を設定する場合に対応します。
- **After Authentication**：認証チェーン完了後にネームスペース式を評価します。標準の接続メタデータに加え、認証バックエンドから返された属性を含む `client_attrs.*` の値も利用可能です（例：HTTP認証レスポンスの `tag` フィールド）。設定ファイルの `multi_tenancy.post_auth_tns_expression` に対応します。

::: tip

**After Authentication** を設定した場合、EMQXはポスト認証式を使ってネームスペースを割り当てます。ポスト認証式が空文字を返すか失敗しても、プリ認証の `tns` 値はフォールバックとして使用されません。[Empty or Failed Post-authentication Expressions](#empty-or-failed-post-authentication-expressions) を参照してください。

:::

### 明示的に作成されたネームスペースのみ許可との連携

**After Authentication** モードを選択すると、**Allow Only Explicitly Created Namespaces** が有効でも認証前のネームスペースチェックは完全にスキップされます。解決されたネームスペースの存在確認やクォータチェックなどのすべての適用は認証完了後まで延期されます。

## ネームスペースの取得元

この設定は、EMQXがクライアントのネームスペース識別子（`client_attrs.tns`）を導出するために使用するVariform式を指定します。

式は **When to Resolve Namespace** 設定で決まる接続ライフサイクルの時点で評価されます：

- **Before Authentication** モードでは、`username`、`clientid`、`cert_common_name` などの標準接続メタデータと認証前属性のみが利用可能です。
- **After Authentication** モードでは、認証結果からマージされた属性を含む `client_attrs.*` も利用可能です。

::: tip

**Take Namespace From** 式はVariform構文を使用します。利用可能な関数の詳細は [Variform Expressions](../configuration/configuration.md#variform-expressions) を参照してください。

:::

この設定は以下の機能の前提条件です：

- 自動ネームスペース作成
- ネームスペース単位のトピック分離
- ネームスペース単位のClient ID分離
- ネームスペースレベルのセッション制限およびレート制限

**Take Namespace From** が設定されていない場合、`tns` 属性は生成されません。この場合、クライアントはどのネームスペースにも関連付けられず、ネームスペース関連の分離や制御機能はすべて無効のままです。

### 例

#### 認証前

ユーザー名からネームスペースを抽出する例：

```text
nth(1, tokens(username, '-'))
```

この設定では、ユーザー名が `tenantA-user1` のクライアントは認証前に `tenantA` をネームスペース識別子として割り当てられます。

#### 認証後

HTTP認証バックエンドから返された `tag` 属性を使用する例：

```text
client_attrs.tag
```

認証バックエンドがタグを返さない場合のフォールバック例：

```text
coalesce(client_attrs.tag, username)
```

この設定では、EMQXは認証チェーン完了を待ち、マージされた `client_attrs` から `tag` 値を読み取り、ネームスペース識別子として割り当てます。

### 空または失敗したポスト認証式

`multi_tenancy.post_auth_tns_expression` が設定されているが空文字を返すか評価に失敗した場合、EMQXは以下のように接続を処理します。評価失敗時は警告ログも出力されます。

1. プリ認証の `client_attrs.tns` 値が `multi_tenancy.deny_namespaces` に含まれる場合、EMQXは `not_authorized` で接続を拒否します。
2. それ以外の場合、EMQXはクライアントをネームスペースなしとして扱います：
   - `multi_tenancy.allow_only_managed_namespaces = true` の場合、接続を `not_authorized` で拒否します。
   - `multi_tenancy.allow_only_managed_namespaces = false` の場合、プリ認証の `tns` 値を削除し、ネームスペースなしで接続を許可します。

## Client ID分離

Client ID分離は、異なるネームスペースのクライアントが同じClient IDを使用した場合の競合を防ぎます。

EMQXはセッションをグローバルに有効なClient IDで識別し、ネームスペースとClient IDの組み合わせでは識別しません。Client ID分離は、通常ネームスペースをプレフィックスとして付加し、グローバルにユニークな有効Client IDを作成します。クライアントは元のClient IDを送信し続け、EMQXは内部的に上書きされたIDを有効Client IDとして使用します。

### Client ID上書き方式の選択

ネームスペース情報の取得元と有効Client IDにネームスペースを含める必要性に応じて方式を選択してください：

- ネームスペースが認証前に利用可能な場合は、`mqtt.clientid_override` を設定します。EMQXは `mqtt.client_attrs_init` 後、認証前にこの式を評価するため、`client_attrs.tns` を含む初期化済み属性を利用できます。
- ネームスペースが認証結果から取得され、有効Client IDに含める必要がある場合は、[認証バックエンドに `clientid_override` を返すよう設定](../access-control/authn/authn.md#override-client-ids-from-authentication-results)してください。返される値は完全な新Client IDを含む必要があります。`mqtt.clientid_override` 式は認証バックエンドから返された属性や `multi_tenancy.post_auth_tns_expression` で生成されたネームスペースを使用できません。
- `multi_tenancy.post_auth_tns_expression` でネームスペースを設定しても有効Client IDに含める必要がない場合は、クライアントが既にグローバルにユニークなClient IDを使用している場合のみClient ID上書きは不要です。

接続に対しては1つのClient ID上書き方式のみ使用してください。両方設定されている場合、認証結果の上書きが後から実行され、`mqtt.clientid_override` で生成されたClient IDを置き換えます。いずれの場合も、結果のClient IDがグローバルにユニークであることを確認してください。

### EMQXのClient ID上書き適用順序

EMQXは以下の順序で有効Client IDを決定します：

1. `mqtt.client_attrs_init` でクライアント属性を初期化
2. 認証前に `mqtt.clientid_override` を評価
3. クライアント認証を行い、成功した認証結果で返された空でない `clientid_override` を適用
4. `multi_tenancy.post_auth_tns_expression` を評価
5. 有効Client IDでクライアントセッションをオープン

EMQXは `mqtt.clientid_override` を再評価せず、認証後に取得したネームスペースを自動的にClient IDに追加しません。認証結果が `clientid_override` を省略または空文字で返した場合、EMQXは以前に決定したClient IDを保持します。

### 認証前Client ID分離の設定

ダッシュボードでClient ID分離を有効にすると、EMQXは `mqtt.clientid_override` を設定し、推奨式を自動的に入力します：

```
concat([client_attrs.tns, '-', clientid])
```

::: warning 重要なお知らせ

EMQX 6.3.0以降、`mqtt.clientid_override` 式がエラーを起こすか空文字を返すと、EMQXはエラーログを出力し接続を拒否します。MQTT 5.0クライアントはCONNACK理由コード `0x85`（Client Identifier not valid）、MQTT 3.1および3.1.1クライアントはリターンコード `2` を受け取ります。EMQXはクライアントから送信されたClient IDにフォールバックしません。

アップグレード前に、すべての接続クライアントが設定された式を空でない文字列に評価できることを確認してください。評価できないクライアントの式や必要なクライアントデータを修正してください。

:::

この設定により：

- 異なるネームスペースのクライアントが同じClient IDを安全に使用可能になります。
- 内部的に使用されるClient IDは常にネームスペースプレフィックスを含みます。

この式は認証前に解決されたネームスペース用の例です。結果のClient IDがグローバルにユニークである限り、ビジネス要件に合わせてカスタマイズ可能です。

### 動作例

ユーザー名からネームスペースを抽出するネームスペースソースが設定されているとします：

```
nth(1, tokens(username, '-'))
```

Client ID分離はデフォルト式で有効化されています：

```
concat([client_attrs.tns, '-', clientid])
```

#### クライアント接続情報

| クライアント | ユーザー名       | Client ID |
| ------------ | ---------------- | --------- |
| A            | tenantA-user1    | client1   |
| B            | tenantB-user2    | client1   |

#### 内部的に使用されるClient ID

| ネームスペース | 元のClient ID | 実際のClient ID     |
| -------------- | ------------- | ------------------- |
| tenantA        | client1       | tenantA-client1     |
| tenantB        | client1       | tenantB-client1     |

## ネームスペースをマウントポイントとして使用

有効にすると、EMQXはネームスペースをトピックのマウントポイントとして使用し、ネームスペースレベルのトピック分離を実現します。

リスナーにすでに `mountpoint` が設定されている場合、この設定は無視され、リスナーレベルの設定が優先されます。

### 動作

**Namespace as Mountpoint** を有効にすると、EMQXは以下のようにトピックを分離します：

- `PUBLISH`、`SUBSCRIBE`、`UNSUBSCRIBE`、およびWillメッセージ処理時に：
  - EMQXは内部的にトピックの先頭に `{namespace}/` を自動的に付加します。
- クライアントへのメッセージ配信時に：
  - ネームスペースプレフィックスは自動的に取り除かれます。
- クライアントから見た場合：
  - パブリッシュおよびサブスクライブするトピック名は変更されません。
  - クライアントはネームスペースプレフィックスを認識しません。

### 例

クライアントがネームスペース `n1` に属し、**Namespace as Mountpoint** が有効になっているとします。

#### クライアント側の動作

- クライアントは `sensors/#` をサブスクライブ
- クライアントは `sensors/data` にパブリッシュ

#### EMQX内部処理

- ブローカーはサブスクリプションを `n1/sensors/#` として登録
- ブローカーはメッセージを `n1/sensors/data` でルーティング
- メッセージはクライアントに `sensors/data` として配信

結果として：

- ネームスペースプレフィックスは内部でのみ使用されます。
- クライアントは常に元のトピック名でやり取りします。
- 異なるネームスペースのクライアントが同じトピックを使用しても互いのメッセージを受信しません。

## 認可のためのマウントプレフィックス

この設定は、認可（ACL）チェックの前にトピックマウントポイントプレフィックスを対象トピックやトピックフィルターに付加するかどうかを制御します。

マウントポイントプレフィックスは通常、**Namespace as Mountpoint** が有効な場合のネームスペースから取得され、以下の形式です：

```
{namespace}/
```

### 動作

**Mount Prefix for Authorization** が有効な場合：

- EMQXはACLルールや認可バックエンドのマッチング前に、対象トピックやトピックフィルターの先頭にマウントポイントを付加します。
- 認可チェックは付加済みトピックで行われます。

この動作は以下の操作に適用されます：

- `PUBLISH`
- `SUBSCRIBE`
- `UNSUBSCRIBE`
- Willメッセージ

### 例

以下の設定が有効とします：

- **Namespace as Mountpoint**
- **Mount Prefix for Authorization**
- クライアントのネームスペース：`n1`

#### クライアント操作

クライアントは以下をサブスクライブしようとします：

```
sensors/#
```

#### 認可に使用されるトピック

認可時にEMQXは `n1/sensors/#` を評価します。したがって、対応するACLルールは `sensors/#` ではなく `n1/sensors/#` として定義する必要があります。

### 推奨

トピック分離のために **Namespace as Mountpoint** を有効にしている場合は、**Mount Prefix for Authorization** も有効にすることを推奨します。これにより、認可チェックがブローカー内部で使用されるトピック名と一致し、認可結果と実際のメッセージルーティングの不整合を防止できます。
