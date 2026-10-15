# グローバルネームスペース設定

EMQX 6.1 では、個別のネームスペースインスタンスの設定に加えて、ネームスペースの識別方法、分離動作の適用方法、トピックおよび認可の取り扱いを制御する一連のグローバルネームスペース設定が利用可能です。

これらの設定はクラスター全体に適用され、すべてのネームスペースおよびクライアント接続に影響します。通常、ネームスペース関連機能を有効化して使用する前に設定します。

グローバルネームスペース設定はダッシュボードの **Management** -> **Namespace** -> **Settings** から管理できます。

::: tip 注意

後方互換性を維持するため、EMQX 6.1 の多くのグローバルネームスペース設定（Client ID Isolation、Namespace as Mountpoint、Mount Prefix for Authorization など）はデフォルトで無効になっています。

対応する分離機能を有効にするには、**Namespace Related Configurations** の下で明示的にオンにする必要があります。

:::

![拒否されたネームスペース名を含むグローバルネームスペース設定](./assets/namespace_global_settings.png)

## 明示的に作成されたネームスペースのみを許可

この設定は、クライアントが明示的に作成されたネームスペースにのみ接続を許可するかどうかを制御します。設定ファイルの `multi_tenancy.allow_only_managed_namespaces` に対応します。

この設定を有効にすると、EMQX は接続時にクライアントのネームスペースを検証し、接続を許可するか拒否するかを判断します。

- **有効**:
  - ダッシュボードや REST API を通じて明示的に作成されていないネームスペースのクライアントは接続を拒否されます。
  - ネームスペースが解決できないクライアント（例：ネームスペースソースが未設定、または有効な値を生成しない場合）も接続を拒否されます。
- **無効**:
  - 明示的に作成されていないネームスペースへの接続も許可されます。
  - ネームスペースソースが設定されている場合、必要に応じて EMQX が自動的にネームスペースを作成することがあります。

::: tip 注意

この設定を有効にする前に、**Take Namespace From** が適切に設定されており、すべての有効なクライアントが明示的に作成されたネームスペースを正常に解決できることを確認してください。そうでないと、ネームスペースが解決できないか明示的に作成されていないためにクライアントが拒否される可能性があります。

**When to Resolve Namespace** で **After Authentication** モードを選択した場合、認証前のネームスペースチェックはスキップされ、明示的に作成されたネームスペースのチェックは認証完了後に実行されます。

:::

## デフォルトの最大セッション数

この設定は、新規作成されるネームスペースの同時セッション数の最大値のデフォルトを定義します。

- **有効**:
  - 新規作成されるネームスペースは自動的にこの最大セッション制限を継承します。
- **無効**:
  - 新規作成されるネームスペースはデフォルトでセッション制限なし（`infinity`）となります。

この設定は設定適用後に作成されるネームスペースにのみ適用されます。既存のネームスペースには影響せず、必要に応じて個別に更新する必要があります。

## 拒否されるネームスペース名

EMQX 6.3.0 以降、`multi_tenancy.deny_namespaces` はネームスペース識別子として使用できない名前を指定します。この制限はダッシュボードのユーザーロール、APIキー、管理APIによるネームスペース作成および一括インポート、`client_attrs.tns` を通じたクライアントのネームスペース割当てに適用されます。

デフォルトリストは `["global", "undefined", "null", "none"]` です。これらの名前はログやダッシュボードの出力で内部識別子と混同される恐れがあります。

ダッシュボードでリストを編集するには：

1. **Management** -> **Namespace** -> **Settings** に移動します。
2. **Denied Namespace Names** に必要に応じて名前を追加または削除します。すべてのエントリをクリアすると名前制限が無効になります。
3. **Confirm** をクリックして変更を適用します。

`etc/base.hocon` でもリストを設定できます。以下はデフォルト値の例です：

```hocon
multi_tenancy.deny_namespaces = ["global", "undefined", "null", "none"]
```

カスタムリストはデフォルトリストを置き換えます。拒否したいデフォルト名を含めてください。名前制限を無効にするには `multi_tenancy.deny_namespaces = []` と設定します。設定ファイルの優先順位については [Config Override Rules](../configuration/configuration.md#config-override-rules) を参照してください。

**Allow Only Explicitly Created Namespaces** が無効でも、解決されたネームスペースがリストに含まれる場合、EMQX は `not_authorized` でクライアント接続を拒否します。この制限はネームスペースがないクライアントの接続を妨げません（`multi_tenancy.allow_only_managed_namespaces = false` の場合）。

::: warning 重要なお知らせ

デフォルトリストは EMQX 6.3.0 より前に許容されていた名前を拒否します。EMQX はこれらの名前を使用しているネームスペースを自動で移行しません。アップグレード前に該当するネームスペース名を変更するか、`multi_tenancy.deny_namespaces` を調整して許可してください。

:::

## ネームスペースを解決するタイミング

この設定は、接続ライフサイクルのどの時点で EMQX がクライアントのネームスペース識別子を解決するかを制御します。

EMQX はダッシュボードの **When to Resolve Namespace** ラジオボタンで選択可能な2つのモードをサポートします：

- **Before Authentication**（デフォルト）：認証チェーン実行前にネームスペース式を評価します。この時点で利用可能なのは `username`、`clientid`、`cert_common_name` などの接続メタデータのみです。設定ファイルの `mqtt.client_attrs_init` で `tns` を設定することに対応します。
- **After Authentication**：認証チェーン完了後にネームスペース式を評価します。標準の接続メタデータに加え、認証バックエンドから返された属性を含む `client_attrs.*` が利用可能です（例：HTTP認証レスポンスの `tag` フィールド）。設定ファイルの `multi_tenancy.post_auth_tns_expression` に対応します。

::: tip

**After Authentication** が設定されている場合、EMQX はポスト認証式を使ってネームスペースを割り当てます。ポスト認証式が空文字や失敗した場合でも、プリ認証の `tns` 値はフォールバックとして使用されません。詳細は [Empty or Failed Post-authentication Expressions](#empty-or-failed-post-authentication-expressions) を参照してください。

:::

### 明示的に作成されたネームスペースのみを許可との連携

**After Authentication** モードを選択すると、**Allow Only Explicitly Created Namespaces** が有効でも認証前のネームスペースチェックは完全にスキップされます。解決されたネームスペースの存在確認やクォータチェックなどのすべての適用は認証完了後に行われます。

## ネームスペースの取得元

この設定は、EMQX がクライアントのネームスペース識別子（`client_attrs.tns`）を導出するために使用する Variform 式を指定します。

式は **When to Resolve Namespace** 設定で決まる接続ライフサイクルの時点で評価されます：

- **Before Authentication** モードでは、`username`、`clientid`、`cert_common_name` などの標準接続メタデータと認証前属性のみが利用可能です。
- **After Authentication** モードでは、認証結果からマージされた属性を含む `client_attrs.*` も利用可能です。

::: tip

**Take Namespace From** 式は Variform 構文を使用します。利用可能な関数の詳細は [Variform Expressions](../configuration/configuration.md#variform-expressions) を参照してください。

:::

この設定は以下の機能の前提条件です：

- 自動ネームスペース作成
- ネームスペースベースのトピック分離
- ネームスペースベースの Client ID 分離
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

認証バックエンドがタグを返さない場合のフォールバック：

```text
coalesce(client_attrs.tag, username)
```

この設定では、EMQX は認証チェーン完了を待ち、マージされた `client_attrs` から `tag` 値を読み取り、それをネームスペース識別子として割り当てます。

### ポスト認証式が空または失敗した場合

`multi_tenancy.post_auth_tns_expression` が設定されているが空文字を返すか評価に失敗した場合、EMQX は以下のように接続を処理します。評価失敗は警告ログも生成します。

1. プリ認証の `client_attrs.tns` が `multi_tenancy.deny_namespaces` に含まれる場合、EMQX は `not_authorized` で接続を拒否します。
2. それ以外の場合、クライアントはネームスペースなしとして扱われます：
   - `multi_tenancy.allow_only_managed_namespaces = true` の場合、EMQX は `not_authorized` で接続を拒否します。
   - `multi_tenancy.allow_only_managed_namespaces = false` の場合、プリ認証の `tns` 値を削除し、ネームスペースなしで接続を許可します。

## Client ID 分離

Client ID 分離は、異なるネームスペースのクライアントが同一の Client ID を使用した場合の競合を防止します。

EMQX はセッション識別にネームスペースと元の Client ID を別々のフィールドとして使いません。代わりに、単一の有効な Client ID でグローバルにセッションを識別します。異なるネームスペースのクライアントが同じ元の Client ID を使う場合、Client ID 分離は通常、元の Client ID にネームスペースをプレフィックスとして付加し、グローバルに一意な有効 Client ID を生成します。クライアントは元の Client ID を送信し続け、EMQX は内部的に上書きされた Client ID を使ってセッションを識別します。

これらの Client ID 上書き機構は MQTT クライアント接続にのみ適用されます。EMQX 6.3.1 以降、ゲートウェイプロトコルはプロトコル定義の Client ID を保持し、認証バックエンドから返される `clientid_override` を無視します。

### Client ID 上書き機構の選択

ネームスペース情報の取得元と有効 Client ID にネームスペースを含める必要があるかに応じて機構を選択します：

- ネームスペースが認証前に利用可能な場合は、`mqtt.clientid_override` を設定します。EMQX は `mqtt.client_attrs_init` の後、認証前にこの式を評価するため、`mqtt.client_attrs_init` で初期化された属性（`client_attrs.tns` を含む）を使用できます。
- ネームスペースが認証結果から取得され、有効 Client ID に含める必要がある場合は、[認証バックエンドで `clientid_override` を返すよう設定](../access-control/authn/authn.md#override-client-ids-from-authentication-results)します。返される値は完全な新しい Client ID でなければなりません。`mqtt.clientid_override` 式は認証バックエンドから返された属性や `multi_tenancy.post_auth_tns_expression` で生成されたネームスペースを使用できません。
- `multi_tenancy.post_auth_tns_expression` でネームスペースを設定しているが、有効 Client ID に含める必要がない場合は、クライアントが既にグローバルに一意な Client ID を使用している場合に限り、Client ID 上書きは不要です。

接続に対しては Client ID 上書き機構を1つだけ使用してください。両方設定されている場合、認証結果の上書きが後から実行され、`mqtt.clientid_override` による Client ID を置き換えます。いずれの場合も、結果の Client ID がグローバルに一意であることを確認してください。

### EMQX の Client ID 上書き適用手順

EMQX は有効 Client ID を以下の順序で決定します：

1. `mqtt.client_attrs_init` でクライアント属性を初期化
2. 認証前に `mqtt.clientid_override` を評価
3. クライアント認証を実行し、成功した認証結果で非空の `clientid_override` を適用
4. `multi_tenancy.post_auth_tns_expression` を評価
5. 有効 Client ID でクライアントセッションをオープン

EMQX は `mqtt.clientid_override` を再評価せず、認証後に取得したネームスペースを自動的に Client ID に追加しません。認証結果が `clientid_override` を省略または空値で返した場合、EMQX は以前に決定した Client ID を保持します。

### 認証前 Client ID 分離の設定

ダッシュボードで Client ID Isolation を有効にすると、EMQX は `mqtt.clientid_override` を設定し、推奨式を自動的に入力します：

```
concat([client_attrs.tns, '-', clientid])
```

::: warning 重要なお知らせ

EMQX 6.3.0 以降、`mqtt.clientid_override` 式がエラーを起こすか空文字を返すと、EMQX はエラーログを出力し接続を拒否します。MQTT 5.0 クライアントは CONNACK 理由コード `0x85`（Client Identifier not valid）、MQTT 3.1 および 3.1.1 クライアントはリターンコード `2` を受け取ります。EMQX はクライアントが送信した Client ID にフォールバックしません。

アップグレード前に、すべての接続クライアントが設定された式を空でない文字列に評価できることを確認してください。評価できないクライアントのために式や必要なクライアントデータを修正してください。

:::

この設定により：

- 異なるネームスペースのクライアントが同じ Client ID を安全に使用可能になります。
- 内部的に使用される Client ID は常にネームスペースのプレフィックスを含みます。

この式は認証前に解決されたネームスペース向けの例として提供されています。結果の Client ID がグローバルに一意である限り、ビジネス要件に合わせてカスタマイズ可能です。

### 動作例

ユーザー名からネームスペースを抽出するソースが設定されていると仮定します：

```
nth(1, tokens(username, '-'))
```

Client ID 分離はデフォルト式で有効化されています：

```
concat([client_attrs.tns, '-', clientid])
```

#### クライアント接続情報

| クライアント | ユーザー名       | Client ID |
| ------------ | ---------------- | --------- |
| A            | tenantA-user1    | client1   |
| B            | tenantB-user2    | client1   |

#### 内部で使用される Client ID

| ネームスペース | 元の Client ID | 実際の Client ID    |
| -------------- | -------------- | ------------------- |
| tenantA        | client1        | tenantA-client1     |
| tenantB        | client1        | tenantB-client1     |

## ネームスペースをマウントポイントとして使用

有効にすると、EMQX はネームスペースをトピックのマウントポイントとして使用します。これによりネームスペース単位のトピック分離が可能になります。

リスナーにすでに `mountpoint` が設定されている場合、この設定は無視され、リスナーの設定が優先されます。

### 動作

**Namespace as Mountpoint** を有効にすると、EMQX は以下のようにトピックを分離します：

- `PUBLISH`、`SUBSCRIBE`、`UNSUBSCRIBE`、および Will メッセージ処理時に：
  - EMQX は内部的にトピックの先頭に `{namespace}/` を自動で付加します。
- クライアントへのメッセージ配信時に：
  - ネームスペースのプレフィックスを自動的に取り除きます。
- クライアントから見た場合：
  - パブリッシュおよびサブスクライブするトピック名は変更されません。
  - クライアントはネームスペースプレフィックスを認識しません。

### 例

クライアントがネームスペース `n1` に属し、**Namespace as Mountpoint** が有効な場合。

#### クライアント側の動作

- クライアントは `sensors/#` をサブスクライブ
- クライアントは `sensors/data` にパブリッシュ

#### EMQX 内部処理

- ブローカーはサブスクリプションを `n1/sensors/#` として登録
- ブローカーはメッセージを `n1/sensors/data` でルーティング
- メッセージはクライアントに `sensors/data` として配信

結果として：

- ネームスペースプレフィックスは内部処理でのみ使用
- クライアントは常に元のトピック名で操作
- 異なるネームスペースのクライアントが同じトピックを使っても互いのメッセージを受け取らない

## 認可のためのマウントプレフィックス

この設定は、認可（ACL）チェック前にトピックマウントポイントのプレフィックスを対象のトピックやトピックフィルターに付加するかどうかを制御します。

マウントポイントプレフィックスは通常、**Namespace as Mountpoint** が有効な場合にネームスペースから取得され、以下の形式です：

```
{namespace}/
```

### 動作

**Mount Prefix for Authorization** が有効な場合：

- EMQX は ACL ルールや認可バックエンドのマッチング前に、トピックマウントポイントを対象トピックまたはフィルターの先頭に付加します。
- 認可チェックはプレフィックス付きトピックで行われます。

この動作は以下の操作に適用されます：

- `PUBLISH`
- `SUBSCRIBE`
- `UNSUBSCRIBE`
- Will メッセージ

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

#### 認可で使用されるトピック

認可時に EMQX は `n1/sensors/#` を評価します。したがって対応する ACL ルールは `sensors/#` ではなく `n1/sensors/#` として定義する必要があります。

### 推奨

トピック分離のために **Namespace as Mountpoint** を有効にしている場合は、**Mount Prefix for Authorization** も有効にすることを推奨します。これにより、認可チェックがブローカー内部で使用されるトピック名と一致し、認可結果と実際のメッセージルーティングの不整合を防げます。
