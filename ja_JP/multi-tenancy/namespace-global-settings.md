# グローバルネームスペース設定

EMQX 6.1では、個別のネームスペースインスタンスの設定に加えて、ネームスペースの識別方法、分離動作の適用方法、トピックおよび認可の取り扱いを制御する一連のグローバルネームスペース設定が利用可能です。

これらの設定はクラスター全体に適用され、すべてのネームスペースおよびクライアント接続に影響します。通常、ネームスペース関連機能の有効化および使用前に設定されます。

グローバルネームスペース設定はダッシュボードの **Management** -> **Namespace** -> **Settings** から管理できます。

::: tip 注意事項

後方互換性を保つため、EMQX 6.1のほとんどのグローバルネームスペース設定（Client ID Isolation、Namespace as Mountpoint、Mount Prefix for Authorizationなど）はデフォルトで無効になっています。

対応する分離機能を有効にするには、**Namespace Related Configurations** で明示的にオンにする必要があります。

:::

![拒否されたネームスペース名を含むグローバルネームスペース設定](./assets/namespace_global_settings.png)

## 明示的に作成されたネームスペースのみ許可

この設定は、クライアントが明示的に作成されたネームスペースにのみ接続を許可するかどうかを制御します。設定ファイルの `multi_tenancy.allow_only_managed_namespaces` に対応しています。

この設定を有効にすると、EMQXは接続プロセス中にクライアントのネームスペースを検証し、接続を許可するか拒否するかを判断します。

- **有効**:
  - ダッシュボードやREST APIで明示的に作成されていないネームスペースに属するクライアントは接続が拒否されます。
  - ネームスペースが解決できないクライアント（例えば、ネームスペースソースが設定されていない、または有効な値を生成しない場合）も接続が拒否されます。
- **無効**:
  - 明示的に作成されていないネームスペースへの接続も許可されます。
  - ネームスペースソースが設定されている場合、EMQXは必要に応じてネームスペースを自動的に作成することがあります。

::: tip 注意事項

この設定を有効にする前に、**Take Namespace From** が正しく設定されており、すべての有効なクライアントが明示的に作成されたネームスペースを正常に解決できることを確認してください。そうでない場合、ネームスペースが解決できないか明示的に作成されていないためにクライアントが拒否される可能性があります。

**When to Resolve Namespace** で **After Authentication** モードを選択した場合、認証前のネームスペースチェックはスキップされます。明示的に作成されたネームスペースとの照合は認証完了後に実行されます。

:::

## デフォルトの最大セッション数

この設定は、新規作成されたネームスペースのデフォルトの最大同時セッション数を定義します。

- **有効**:
  - 新規作成されたネームスペースは自動的にこの最大セッション制限を継承します。
- **無効**:
  - 新規作成されたネームスペースにはデフォルトでセッション制限がなく（`infinity`）、無制限となります。

この設定は設定適用後に作成されたネームスペースにのみ適用されます。既存のネームスペースには影響せず、必要に応じて個別に更新する必要があります。

## 拒否されるネームスペース名

EMQX 6.3.0以降、`multi_tenancy.deny_namespaces` はネームスペース識別子として使用できない名前を指定します。この制限はダッシュボードのユーザーロール、APIキー、管理APIによるネームスペース作成および一括インポート、`client_attrs.tns` を介したクライアントのネームスペース割り当てに適用されます。

デフォルトリストは `["global", "undefined", "null", "none"]` です。これらの名前はログやダッシュボードの出力で内部識別子と混同される可能性があります。

ダッシュボードでリストを編集するには：

1. **Management** -> **Namespace** -> **Settings** に移動します。
2. **Denied Namespace Names** に必要に応じて名前を追加または削除します。すべてのエントリーをクリアすると名前制限が無効になります。
3. **Confirm** をクリックして変更を適用します。

`etc/base.hocon` でもリストを設定可能です。以下はデフォルト値の例です：

```hocon
multi_tenancy.deny_namespaces = ["global", "undefined", "null", "none"]
```

カスタムリストはデフォルトリストを置き換えます。拒否したいデフォルト名を含めてください。名前制限を無効にするには `multi_tenancy.deny_namespaces = []` と設定します。設定ファイルの優先順位については [Config Override Rules](../configuration/configuration.md#config-override-rules) を参照してください。

EMQXは、**Allow Only Explicitly Created Namespaces** が無効でも、解決されたネームスペースがリストに含まれている場合、`not_authorized` でクライアント接続を拒否します。この制限は、ネームスペースを持たないクライアントの接続を妨げるものではありません（`multi_tenancy.allow_only_managed_namespaces = false` の場合）。

::: warning 重要なお知らせ

デフォルトリストはEMQX 6.3.0以前で許可されていた名前を拒否します。EMQXはこれらの名前を使用しているネームスペースを自動的に移行しません。アップグレード前に、影響を受けるネームスペース名を変更するか、`multi_tenancy.deny_namespaces` を調整して許可してください。

:::

## ネームスペースを解決するタイミング

この設定は、接続ライフサイクルのどの時点でEMQXがクライアントのネームスペース識別子を解決するかを制御します。

EMQXはダッシュボードの **When to Resolve Namespace** ラジオボタンで選択可能な2つのモードをサポートしています：

- **Before Authentication**（デフォルト）：認証チェーン実行前にネームスペース式を評価します。この時点で利用可能な接続メタデータ（`username`、`clientid`、`cert_common_name`など）のみを使用します。設定ファイルの `mqtt.client_attrs_init` で `tns` を設定することに対応します。
- **After Authentication**：認証チェーン完了後にネームスペース式を評価します。標準の接続メタデータに加え、認証バックエンドから返された属性を含む `client_attrs.*` の値も利用可能です（例：HTTP認証レスポンスの `tag` フィールドなど）。設定ファイルの `multi_tenancy.post_auth_tns_expression` に対応します。

::: tip

**After Authentication** が設定されている場合、EMQXはポスト認証式を使ってネームスペースを割り当てます。ポスト認証式が空文字を返すか失敗しても、プリ認証の `tns` 値はフォールバックとして使用されません。[Empty or Failed Post-authentication Expressions](#empty-or-failed-post-authentication-expressions) を参照してください。

:::

### 明示的に作成されたネームスペースのみ許可との連携

**After Authentication** モードが選択されている場合、**Allow Only Explicitly Created Namespaces** が有効でも認証前のネームスペースチェックは完全にスキップされます。解決されたネームスペースの存在確認やクォータチェックなどのすべての適用は、認証完了後に最終的なネームスペース値が判明してから行われます。

## Take Namespace From

この設定は、EMQXがクライアントのネームスペース識別子（`client_attrs.tns`）を導出するために使用するVariform式を指定します。

式は **When to Resolve Namespace** 設定で決まる接続ライフサイクルの時点で評価されます：

- **Before Authentication** モードでは、`username`、`clientid`、`cert_common_name` などの標準接続メタデータと認証前属性のみが利用可能です。
- **After Authentication** モードでは、認証結果からマージされた属性を含む `client_attrs.*` も利用可能です。

::: tip

**Take Namespace From** 式はVariform構文を使用します。利用可能な関数の詳細は [Variform Expressions](../configuration/configuration.md#variform-expressions) を参照してください。

:::

この設定は以下の機能の前提条件です：

- 自動ネームスペース作成
- ネームスペースベースのトピック分離
- ネームスペースベースのClient ID分離
- ネームスペースレベルのセッション制限およびレート制限

**Take Namespace From** が設定されていない場合、`tns` 属性は生成されません。この場合、クライアントはどのネームスペースにも関連付けられず、ネームスペース関連の分離および制御機能はすべて無効のままになります。

### 例

#### 認証前

ユーザー名からネームスペースを抽出する例：

```text
nth(1, tokens(username, '-'))
```

この設定では、ユーザー名が `tenantA-user1` のクライアントは、認証前に `tenantA` がネームスペース識別子として割り当てられます。

#### 認証後

HTTP認証バックエンドから返された `tag` 属性を使用する例：

```text
client_attrs.tag
```

認証バックエンドがタグを返さない場合のフォールバック：

```text
coalesce(client_attrs.tag, username)
```

この設定では、EMQXは認証チェーンの完了を待ち、マージされた `client_attrs` から `tag` 値を読み取り、それをネームスペース識別子として割り当てます。

### 空または失敗したポスト認証式

`multi_tenancy.post_auth_tns_expression` が設定されているが空文字を返すか評価に失敗した場合、EMQXは以下のように接続を処理します。評価失敗時には警告ログも出力されます。

1. プリ認証の `client_attrs.tns` 値が `multi_tenancy.deny_namespaces` に含まれる場合、EMQXは `not_authorized` で接続を拒否します。
2. それ以外の場合、EMQXはクライアントをネームスペースなしとして扱います：
   - `multi_tenancy.allow_only_managed_namespaces = true` の場合、`not_authorized` で接続を拒否します。
   - `multi_tenancy.allow_only_managed_namespaces = false` の場合、プリ認証の `tns` 値を削除し、ネームスペースなしでの接続を許可します。

## Client ID Isolation

Client ID Isolationは、異なるネームスペースのクライアントが同じClient IDを使用した場合の競合を防止します。

有効にすると、EMQXは内部的にクライアントのClient IDにネームスペースをプレフィックスとして付加しますが、クライアントから提供された元のClient IDは変更されません。

Client ID Isolationが有効な場合、ダッシュボードは推奨されるデフォルト式を自動的に入力します：

```
concat([client_attrs.tns, '-', clientid])
```

この設定により：

- 異なるネームスペースのクライアントが同じClient IDを安全に使用できます。
- 内部的に使用されるClient IDは常にネームスペースプレフィックスを含みます。

この式は例示であり、結果のClient IDがグローバルに一意である限り、ビジネス要件に応じてカスタマイズ可能です。

### 動作例

ユーザー名からネームスペースを抽出するネームスペースソースが設定されているとします：

```
nth(1, tokens(username, '-'))
```

Client ID Isolationはデフォルト式で有効化されています：

```
concat([client_attrs.tns, '-', clientid])
```

#### クライアント接続情報

| クライアント | ユーザー名       | Client ID |
| ------------ | ---------------- | --------- |
| A            | tenantA-user1    | client1   |
| B            | tenantB-user2    | client1   |

#### 内部的に使用されるClient ID

| ネームスペース | 元のClient ID | 実際のClient ID    |
| -------------- | ------------- | ------------------ |
| tenantA        | client1       | tenantA-client1    |
| tenantB        | client1       | tenantB-client1    |

## Namespace as Mountpoint

有効にすると、EMQXはネームスペースが正常に解決された後にクライアントのネームスペースをトピックのマウントポイントとして使用します。これによりネームスペース単位のトピック分離が可能になります。

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

クライアントがネームスペース `n1` に属し、**Namespace as Mountpoint** が有効な場合。

#### クライアント側の動作

- クライアントは `sensors/#` をサブスクライブ
- クライアントは `sensors/data` にパブリッシュ

#### EMQX内部処理

- ブローカーはサブスクリプションを `n1/sensors/#` として登録
- ブローカーはメッセージを `n1/sensors/data` でルーティング
- メッセージはクライアントに `sensors/data` として配信

結果として：

- ネームスペースプレフィックスは内部でのみ使用されます。
- クライアントは常に元のトピック名で操作します。
- 異なるネームスペースのクライアントが同じトピック名を使用しても、お互いのメッセージを受信しません。

## Mount Prefix for Authorization

この設定は、認可（ACL）チェックの前にトピックマウントポイントのプレフィックスを対象トピックやトピックフィルターに追加するかどうかを制御します。

マウントポイントプレフィックスは通常、**Namespace as Mountpoint** が有効な場合のネームスペースから取得され、以下の形式になります：

```
{namespace}/
```

### 動作

**Mount Prefix for Authorization** が有効な場合：

- EMQXはACLルールや認可バックエンドのマッチング前にトピックマウントポイントを対象トピックまたはトピックフィルターに付加します。
- 認可チェックはプレフィックス付きトピックで行われます。

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

#### クライアントの操作

クライアントは以下をサブスクライブしようとします：

```
sensors/#
```

#### 認可に使用されるトピック

認可時にEMQXは `n1/sensors/#` を評価します。したがって、対応するACLルールは `sensors/#` ではなく `n1/sensors/#` として定義する必要があります。

### 推奨

トピック分離のために **Namespace as Mountpoint** を有効にしている場合、**Mount Prefix for Authorization** も有効にすることを推奨します。これにより、認可チェックがブローカー内部で使用されるトピック名と一致し、認可結果と実際のメッセージルーティングの不整合を防止できます。
