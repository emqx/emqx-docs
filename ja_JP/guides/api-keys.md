# APIキー

EMQXダッシュボードの**APIキー**ページでは、[HTTP API](../guides/api.md)リクエストの認証に使用するAPIキーとシークレットキーを生成できます。

## APIキーの作成

1. ダッシュボードで **System** -> **API Key** に移動します。

2. 右上の **+ Create** ボタンをクリックして、APIキー作成ダイアログを開きます。

3. APIキーの詳細を設定します：

   - **Expire At** フィールドは、キーを無期限にしたい場合は空欄のままにします。
   - （オプション）APIキーのロールを選択します（EMQX Enterpriseのみ）。利用可能なロールの詳細は、[Roles and Permissions](../guides/api.md#roles-and-permissions)を参照してください。

4. **Confirm** をクリックします。APIキーとシークレットキーが **Created Successfully** ダイアログに表示されます。

   ::: warning 重要なお知らせ

   APIキーとシークレットキーはすぐに安全な場所に保存してください。このダイアログを閉じるとシークレットキーは再表示されません。

   :::

5. **Close** をクリックしてダイアログを閉じます。

<img src="./dashboard/assets/api-key.png" alt="APIキー作成画面" style="zoom:67%;" />

## APIキーの管理

APIキーを作成した後は、APIキー一覧ページで管理できます：

- **詳細表示**：**Name** 列のキー名をクリックします。
- **編集**：**Actions** 列の **Edit** ボタンをクリックして、有効期限のリセット、有効/無効の切り替え、メモの更新ができます。
- **削除**：不要になったAPIキーは、**Actions** 列の **Delete** ボタンで削除できます。

<img src="./dashboard/assets/api-key-detail.png" alt="APIキー詳細画面" style="zoom:50%;" />
