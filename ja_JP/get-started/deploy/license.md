---
title: EMQX Enterprise ライセンスの利用
description: EMQX Enterprise には、限定的な商用利用権を持つシングルノードの Community ライセンスが含まれています。ただし、EMQX Enterprise を商用利用やクラスター展開で使用する場合は、商用ライセンスの取得が必要です。
---

# EMQX Enterprise ライセンスの利用

EMQX 5.9 以降、EMQX は Business Source License (BSL) 1.1 のもとでリリースされており、オープンな開発を可能にしつつ、EMQX の商用利用を保護しています。

::: tip

ライセンス変更の詳細については、[EMQX ライセンス FAQ](https://www.emqx.com/en/content/license-faq) をご覧ください。

:::

インストールパッケージには、限定的な商用利用権を持つシングルノード Community ライセンスが含まれています。しかし、EMQX Enterprise を完全な商用利用やクラスター展開で使用する場合は、商用ライセンスの取得が必要です。

本ページでは、商用ライセンスの取得方法と EMQX へのインポート手順をご案内します。

## ライセンスの申請

有効なライセンスキーを伴う商用ライセンスを申請するには、EMQ の営業担当者にご連絡いただくか、[お問い合わせ](https://www.emqx.com/en/contact?product=emqx&channel=apply-Licenses) ページの連絡先情報を入力して商用ライセンスを申請してください。営業担当者より折り返しご連絡いたします。

購入前に EMQX Enterprise を試用したい場合は、[トライアルライセンス申請ページ](https://www.emqx.com/en/apply-licenses/emqx) からトライアルライセンスを申請できます。申請後、ライセンスファイルがすぐにメールで送付されます。

- トライアルライセンスの有効期限は15日間です。
- トライアルライセンスは最大10,000の同時セッションをサポートします。

::: tip 注意

トライアル期間中は EMQX Enterprise の全機能が利用可能です。ただし、トライアル期間終了後はクラスター機能が無効になります。クラスター機能を継続して利用するには商用ライセンスの購入が必要です。

トライアルライセンスの EMQX Enterprise は本番環境での利用は許可されていません。

:::

トライアル期間の延長を希望される場合は、営業部門までご連絡ください。

## ライセンス設定の更新と構成

ライセンスファイルの更新やライセンス接続クォータ使用量の設定は、EMQX ダッシュボード、コマンドラインインターフェース（CLI）、または設定ファイルから行えます。

### ダッシュボード

1. EMQX ダッシュボードの左側ナビゲーションメニューから **System** -> **License** をクリックします。License ページの **Basic Info** セクションで、ライセンス接続クォータ使用量、EMQX バージョン、発行情報などを確認できます。

2. **Update License** ボタンをクリックします。ポップアップダイアログにライセンスキーを貼り付けて **Save** をクリックします。送信後、ページのライセンス情報が自動的に更新されます。

   新しいライセンスファイルが有効になったことを確認してください。

3. **License Settings** セクションで、ライセンス接続クォータ使用量のウォーターマーク制限を設定できます。

   - **Usage High Watermark**：ライセンス接続クォータ使用量のアラームが発動する閾値のパーセンテージを指定します。
   - **Usage Low Watermark**：ライセンス接続クォータ使用量のアラームが解除される閾値のパーセンテージを指定します。

4. **Save Changes** をクリックしてライセンス設定を保存します。

   <img src="./assets/license.png" alt="ライセンス" style="zoom: 50%;" />

#### Community ライセンスへの戻し方

EMQX ダッシュボードでは、デフォルトのシングルノード Community ライセンスに戻すことが可能です。**License** ページの **Remove License** ボタンをクリックし、ポップアップで確認すると現在のライセンスが削除されます。

::: tip 注意

クラスター運用中はライセンスを削除できません。クラスター運用中の場合は、まずクラスターを解散してください。

:::

Community ライセンスに戻した場合：

- 現在のライセンスはクリアされ、Community ライセンスに置き換えられます。
- 既存のクライアント接続は維持されます。

::: tip 注意

Community ライセンスは完全な商用利用を許可しておらず、シングルノード展開のみをサポートします。ライセンスを削除するとクラスター展開は無効になります。

:::

### CLI

以下のコマンドでも EMQX Enterprise ライセンスの更新が可能です。

```bash
./bin/emqx ctl 

    license info             # ライセンス情報を表示
    license update <License> # 文字列としてライセンスを更新
    license update default   # デフォルトの Community ライセンスに戻す
```

### 設定ファイル

設定ファイルでライセンスファイルを指定することもできます。設定後、[EMQX コマンドラインツール](../admin/cli.md)で `emqx ctl license reload` を実行してライセンスをリロードしてください。

```bash
license {
    ## ライセンスキー
    key = "MjIwMTExCjAKMTAKRXZhbHVhdGlvbgpjb250YWN0QGVtcXguaW8KZGVmYXVsdAoyMDIzMDEwOQoxODI1CjEwMAo=.MEUCIG62t8W15g05f1cKx3tA3YgJoR0dmyHOPCdbUxBGxgKKAiEAhHKh8dUwhU+OxNEaOn8mgRDtiT3R8RZooqy6dEsOmDI="
    ## ライセンス接続クォータ使用量アラームが解除される低ウォーターマーク閾値
    connection_low_watermark = "75%"

    ## ライセンス接続クォータ使用量アラームが発動する高ウォーターマーク閾値
    connection_high_watermark = "80%"
}
```

実行後、`emqx ctl license info` を実行して新しいライセンスファイルが有効になっていることを確認できます。

<!-- 環境変数 `EMQX_LICENSE__KEY` でもライセンスを設定可能です。TODO: リロード可能か確認 -->
