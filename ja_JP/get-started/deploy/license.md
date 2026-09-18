---
title: EMQX Enterprise ライセンスの取り扱い
description: EMQX Enterprise には限定的な商用利用が許可されたシングルノードの Community ライセンスが含まれています。ただし、EMQX Enterprise を本格的な商用利用やクラスター展開で使用する場合は商用ライセンスの取得が必要です。
---

# EMQX Enterprise ライセンスの取り扱い

EMQX 5.9 以降、EMQX は Business Source License (BSL) 1.1 のもとでリリースされており、これはソースコードの公開を維持しつつ、EMQX の商用利用を保護するライセンスです。

::: tip

ライセンス変更の詳細については、[EMQX ライセンス FAQ](https://www.emqx.com/en/content/license-faq) をご参照ください。

:::

インストールパッケージには、限定的な商用利用が許可されたシングルノードの Community ライセンスが含まれています。しかし、EMQX Enterprise を本格的な商用利用やクラスター展開で使用する場合は、商用ライセンスの取得が必須です。

本ページでは、商用ライセンスの取得方法と EMQX へのインポート手順について説明します。

## ライセンスの申請

有効なライセンスキーを伴う商用ライセンスを申請するには、EMQ の営業担当者にご連絡いただくか、[お問い合わせ](https://www.emqx.com/en/contact?product=emqx&channel=apply-Licenses) ページの連絡先情報を入力して商用ライセンスを申請してください。営業担当者より折り返しご連絡いたします。

購入前に EMQX Enterprise を試用したい場合は、[トライアルライセンス申請ページ](https://www.emqx.com/en/apply-licenses/emqx) からトライアルライセンスを申請できます。ライセンスファイルは即座にメールで送付されます。

- トライアルライセンスの有効期限は15日間です。
- トライアルライセンスは最大10,000同時セッションをサポートします。

::: tip 注意

トライアル期間中は EMQX Enterprise の全機能が利用可能です。ただし、トライアル期間終了後はクラスタリング機能が無効になります。クラスタリング機能を継続利用するには商用ライセンスの購入が必要です。

トライアルライセンスの EMQX Enterprise は本番環境での利用は許可されていません。

:::

トライアル期間の延長をご希望の場合は、営業部門までご連絡ください。

## ライセンス設定の更新と構成

ライセンスファイルの更新やライセンス接続クォータ使用量の設定は、EMQX ダッシュボード、コマンドラインインターフェース（CLI）、または設定ファイルを通じて行えます。

### ダッシュボード

1. EMQX ダッシュボードの左側ナビゲーションメニューから **System** -> **License** をクリックします。ライセンスページの **Basic Info** セクションで、ライセンス接続クォータ使用量、EMQX バージョン、発行情報などを確認できます。

2. **Update License** ボタンをクリックします。ポップアップダイアログにライセンスキーを貼り付けて **Save** をクリックしてください。送信後、ページ上のライセンス情報が自動的に更新されます。

   新しいライセンスファイルが有効になっているか情報を確認してください。

3. **License Settings** セクションで、ライセンス接続クォータ使用量のウォーターマーク制限を設定できます。

   - **Usage High Watermark**：ライセンス接続クォータ使用量がこの閾値を超えた場合にアラームを発動する割合（パーセンテージ）を指定します。
   - **Usage Low Watermark**：ライセンス接続クォータ使用量がこの閾値を下回った場合にアラームを解除する割合（パーセンテージ）を指定します。

4. **Save Changes** をクリックしてライセンス設定を保存します。

   <img src="./assets/license.png" alt="ライセンス" style="zoom: 50%;" />

#### Community ライセンスへの戻し方

EMQX ダッシュボードでは、ユーザーがデフォルトのシングルノード Community ライセンスに戻すことが可能です。**License** ページの **Remove License** ボタンをクリックし、ポップアップで確認すると現在のライセンスが削除されます。

::: tip 注意

クラスター モードではライセンスの削除はできません。クラスター モードで EMQX を使用している場合は、まずクラスターを解散する必要があります。

:::

Community ライセンスに戻した後は：

- 現在のライセンスはクリアされ、Community ライセンスに置き換えられます。
- 既存のクライアント接続は維持されます。

::: tip 注意

Community ライセンスは本格的な商用利用を許可しておらず、シングルノード展開のみをサポートします。ライセンスを削除するとクラスター展開は無効になります。

:::

### CLI

以下のコマンドでも EMQX Enterprise ライセンスの更新が可能です。

```bash
./bin/emqx ctl

    license info             # ライセンス情報を表示
    license update <License> # 文字列として与えられたライセンスを更新
    license update default   # デフォルトの Community ライセンスに戻す
```

### 設定ファイル

設定ファイルでライセンスファイルを指定することもできます。設定後、[EMQX コマンドラインツール](../../guides/cli.md) で `emqx ctl license reload` を実行してライセンスをリロードしてください。

```bash
license {
    ## ライセンスキー
    key = "MjIwMTExCjAKMTAKRXZhbHVhdGlvbgpjb250YWN0QGVtcXguaW8KZGVmYXVsdAoyMDIzMDEwOQoxODI1CjEwMAo=.MEUCIG62t8W15g05f1cKx3tA3YgJoR0dmyHOPCdbUxBGxgKKAiEAhHKh8dUwhU+OxNEaOn8mgRDtiT3R8RZooqy6dEsOmDI="
    ## ライセンス接続クォータ使用量アラーム解除の閾値（低ウォーターマーク）
    connection_low_watermark = "75%"

    ## ライセンス接続クォータ使用量アラーム発動の閾値（高ウォーターマーク）
    connection_high_watermark = "80%"
}
```

実行後、`emqx ctl license info` を実行して新しいライセンスファイルが有効になっていることを確認できます。

<!-- 環境変数 `EMQX_LICENSE__KEY` を使用してライセンスを設定することも可能です。TODO: リロード可能か確認 -->
