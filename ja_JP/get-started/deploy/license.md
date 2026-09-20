---
title: EMQX Enterprise ライセンスの利用
description: EMQX Enterprise には限定的な商用利用が許可されたシングルノード Community ライセンスが含まれています。ただし、完全な商用利用やクラスター展開には商用ライセンスが必要です。
---

# EMQX Enterprise ライセンスの利用

EMQX 5.9 以降、EMQX は Business Source License (BSL) 1.1 のもとでリリースされており、ソースコードの公開開発を可能にしつつ、EMQX の商用利用を保護しています。

::: tip

ライセンス変更の詳細については、[EMQX ライセンス FAQ](https://www.emqx.com/en/content/license-faq) をご参照ください。

:::

EMQX Enterprise のインストールパッケージには、限定的な商用利用が許可されたシングルノード Community ライセンスが含まれています。ただし、EMQX Enterprise を完全な商用利用やクラスター展開で使用する場合は、商用ライセンスを取得する必要があります。

本ページでは、商用ライセンスの取得と EMQX へのインポート手順について説明します。

## ライセンスの申請

有効なライセンスキーを持つ商用ライセンスを申請するには、EMQ の営業担当者にお問い合わせいただくか、[お問い合わせ](https://www.emqx.com/en/contact?product=emqx&channel=apply-Licenses) ページの連絡先情報を入力して商用ライセンスを申請してください。営業担当者より折り返しご連絡いたします。

購入前に EMQX Enterprise を試用したい場合は、[トライアルライセンス申請ページ](https://www.emqx.com/en/apply-licenses/emqx) からトライアルライセンスを申請できます。ライセンスファイルは即座にメールで送付されます。

- トライアルライセンスの有効期限は15日間です。
- トライアルライセンスは最大10,000同時セッションをサポートします。

::: tip 注意

トライアル期間中は EMQX Enterprise のすべての機能が利用可能です。ただし、トライアル期間終了後はクラスター機能が無効になります。クラスター機能を継続して使用するには商用ライセンスの購入が必要です。

トライアルライセンスの EMQX Enterprise は本番環境での利用は許可されていません。

:::

トライアル期間の延長を希望される場合は、営業部門までご連絡ください。

## ライセンス設定の更新と構成

ライセンスファイルの更新やライセンス接続クォータ使用量の設定は、EMQX ダッシュボード、コマンドラインインターフェース（CLI）、または設定ファイルから行えます。

### ダッシュボード

1. EMQX ダッシュボードの左側ナビゲーションメニューから **System** -> **License** をクリックします。ライセンスページの **Basic Info** セクションで、ライセンス接続クォータ使用量、EMQX バージョン、発行情報などを確認できます。

2. **Update License** ボタンをクリックします。ポップアップダイアログにライセンスキーを貼り付け、**Save** をクリックしてください。送信後、ページ上のライセンス情報が自動的に更新されます。

   情報を確認し、新しいライセンスファイルが有効になっていることを確かめてください。

3. **License Settings** セクションでは、ライセンスセッションクォータ使用量のウォーターマーク閾値を設定できます。セッション制限の詳細は[セッション制限](#session-limits)をご覧ください。

   - **Usage High Watermark**: ライセンスセッションクォータ使用量のアラームを発動する閾値のパーセンテージを指定します。
   - **Usage Low Watermark**: アラームを解除する閾値のパーセンテージを指定します。

4. **Save Changes** をクリックしてライセンス設定を保存します。

   <img src="./assets/license.png" alt="ライセンス" style="zoom: 50%;" />

#### Community ライセンスへの戻し方

EMQX ダッシュボードでは、デフォルトのシングルノード Community ライセンスに戻すことができます。**License** ページの **Remove License** ボタンをクリックし、ポップアップで確認すると現在のライセンスが削除されます。

::: tip 注意

クラスター モードではライセンスを削除できません。クラスター モードで EMQX を使用している場合は、まずクラスターを解散する必要があります。

:::

Community ライセンスに戻した後は：

- 現在のライセンスがクリアされ、Community ライセンスに置き換わります。
- 既存のクライアント接続は維持されます。

::: tip 注意

Community ライセンスは完全な商用利用を許可しておらず、シングルノード展開のみをサポートします。ライセンスを削除するとクラスター展開は無効になります。

:::

### CLI

以下のコマンドでも EMQX Enterprise ライセンスを更新できます。

```bash
./bin/emqx ctl

    license info             # ライセンス情報を表示
    license update <License> # ライセンスキー文字列で更新
    license update default   # デフォルトの Community ライセンスに戻す
```

### 設定ファイル

設定ファイルでライセンスファイルを指定することも可能です。設定後、[EMQX コマンドラインツール](../../guides/cli.md)で `emqx ctl license reload` を実行してライセンスをリロードしてください。

```bash
license {
    ## ライセンスキー
    key = "MjIwMTExCjAKMTAKRXZhbHVhdGlvbgpjb250YWN0QGVtcXguaW8KZGVmYXVsdAoyMDIzMDEwOQoxODI1CjEwMAo=.MEUCIG62t8W15g05f1cKx3tA3YgJoR0dmyHOPCdbUxBGxgKKAiEAhHKh8dUwhU+OxNEaOn8mgRDtiT3R8RZooqy6dEsOmDI="
    ## ライセンス接続クォータ使用量アラームを解除する低ウォーターマーク閾値
    connection_low_watermark = "75%"

    ## ライセンス接続クォータ使用量アラームを発動する高ウォーターマーク閾値
    connection_high_watermark = "80%"
}
```

実行後、`emqx ctl license info` を実行して新しいライセンスファイルが有効になっていることを確認できます。

<!-- 環境変数 `EMQX_LICENSE__KEY` でもライセンスを設定可能か確認中 -->

## ライセンス制限

EMQX Enterprise ライセンスには、本番環境でのライセンス条件の遵守を保証するための使用制限が含まれる場合があります。主な制限は以下の通りです。

- セッション制限
- TPS 制限（EMQX 6.0 以降）

### セッション制限

セッション制限は、現在のライセンス下で EMQX Enterprise がサポート可能な同時 MQTT クライアント接続（セッション）の最大数を定義します。

- 制限に達すると、新規接続は拒否されます。
- ライセンスクォータを超えた接続を試みたクライアントには、CONNACK 理由コード `151 (0x97)` の「Quota Exceeded」レスポンスが返されます。
- セッション使用量が設定された高ウォーターマーク閾値を超えるとアラームが発生します。
- 使用量が低ウォーターマーク閾値を下回るとアラームは自動的に解除されます。

アラームのウォーターマークは EMQX ダッシュボードまたは設定ファイルで設定可能です。

### セッション高ウォーターマーク履歴

EMQX Enterprise はクラスター全体の毎日のピークセッション数を自動的に記録し、最大24ヶ月分の履歴を保持します。このデータは複製され整合性が保たれた内部テーブルに保存され、ノードの再起動やクラスターのトポロジー変更をまたいで永続化されるため、課金決済の監査用データとして利用できます。

#### CLI

記録された履歴は `emqx ctl license history` で照会できます。

```bash
# 月間ピーク（デフォルト）
emqx ctl license history

# 過去7日間の日間ピーク
emqx ctl license history 7 --period daily

# JSON 出力
emqx ctl license history --json
```

コマンドの詳細は [license history](../../guides/cli.md#license-history) をご覧ください。

#### REST API

```bash
GET /api/v5/license/session_hwm_history
```

**クエリパラメータ**

| パラメータ | 型 | デフォルト | 説明 |
| --------- | ---- | ------- | ----------- |
| `period` | `daily` \| `monthly` | `daily` | 集計粒度。`daily` はカレンダー日ごとに1行、`monthly` は日間ピークを月ごとの最大値に集約します。 |
| `limit` | 整数 | `30` | 返却する最大行数。`daily` のみ適用。`monthly` は最大24ヶ月分の全データを返します。 |

**レスポンス例**

以下は月間集計を明示的に指定した例です。

```bash
GET /api/v5/license/session_hwm_history?period=monthly
```

```json
{
  "period": "monthly",
  "count": 2,
  "data": [
    { "period": "2026-04", "high_watermark": 25000, "observed_at": "2026-04-18T13:53:05.000Z" },
    { "period": "2026-03", "high_watermark": 23500, "observed_at": "2026-03-31T22:10:42.000Z" }
  ]
}
```

各レコードには以下が含まれます：

- `period`: 要求された期間に応じたカレンダー日（`YYYY-MM-DD`）または月（`YYYY-MM`）
- `high_watermark`: 期間中に観測されたピークセッション数
- `observed_at`: ピーク観測の RFC 3339 タイムスタンプ

#### タイムゾーン設定

日付の境界は `license.high_watermark_timezone` 設定フィールドで決定されます。デフォルトはノードホストのローカルタイムゾーン（`"system"`）です。異なる地域のノード間で日付境界を統一したい場合は、明示的に UTC オフセット（例：`"+08:00"`）を設定してください。詳細は[ライセンス設定](../../guides/configuration/license.md)をご参照ください。

### TPS 制限

EMQX 6.0 以降、ライセンスには TPS（Transactions Per Second）制限が含まれる場合があります。この制限はクラスター全体で処理される MQTT メッセージの総数（受信・送信両方）に適用されます。

- TPS 使用量がライセンス制限を超えるとアラームが発生します。
- アラームはピーク TPS を記録しますが、メッセージの流量を制限することはありません。
- アラームは以下のいずれかで解除されるまで継続します：
  - より高い TPS 制限を持つ新しいライセンスが適用された場合
  - EMQX ダッシュボードまたは CLI から手動でアラームが解除された場合

この TPS 制限は厳密な制御ではなく、可観測性とコンプライアンスのためのものです。

::: tip 注意

TPS 制限はライセンスで定義されており、ユーザーが設定や調整を行うことはできません。制限を引き上げるには、より高い TPS 値を持つ新しいライセンスを適用してください。

:::
