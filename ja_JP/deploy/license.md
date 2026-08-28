---
title: EMQX Enterprise ライセンスの利用
description: EMQX Enterprise には、限定的な商用利用が許可されたシングルノードの Community ライセンスが含まれています。ただし、EMQX Enterprise を本格的な商用利用やクラスター展開で使用する場合は、商用ライセンスの取得が必要です。
---


# EMQX Enterprise ライセンスの利用

EMQX 5.9 以降、EMQX は Business Source License（BSL）1.1 のもとでリリースされており、ソースコードの公開開発を可能にしつつ、EMQX の商用利用を保護しています。

::: tip

ライセンス変更の詳細については、[EMQX ライセンス FAQ](https://www.emqx.com/en/content/license-faq) をご参照ください。

:::

EMQX Enterprise のインストールパッケージには、限定的な商用利用が許可されたシングルノードの Community ライセンスが含まれています。ただし、EMQX Enterprise を本格的な商用利用やクラスター展開で使用する場合は、商用ライセンスの取得が必要です。

本ページでは、商用ライセンスの取得および EMQX へのインポート手順をご案内します。

## ライセンスの申請

有効なライセンスキーを伴う商用ライセンスの申請は、EMQ の営業担当者にご連絡いただくか、[お問い合わせ](https://www.emqx.com/en/contact?product=emqx&channel=apply-Licenses) ページのフォームに必要事項を入力してお申し込みください。営業担当者より折り返しご連絡いたします。

購入前に EMQX Enterprise を試用されたい場合は、[トライアルライセンス申請ページ](https://www.emqx.com/en/apply-licenses/emqx) からトライアルライセンスを申請できます。申請後、ライセンスファイルが即座にメールで送付されます。

- トライアルライセンスの有効期限は15日間です。
- トライアルライセンスは最大10,000同時セッションをサポートします。

::: tip 注意事項

トライアル期間中は EMQX Enterprise の全機能が利用可能ですが、トライアル期間終了後はクラスター機能が無効になります。クラスター機能を継続して利用するには商用ライセンスの購入が必要です。

トライアルライセンスの EMQX Enterprise は本番環境での利用は許可されていません。

:::

トライアル期間の延長をご希望の場合は、営業部門までご連絡ください。

## ライセンス設定の更新と構成

ライセンスファイルの更新およびライセンス接続クォータ使用量に関する設定は、EMQX ダッシュボード、コマンドラインインターフェース（CLI）、または設定ファイルから行えます。

### ダッシュボード

1. EMQX ダッシュボードの左側ナビゲーションメニューから **System** -> **License** をクリックします。License ページの **Basic Info** セクションで、ライセンス接続クォータの使用状況、EMQX バージョン、発行情報などを確認できます。

2. **Update License** ボタンをクリックします。ポップアップダイアログにライセンスキーを貼り付け、**Save** をクリックします。送信後、ページ上のライセンス情報が自動的に更新されます。

   情報を確認し、新しいライセンスファイルが反映されていることを確認してください。

3. **License Settings** セクションでは、ライセンスセッションクォータ使用量のしきい値（水位）を設定できます。セッション制限の詳細は [Session Limits](#session-limits) をご参照ください。

   - **Usage High Watermark**：ライセンスセッションクォータ使用量のアラームが発生する上限のパーセンテージ値を指定します。
   - **Usage Low Watermark**：アラームが解除される下限のパーセンテージ値を指定します。

4. **Save Changes** をクリックしてライセンス設定を保存します。

   <img src="./assets/license.png" alt="ライセンス" style="zoom: 50%;" />

#### Community ライセンスへの戻し方

EMQX ダッシュボードでは、デフォルトのシングルノード Community ライセンスに戻すことができます。**License** ページの **Remove License** ボタンをクリックし、ポップアップで確認すると現在のライセンスが削除されます。

::: tip 注意事項

クラスター モードではライセンスを削除できません。クラスター モードで EMQX を使用している場合は、まずクラスターを解散する必要があります。

:::

Community ライセンスに戻した場合：

- 現在のライセンスはクリアされ、Community ライセンスに置き換わります。
- 既存のクライアント接続は維持されます。

::: tip 注意事項

Community ライセンスは本格的な商用利用を許可しておらず、シングルノード展開のみをサポートします。ライセンスを削除するとクラスター展開は無効になります。

:::

### CLI

以下のコマンドでも EMQX Enterprise ライセンスの更新が可能です。

```bash
./bin/emqx ctl 

    license info             # ライセンス情報の表示
    license update <License> # ライセンスキー文字列による更新
    license update default   # デフォルトの Community ライセンスに戻す
```

### 設定ファイル

設定ファイルでライセンスファイルを構成することも可能です。設定後、[EMQX コマンドラインツール](../admin/cli.md) の `emqx ctl license reload` を実行してライセンスをリロードしてください。

```bash
license {
    ## ライセンスキー
    key = "MjIwMTExCjAKMTAKRXZhbHVhdGlvbgpjb250YWN0QGVtcXguaW8KZGVmYXVsdAoyMDIzMDEwOQoxODI1CjEwMAo=.MEUCIG62t8W15g05f1cKx3tA3YgJoR0dmyHOPCdbUxBGxgKKAiEAhHKh8dUwhU+OxNEaOn8mgRDtiT3R8RZooqy6dEsOmDI="
    ## ライセンス接続クォータ使用量アラーム解除の下限水位
    connection_low_watermark = "75%"

    ## ライセンス接続クォータ使用量アラーム発生の上限水位
    connection_high_watermark = "80%"
}
```

実行後、`emqx ctl license info` を実行して新しいライセンスファイルが反映されていることを確認してください。

<!-- 環境変数 `EMQX_LICENSE__KEY` でもライセンスを設定可能です。TODO: リロード可能か確認 -->

## ライセンス制限

EMQX Enterprise ライセンスには、本番環境でのライセンス条件遵守を強制するための使用制限が含まれる場合があります。主な制限は以下の通りです。

- セッション制限
- TPS 制限（EMQX 6.0 以降）

### セッション制限

セッション制限は、現在のライセンス下で EMQX Enterprise がサポートできる最大同時 MQTT クライアント接続数（セッション数）を定義します。

- 制限に達すると、新規接続は拒否されます。
- ライセンスクォータを超えた接続を試みたクライアントには、CONNACK 理由コード `151 (0x97)` の「Quota Exceeded」応答が返されます。
- セッション使用量が設定された上限水位を超えるとアラームが発生します。
- 使用量が下限水位を下回るとアラームは自動的に解除されます。

アラームの水位は EMQX ダッシュボードまたは設定ファイルで設定可能です。

### セッション上限水位の履歴

EMQX Enterprise はクラスター全体の毎日のピークセッション数を自動的に記録し、最大24ヶ月分の履歴を保持します。このデータは複製され、整合性が保護された内部テーブルに保存され、ノードの再起動やクラスターのトポロジー変更後も持続し、課金精算の監査用データとして利用可能です。

#### CLI

記録された履歴は `emqx ctl license history` コマンドで照会できます。

```bash
# 月間ピーク（デフォルト）
emqx ctl license history

# 過去7日間の日間ピーク
emqx ctl license history 7 --period daily

# JSON 出力
emqx ctl license history --json
```

コマンドの詳細は [license history](../admin/cli.md#license-history) をご参照ください。

#### REST API

```bash
GET /api/v5/license/session_hwm_history
```

**クエリパラメーター**

| パラメーター | 型 | デフォルト | 説明 |
| --------- | ---- | ------- | ----------- |
| `period` | `daily` \| `monthly` | `daily` | 集計粒度。`daily` はカレンダー日ごとに1行、`monthly` は日別ピークを月別最大値に集約します。 |
| `limit` | 整数 | `30` | 返却する最大行数。`daily` のみ適用され、`monthly` は最大24ヶ月分の全データを返します。 |

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
- `period`：要求された期間に応じたカレンダー日（`YYYY-MM-DD`）または月（`YYYY-MM`）
- `high_watermark`：期間中に観測されたピークセッション数
- `observed_at`：ピーク観測の RFC 3339 タイムスタンプ

#### タイムゾーン設定

日付の区切りは `license.high_watermark_timezone` 設定項目で決定されます。デフォルトはノードホストのローカルタイムゾーン（`"system"`）です。異なる地域のノード間で日付境界を統一するために、明示的な UTC オフセット（例：`"+08:00"`）を設定可能です。詳細は [ライセンス設定](../configuration/license.md) をご参照ください。

### TPS 制限

EMQX 6.0 以降、ライセンスには TPS（Transactions Per Second）制限が含まれる場合があります。この制限はクラスター全体で処理される MQTT メッセージの総数（送受信両方）に適用されます。

- TPS 使用量がライセンス上限を超えるとアラームが発生します。
- アラームはピーク TPS を記録しますが、メッセージの流量制限は行いません。
- アラームは以下のいずれかで解除されるまで継続します：
  - より高い TPS 上限の新しいライセンスが適用された場合
  - EMQX ダッシュボードまたは CLI から手動でアラームを解除した場合

この TPS 制限は監視およびコンプライアンス目的で設計されており、厳密な制限ではありません。

::: tip 注意事項

TPS 制限はライセンスで定義されており、ユーザー側で設定や調整はできません。上限を引き上げるには、より高い TPS 値を持つ新しいライセンスを適用してください。

:::
