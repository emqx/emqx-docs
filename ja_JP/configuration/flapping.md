# Flapping Detect 設定

Flapping検出は、EMQXへの過剰な接続試行を識別します。EMQX 6.3.0以降では、クライアントID、ユーザー名、および送信元IPアドレスごとに独立したポリシーを設定できます。

## 検出の次元

`flapping_detect` 設定には以下の次元があります。各次元はデフォルトで無効になっています。

| 設定項目 | 検出キー | 動作内容 |
| --- | --- | --- |
| `by_clientid` | クライアントID | 同一クライアントIDからの接続試行をカウントします。 |
| `by_username` | ユーザー名 | 同一ユーザー名を共有するクライアントからの接続試行をカウントします。ユーザー名なしの接続はカウントされません。 |
| `by_peerhost` | 送信元IPアドレス | 同一送信元IPアドレスからの接続試行をカウントします。 |

検出設定が構成されると、その次元は有効になります。次元を無効にするには `none` に設定してください。3つの次元の任意の組み合わせを有効にでき、有効な各次元は独立してカウントされます。

各次元で設定可能な検出パラメータは以下の通りです。

| フィールド | 説明 | デフォルト値 |
| --- | --- | --- |
| `window_time` | EMQXが接続試行をカウントする時間ウィンドウ。 | `1m` |
| `max_count` | `window_time`内での接続試行回数の上限。超えるとBANが発動します。 | `15` |
| `ban_time` | 対象のクライアントID、ユーザー名、または送信元IPアドレスをBANする期間。 | `5m` |

クライアントID、ユーザー名、または送信元IPアドレスが閾値に達すると、EMQXはその識別子またはアドレスに対して一時的なBANエントリを作成します。新たな該当接続試行は認証前に拒否され、既存の接続は維持されます。BANエントリは自動的に期限切れとなり、`/banned` REST APIで確認や削除が可能です。

検出カウンターは各ノードで個別に管理されます。EMQXは異なるノードで処理された接続試行を統合しません。ノードがflappingを検出すると、そのBANエントリはクラスター全体にレプリケートされます。

## 設定例

以下のHOCON例では、クライアントIDと送信元IPアドレスに異なるポリシーを設定し、ユーザー名による検出を無効にしています。

```hocon
flapping_detect {
  by_clientid {
    window_time = 1m
    max_count = 15
    ban_time = 5m
  }

  by_username = none

  by_peerhost {
    window_time = 30s
    max_count = 100
    ban_time = 10m
  }
}
```

これらの設定はゾーン単位でも構成可能です。ゾーン内で部分的に設定された次元ポリシーは、未指定のフィールドを対応するグローバル次元ポリシーから継承します。

## EMQX 6.3.0以前に作成された設定との互換性

EMQX 6.3.0以降、以下のフラットフィールドは非推奨となりました。

- `flapping_detect.enable`
- `flapping_detect.window_time`
- `flapping_detect.max_count`
- `flapping_detect.ban_time`

EMQXは後方互換性のためこれらのフィールドを引き続き受け入れます。`enable = true` の場合、EMQXはフラットポリシーフィールドを `by_clientid` にマッピングします。`enable = false` または明示的な `enable = true` がない場合は、`by_clientid` を `none` に設定します。

非推奨フィールドはクライアントID検出のみに影響し、`by_username` と `by_peerhost` は明示的に設定しない限り `none` のままです。

設定に `by_clientid` と非推奨のフラットフィールドが両方含まれる場合は、`by_clientid` の設定が優先されます。`by_clientid` が `none` に設定されている場合も同様です。

EMQXは高度なカスタマイズのための追加設定オプションを提供しています。詳細は[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご参照ください。

また、EMQXダッシュボードの **Access Control** -> **Flapping Detect** からもflapping検出を設定できます。ダッシュボードでの手順は[Flapping Detect](../access-control/flapping-detect.md)をご覧ください。
