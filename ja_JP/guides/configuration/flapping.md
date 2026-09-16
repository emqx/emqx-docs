# Flapping Detect 設定

Flapping検出は、EMQXへの過剰な接続試行を識別します。EMQX 6.3.0以降では、クライアントID、ユーザー名、送信元IPアドレスごとに独立したポリシーを設定できます。

## 検出の次元

`flapping_detect` 設定には以下の次元があります。各次元はデフォルトで無効です。

| 設定項目 | 検出キー | 動作内容 |
| --- | --- | --- |
| `by_clientid` | クライアントID | 同じクライアントIDからの接続試行をカウントします。 |
| `by_username` | ユーザー名 | 同じユーザー名を共有するクライアントからの接続試行をカウントします。ユーザー名がない接続はカウントされません。 |
| `by_peerhost` | 送信元IPアドレス | 同じ送信元IPアドレスからの接続試行をカウントします。 |

検出設定が構成されると、その次元は有効になります。無効にするには該当次元を `none` に設定してください。3つの次元の任意の組み合わせを有効にでき、有効な各次元は独立してカウントされます。

各次元に設定可能な検出パラメータは以下の通りです。

| フィールド | 説明 | デフォルト値 |
| --- | --- | --- |
| `window_time` | EMQXが接続試行をカウントする時間ウィンドウ | `1m` |
| `max_count` | `window_time`内でバンを発動する接続試行回数の閾値 | `15` |
| `ban_time` | マッチしたクライアントID、ユーザー名、または送信元IPアドレスをEMQXがバンする期間 | `5m` |

クライアントID、ユーザー名、または送信元IPアドレスが閾値に達すると、EMQXは該当識別子またはアドレスに対して一時的なバンエントリを作成します。新たなマッチする接続試行は認証前に拒否され、既存の接続は維持されます。バンエントリは自動的に期限切れとなり、`/banned` REST APIで確認・削除できます。

検出カウンターは各ノードで個別に管理されます。EMQXは異なるノードで処理された接続試行を集約しません。ノードがflappingを検出すると、そのバンエントリはクラスター全体にレプリケートされます。

## 設定例

以下のHOCON例は、クライアントIDと送信元IPアドレスに異なるポリシーを設定し、ユーザー名による検出を無効化しています。

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

ゾーンごとにこれらの設定を構成することも可能です。ゾーン内の部分的な次元ポリシーは、未指定のフィールドを対応するグローバル次元ポリシーから継承します。

## EMQX 6.3.0以前に作成された設定との互換性

EMQX 6.3.0以降、以下のフラットフィールドは非推奨となりました。

- `flapping_detect.enable`
- `flapping_detect.window_time`
- `flapping_detect.max_count`
- `flapping_detect.ban_time`

EMQXは後方互換性のためこれらのフィールドを引き続き受け付けます。`enable = true`の場合、EMQXはフラットポリシーフィールドを `by_clientid` にマッピングします。`enable = false` または明示的に `enable = true` がない場合は、`by_clientid` を `none` に設定します。

非推奨フィールドはクライアントID検出のみに影響し、`by_username` と `by_peerhost` は明示的に設定しない限り `none` のままです。

設定に `by_clientid` と非推奨のフラットフィールドの両方が含まれる場合は、`by_clientid` が優先されます。`by_clientid` が `none` に設定されていても同様です。

EMQXは高度なカスタマイズ向けの追加設定オプションを提供しています。詳細は[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご参照ください。

また、EMQXダッシュボードの **アクセス制御** -> **Flapping Detect** からもflapping検出を設定できます。ダッシュボードでの手順は[Flapping Detect](../access-control/flapping-detect.md)をご覧ください。
