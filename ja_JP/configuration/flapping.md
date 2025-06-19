# Flapping Detect 設定

EMQXにおけるフラッピングとは、MQTTクライアントが短時間にブローカーへ頻繁に接続・切断を繰り返す状況を指します。フラッピング検出機能は、こうしたフラッピング状態のクライアントを検出し、接続を切断するための仕組みです。

例えば、1分間に15回接続を試みたクライアントをフラッピングクライアントとしてマークし、検出されたクライアントを5分間EMQXへの接続禁止にしたい場合は、以下のコードのように設定します。

```bash
flapping_detect {
  enable = true
  max_count  =  15
  window_time  =  1m
  ban_time  =  5m
}
```

ここで、<!--ダッシュボードUIはほとんどの設定項目がUIで設定できないため割愛しています-->

- `max_count` は、指定した時間窓（`window_time`）内でクライアントが許容される最大接続試行回数を設定します。
- `window_time` は、クライアントの接続試行回数をカウントする時間窓を設定します。
- `ban_time` は、フラッピングと検出されたクライアントがEMQXへの接続を禁止される期間を設定します。

::: tip

ダッシュボードの左側ナビゲーションメニューから **Access Control** -> **Flapping Detect** をクリックすることで、設定をカスタマイズすることも可能です。ダッシュボードで設定した内容は、設定ファイルの同じ項目を上書きします。

:::

::: tip

EMQXはさらに多くの設定項目を提供しており、より細かいカスタマイズが可能です。詳細は[EMQX Enterprise Configuration Manual for Enterprise](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご参照ください。

:::
