# Flapping Detect Configuration

EMQXにおけるフラッピングとは、MQTTクライアントが短時間にブローカーへ頻繁に接続と切断を繰り返す状況を指します。フラッピング検出機能は、フラッピングしているクライアントを検出し、そのクライアントの接続を切断するための仕組みです。

例えば、1分間に15回の接続試行を行ったクライアントをフラッピングクライアントとしてマークし、検出されたクライアントを5分間EMQXへの接続禁止にしたい場合は、以下のコードのように設定します。

```bash
flapping_detect {
  enable = true
  max_count  =  15
  window_time  =  1m
  ban_time  =  5m
}
```

ここで、<!--ダッシュボードUIはほとんどの設定項目がUIで設定できないため記載していません-->

- `max_count` は、指定した時間枠（window_time）内にクライアントから許容される最大接続試行回数を設定します。
- `window_time` は、クライアントの接続試行回数をカウントする時間枠を設定します。
- `ban_time` は、フラッピングとして検出されたクライアントがEMQXへの接続を禁止される期間を設定します。

::: tip

ダッシュボードの左側ナビゲーションメニューから **アクセスコントロール** -> **Flapping Detect** をクリックすることで、設定をカスタマイズすることも可能です。ダッシュボードで設定した内容は、設定ファイル内の同じ項目を上書きします。

:::

::: tip

EMQXはより詳細なカスタマイズに対応する設定項目も提供しています。詳細は[EMQX Enterprise Configuration Manual for Enterprise](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご参照ください。

:::
