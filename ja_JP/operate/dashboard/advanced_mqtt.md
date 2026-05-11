# Advanced MQTT

## トピック書き換え

トピック書き換え機能は、デバイスの業務用トピックを変更することを可能にします。EMQXでルールを設定することで、サブスクライブやパブリッシュ時に元のトピックを新しいターゲットトピックに書き換えることができます。本ページでは、設定ファイルを変更せずにダッシュボードからトピック書き換えルールを追加できます。詳細なトピック書き換えルールについては、[Topic Rewrite](../messaging/mqtt-topic-rewrite.md)をご参照ください。

## 自動サブスクライブ

自動サブスクライブは、EMQXがサポートするMQTTの拡張機能です。デバイスが正常に接続した後、追加のサブスクライブ要求なしに、あらかじめ定義されたルールに基づいて指定されたトピックに自動的にサブスクライブすることを可能にします。本ページでは、ダッシュボードから自動サブスクライブ機能の設定が行えます。詳細な設定方法は、[Auto-Subscription](../messaging/mqtt-auto-subscription.md)をご参照ください。

## 遅延パブリッシュ

遅延パブリッシュは、EMQXがサポートするMQTTの拡張機能です。クライアントが特別なトピックプレフィックス `$delayed/{DelayInterval}` を使ってメッセージをパブリッシュすると、遅延パブリッシュ機能が起動し、ユーザーが設定した遅延時間後にメッセージがパブリッシュされます。本ページでは、ダッシュボードから遅延パブリッシュ機能の設定が行えます。詳細な設定方法は、[Configure Delayed Publish via Dashboard](../messaging/mqtt-delayed-publish.md#configure-delayed-publish-via-dashboard)をご参照ください。

## ファイル転送

MQTTに基づくファイル転送は、EMQX Enterpriseの高度な機能です。EMQXはMQTTプロトコルを拡張し、クライアントデバイスがセンサーのデータや制御指令などのリアルタイム構造化データに加え、音声、映像、画像、診断ログなどのオフラインファイルデータを転送、管理、保存できるようにします。この機能はファイル転送ページで設定可能です。詳細な設定方法は、[Enabling and Configuring File Transfer via Dashboard](../file-transfer/broker.md#enable-and-configure-file-transfer-via-dashboard)をご参照ください。
