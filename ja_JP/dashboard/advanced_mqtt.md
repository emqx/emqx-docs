# Advanced MQTT

## Topic Rewrite

Topic Rewrite機能は、デバイスの業務用トピックを変更することを可能にします。EMQXでルールを設定することで、サブスクライブまたはパブリッシュ時に元のトピックを新しいターゲットトピックに書き換えることができます。このページでは、設定ファイルを変更せずにダッシュボードからトピック書き換えルールを追加できます。詳細なトピック書き換えルールについては、[Topic Rewrite](../messaging/mqtt-topic-rewrite.md)を参照してください。

## Auto-Subscription

Auto-subscriptionはEMQXがサポートするMQTT拡張機能です。デバイスが正常に接続した後、事前定義されたルールに基づき、追加のサブスクライブ要求なしにEMQXが自動的に指定されたトピックをサブスクライブします。このページでは、ダッシュボードからAuto-subscription機能を設定できます。詳細な設定方法については、[Auto-Subscription](../messaging/mqtt-auto-subscription.md)を参照してください。

## Delayed Publish

Delayed publishはEMQXがサポートするMQTT拡張機能です。クライアントが特殊なトピックプレフィックス `$delayed/{DelayInterval}` を使用してメッセージをパブリッシュすると、遅延パブリッシュ機能がトリガーされ、ユーザーが設定した遅延時間後にメッセージがパブリッシュされます。このページでは、ダッシュボードから遅延パブリッシュ機能を設定できます。詳細な設定方法については、[Configure Delayed Publish via Dashboard](../messaging/mqtt-delayed-publish.md#configure-delayed-publish-via-dashboard)を参照してください。

## File Transfer

MQTTベースのファイル転送はEMQX Enterpriseの高度な機能です。EMQXはMQTTプロトコルを拡張し、クライアントデバイスがセンサーのデータや制御指示などのリアルタイム構造化データに加え、音声、映像、画像、診断ログなどのオフラインファイルデータを転送、管理、保存できるようにします。この機能はファイル転送ページで設定できます。詳細な設定方法については、[Enabling and Configuring File Transfer via Dashboard](../file-transfer/broker.md#enable-and-configure-file-transfer-via-dashboard)を参照してください。
