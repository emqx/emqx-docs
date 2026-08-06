# 機能比較

このページでは、さまざまなデプロイタイプでサポートされている機能を詳細に一覧化しています。

## コア／エンタープライズ機能

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">機能</th>
    <th colspan="1">セルフホスト</th>
    <th colspan="2">MQTT as a Service</th>
    <th rowspan="2">備考およびリンク</th>
  </tr>
  <tr>
    <td>EMQX Enterprise</td>
    <td>EMQX Serverless</td>
    <td>EMQX Dedicated</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>MQTT 5.0 ブローカー</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> 完全なMQTT 5.0プロトコル実装</td>
  </tr>
  <tr>
    <td><b>MQTT over QUIC</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> 世界初のサポート<br>クラウド向け開発中</td>
  </tr>
  <tr>
    <td><b>MQTT アドオン</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://docs.emqx.com/en/emqx/latest/messaging/mqtt-shared-subscription.html">共有サブスクリプション</a><br><a href="https://docs.emqx.com/en/emqx/latest/messaging/mqtt-exclusive-subscription.html">排他サブスクリプション</a><br><a href="https://docs.emqx.com/en/emqx/latest/messaging/mqtt-delayed-publish.html">遅延パブリッシュ</a><br><a href="https://docs.emqx.com/en/emqx/latest/messaging/mqtt-auto-subscription.html">自動サブスクリプション</a><br><a href="https://docs.emqx.com/en/emqx/latest/messaging/mqtt-topic-rewrite.html">トピック書き換え</a><br>その他のカスタマイズオプション</td>
  </tr>
  <tr>
    <td><b>マルチプロトコルゲートウェイ</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> より多くの業界デバイスアクセス</td>
  </tr>
  <tr>
    <td><b>マルチテナンシー</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> システムの柔軟性と利用率向上<br>（近日公開予定）</td>
  </tr>
  <tr>
    <td><b>クラスターリンク</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> デバイスとアプリケーションデータのシームレス接続<br>（近日公開予定）</td>
  </tr>
  <tr>
    <td><b>イベント履歴</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> クライアントの障害診断および行動監査</td>
  </tr>
  <tr>
    <td><b>メッセージキューイング</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> データ送受信と分析の統一アーキテクチャ<br>（近日公開予定）</td>
  </tr>
  <tr>
    <td><b>ストリーム処理</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> 高い信頼性と災害復旧機能<br>（近日公開予定）</td>
  </tr>
  <tr>
    <td><b>データパーシステンス</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 内蔵RocksDBバックエンドまたは外部データベース</td>
    <td>N/A</td>
    <td>N/A</td>
    <td> <a href="../durability/durability_introduction.md">安定性と信頼性の向上</a></td>
  </tr>
  <tr>
    <td><b>スキーマレジストリ</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://docs.emqx.com/en/emqx/latest/data-integration/schema-registry.html">スキーマレジストリ</a> によりデータの一貫性と互換性を保証</td>
  </tr>
  <tr>
    <td><b>メッセージコーデック</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>柔軟なメッセージフォーマット変換：<br>JSON<br>Avro<br>Protobuf<br>カスタムコーデック（HTTP/gRPC）</td>
  </tr>
<tr>
    <td><b>スキーマバリデーション</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 製品ロードマップに含む</td>
    <td> メッセージの整合性と合法性を保証</td>
  </tr>
  <tr>
    <td><b>ルールエンジン</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://docs.emqx.com/en/emqx/latest/data-integration/rules.html">SQLベースの組み込みルールエンジンとリアルタイムデータ処理</a></td>
  </tr>
<tr>
    <td><b>フローデザイナー</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 製品ロードマップに含む</td>
    <td><a href="https://docs.emqx.com/en/emqx/latest/flow-designer/introduction.html">データ統合の簡単なオーケストレーション</a></td>
  </tr>
  <tr>
    <td><b>ファイル転送</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 製品ロードマップに含む</td>
    <td>統一プラットフォームのデータ送信</td>
  </tr>
  <tr>
    <td><b>Kafka統合</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://docs.emqx.com/en/emqx/latest/data-integration/data-bridge-kafka.html">Apache KafkaへのMQTTデータストリーム</a></td>
  </tr>
  <tr>
    <td><b>エンタープライズデータ統合</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 40以上</td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" />40以上</td>
    <td><a href="https://www.emqx.com/en/integrations">ビジネス開発と提供速度の加速</a></td>
  </tr>
  <tr>
    <td><b>トラブルシューティング</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://docs.emqx.com/en/emqx/latest/observability/tracer.html">ログトレース</a><br><a href="https://docs.emqx.com/en/emqx/latest/observability/slow-subscribers-statistics.html">遅延サブスクリプション</a></td>
  </tr>
  <tr>
    <td><b>クラウドネイティブ＆K8s</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td><a href="https://docs.emqx.com/en/emqx/latest/deploy/kubernetes/kubernetes.html">システムのデプロイおよび管理コスト削減</a></td>
  </tr>
  <tr>
    <td><b>エッジコンピューティング</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> データ送信のレイテンシとコスト削減<br><a href="https://www.emqx.com/en/products/neuronex">Neuron</a><br><a href="https://www.emqx.com/en/products/nanomq">NanoMQ</a></td>
  </tr>
</tbody>
</table>
</div>




## スケーラビリティとパフォーマンス

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">スケーラビリティ／パフォーマンス</th>
    <th colspan="1">セルフホスト</th>
    <th colspan="2">MQTT as a Service</th>
    <th rowspan="2">備考およびリンク</th>
  </tr>
  <tr>
    <td>EMQX Enterprise</td>
    <td>EMQX Serverless</td>
    <td>EMQX Dedicated</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>スケーラビリティ</b></td>
    <td><span style="font-weight:normal">最大100ノードクラスター<br>クラスターあたり最大1億MQTT接続</span></td>
    <td><span style="font-weight:normal">1000自動スケール</span></td>
    <td><span style="font-weight:normal">1000～無制限</span></td>
    <td><a href="https://www.emqx.com/en/blog/reaching-100m-mqtt-connections-with-emqx-5-0">EMQX 5.0で1億MQTT接続を達成</a></td>
  </tr>
  <tr>
    <td><b>可用性</b></td>
    <td><span style="font-weight:normal">コア-レプリカクラスター</span></td>
    <td><span style="font-weight:normal">マスターレスクラスター</span></td>
    <td><span style="font-weight:normal">マスターレスクラスター</span></td>
    <td><span style="font-weight:normal"> </span></td>
  </tr>
  <tr>
    <td><b>信頼性</b></td>
    <td><span style="font-weight:normal">高可用レプリケーションを備えたRocksDBによるデータパーシステンス</span></td>
    <td><span style="font-weight:normal">セッションパーシステンス</span></td>
    <td><span style="font-weight:normal">セッションパーシステンス</span></td>
    <td><a href="https://www.emqx.com/en/blog/mqtt-persistence-based-on-rocksdb">RocksDBに基づく高信頼MQTTデータパーシステンス</a></td>
  </tr>
  <tr>
    <td><b>パフォーマンス</b></td>
    <td><span style="font-weight:normal">毎秒500万以上のMQTTメッセージ</span></td>
    <td><span style="font-weight:normal">毎秒1000のMQTTメッセージ</span></td>
    <td><span style="font-weight:normal">毎秒500万以上のMQTTメッセージ</span></td>
    <td><span style="font-weight:normal"> </span></td>
  </tr>
  <tr>
    <td><b>レイテンシ</b></td>
    <td><span style="font-weight:normal">1～5ミリ秒</span></td>
    <td><span style="font-weight:normal">1～5ミリ秒</span></td>
    <td><span style="font-weight:normal">1～5ミリ秒</span></td>
    <td><span style="font-weight:normal"> </span></td>
  </tr>
  <tr>
    <td><b>SLA</b></td>
    <td><span style="font-weight:normal">該当なし</span></td>
    <td><span style="font-weight:normal">99.9% アップタイム</span></td>
    <td><span style="font-weight:normal">最大99.99%</span><br><span style="font-weight:normal">アップタイム</span></td>
    <td><span style="font-weight:normal"> </span></td>
  </tr>
</tbody>
</table>
</div>




## クラスタリングアーキテクチャ

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">MQTT & 接続性<br></th>
    <th colspan="1">セルフホスト<br></th>
    <th colspan="2">MQTT as a Service<br></th>
    <th rowspan="2">備考およびリンク<br></th>
  </tr>
  <tr>
    <td>EMQX Enterprise</td>
    <td>EMQX Serverless</td>
    <td>EMQX Dedicated</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>MQTT 3.x</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>MQTT 5.0</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>MQTT リテーナー</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>MQTT over TCP</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>MQTT over TLS</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>MQTT over WebSocket</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>MQTT over QUIC</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>EMQXは現在、QUICトランスポートをサポートする唯一のMQTTブローカーです。</td>
  </tr>
  <tr>
    <td><b>LB（プロキシプロトコル）</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>プロキシプロトコル v1、v2</td>
  </tr>
  <tr>
    <td><b>LB（カスタム）</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>GmSSL<br>スムーズな接続移行</td>
  </tr>
  <tr>
    <td><b>IPv6サポート</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>マルチプロトコルゲートウェイ</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>MQTT-SN</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>STOMP</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>CoAP</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>LwM2M</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>OCPP</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>JT/808</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>GBT32960</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
</tbody>
</table>
</div>



## MQTT と接続性

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">MQTT & 接続性<br></th>
    <th colspan="1">セルフホスト<br></th>
    <th colspan="2">MQTT as a Service<br></th>
    <th rowspan="2">備考およびリンク<br></th>
  </tr>
  <tr>
    <td>EMQX Enterprise</td>
    <td>EMQX Serverless</td>
    <td>EMQX Dedicated</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>MQTT 3.x</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>MQTT 5.0</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>MQTT リテーナー</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>MQTT over TCP</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>MQTT over TLS</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>MQTT over WebSocket</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>MQTT over QUIC</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>EMQXは現在、QUICトランスポートをサポートする唯一のMQTTブローカーです。</td>
  </tr>
  <tr>
    <td><b>LB（プロキシプロトコル）</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>プロキシプロトコル v1、v2</td>
  </tr>
  <tr>
    <td><b>LB（カスタム）</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>GmSSL<br>スムーズな接続移行</td>
  </tr>
  <tr>
    <td><b>IPv6サポート</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>マルチプロトコルゲートウェイ</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>MQTT-SN</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>STOMP</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>CoAP</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>LwM2M</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>OCPP</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>JT/808</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>GBT32960</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
</tbody>
</table>
</div>



## セキュリティ

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">セキュリティ<br></th>
    <th colspan="1">セルフホスト<br></th>
    <th colspan="2">MQTT as a Service<br></th>
    <th rowspan="2">備考およびリンク<br></th>
  </tr>
  <tr>
    <td>EMQX Enterprise</td>
    <td>EMQX Serverless</td>
    <td>EMQX Dedicated</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>TLS/SSL</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>データ送信のセキュリティ保護：TLS 1.1、1.2、1.3</td>
  </tr>
  <tr>
    <td><b>QUIC</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>弱いネットワークやモバイルネットワークのデータ送信効率を向上</td>
  </tr>
  <tr>
    <td><b>OCSP Stapling</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>より柔軟なセキュリティ運用を提供</td>
  </tr>
  <tr>
    <td><b>フラッピング検出</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 製品ロードマップに含む</td>
    <td>頻繁なオンライン・オフライン接続を検出・遮断</td>
  </tr>
  <tr>
    <td><b>監査ログ</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>重要操作の監査トレーシングをサポート</td>
  </tr>
  <tr>
    <td><b>ダッシュボードSSO</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>安全かつ簡素化された認証プロセス</td>
  </tr>
  <tr>
    <td><b>ダッシュボード／REST API RBAC</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>権限を最小限に抑えシステムの安全性を確保</td>
  </tr>
  <tr>
    <td><b>Black Duck解析</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td>安全かつ簡素化された認証プロセス</td>
  </tr>
</tbody>
</table>
</div>



## 認証と認可

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">認証／認可<br></th>
    <th colspan="1">セルフホスト<br></th>
    <th colspan="2">MQTT as a Service<br></th>
    <th rowspan="2">備考およびリンク<br></th>
  </tr>
  <tr>
    <td>EMQX Enterprise</td>
    <td>EMQX Serverless</td>
    <td>EMQX Dedicated</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>ユーザー名／パスワード</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://docs.emqx.com/en/emqx/latest/access-control/authn/pwoverview.html">パスワード認証</a></td>
  </tr>
  <tr>
    <td><b>JWT</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://docs.emqx.com/en/emqx/latest/access-control/authn/jwt.html">JWT認証</a></td>
  </tr>
  <tr>
    <td><b>MQTT 5.0 強化認証</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td><a href="https://docs.emqx.com/en/emqx/latest/access-control/authn/scram.html">MQTT 5.0 強化認証</a></td>
  </tr>
  <tr>
    <td><b>LDAP認証</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 製品ロードマップに含む</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>PSK認証</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://docs.emqx.com/en/emqx/latest/network/psk-authentication.html#enable-psk-authentication">PSK認証の有効化</a></td>
  </tr>
  <tr>
    <td><b>X.509証明書</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> EMQX Cloudで管理</td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>細粒度アクセス制御</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>認証データベースバックエンド</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>ACLデータベースバックエンド</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
</tbody>
</table>
</div>



## データ統合

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">データ統合<br></th>
    <th colspan="1">セルフホスト<br></th>
    <th colspan="2">MQTT as a Service<br></th>
  </tr>
  <tr>
    <td>EMQX Enterprise</td>
    <td>EMQX Serverless</td>
    <td>EMQX Dedicated</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>MQTTブリッジ</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Webhook/HTTPサーバー</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Apache Kafka/Confluent</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Apache IoTDB</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Apache Pulsar</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>AWS Kinesis</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>AWS S3</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Azure Event Hubs</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Cassandra</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>ClickHouse</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>DynamoDB</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Elasticsearch</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>GCP PubSub</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>GreptimeDB</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>HStreamDB</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>InfluxDB</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Microsoft SQL Server</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>MongoDB</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>MySQL</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>OpenTSDB</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Oracle Database</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>PostgreSQL</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>RabbitMQ</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Redis</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>RocketMQ</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Syskeeper</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>TDengine</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>TimeScaleDB</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
</tbody>
</table>
</div>



## ルールエンジン

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">ルールエンジン<br></th>
    <th colspan="1">セルフホスト<br></th>
    <th colspan="2">MQTT as a Service<br></th>
    <th rowspan="2">備考およびリンク<br></th>
  </tr>
  <tr>
    <td>EMQX Enterprise</td>
    <td>EMQX Serverless</td>
    <td>EMQX Dedicated</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>スキーマレジストリ</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>データフォーマットの一貫性を保証</td>
  </tr>
  <tr>
    <td><b>JSONコーデック</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Avroコーデック</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Protobufコーデック</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Sparkplug Bコーデック</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>JSONスキーマバリデーション</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Avroバリデーション</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>ProtoBufバリデーション</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>組み込み関数</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://docs.emqx.com/en/emqx/latest/data-integration/rule-sql-builtin-functions.html">SQL文で利用可能な関数、豊富な組み込みライブラリ、カスタム拡張対応</a></td>
  </tr>
  <tr>
    <td><b>jq関数</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>効率的なJSONデータ処理</td>
  </tr>
  <tr>
    <td><b>イベントトリガー</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://docs.emqx.com/en/emqx/latest/data-integration/rule-sql-events-and-fields.html#mqtt-events">クライアントイベント</a>、イベント駆動型ビジネス開発</td>
  </tr>
</tbody>
</table>
</div>



## 拡張性

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">拡張性<br></th>
    <th colspan="1">セルフホスト<br></th>
    <th colspan="2">MQTT as a Service<br></th>
    <th rowspan="2">備考およびリンク<br></th>
  </tr>
  <tr>
    <td>EMQX Enterprise</td>
    <td>EMQX Serverless</td>
    <td>EMQX Dedicated</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>フック</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td><a href="https://docs.emqx.com/en/emqx/latest/extensions/hooks.html#hooks">フック</a></td>
  </tr>
  <tr>
    <td><b>プラグイン</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td><a href="https://docs.emqx.com/en/emqx/latest/extensions/plugins.html#plugins">プラグイン</a></td>
  </tr>
  <tr>
    <td><b>プラグインホットロード</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>プラグインホット設定</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>ゲートウェイ</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>ExHooks/gRPC</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td></td>
  </tr>
</tbody>
</table>
</div>



## 運用性

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">運用性<br></th>
    <th colspan="1">セルフホスト<br></th>
    <th colspan="2">MQTT as a Service<br></th>
    <th rowspan="2">備考およびリンク<br></th>
  </tr>
  <tr>
    <td>EMQX Enterprise</td>
    <td>EMQX Serverless</td>
    <td>EMQX Dedicated</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>ダッシュボード</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>EMQXダッシュボードは多機能です。<br>設定はダッシュボードからホットアップデート可能です。</td>
  </tr>
  <tr>
    <td><b>設定</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> HOCON</td>
    <td>N/A</td>
    <td>N/A</td>
    <td>HOCON形式はシンプルかつ簡潔です。</td>
  </tr>
  <tr>
    <td><b>HTTP API</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>CLI</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>設定ホットアップデート</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>運用監査</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
</tbody>
</table>
</div>



## 可観測性

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">可観測性<br></th>
    <th colspan="1">セルフホスト<br></th>
    <th colspan="2">MQTT as a Service<br></th>
    <th rowspan="2">備考およびリンク<br></th>
  </tr>
  <tr>
    <td>EMQX Enterprise</td>
    <td>EMQX Serverless</td>
    <td>EMQX Dedicated</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>ダッシュボード</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>エレガントなダッシュボードでクラスターをリアルタイム監視</td>
  </tr>
  <tr>
    <td><b>メトリクス</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>ノードメトリクス</td>
  </tr>
  <tr>
    <td><b>Grafana</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Prometheus</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Datadog</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 製品ロードマップに含む</td>
    <td></td>
  </tr>
  <tr>
    <td><b>OpenTelemetry</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 製品ロードマップに含む</td>
    <td></td>
  </tr>
  <tr>
    <td><b>クラスター メトリクス</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>アラーム通知</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>遅延サブスクリプション監視</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 製品ロードマップに含む</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>トピック監視</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 製品ロードマップに含む</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>クライアント監視</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>ログトレース</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
</tbody>
</table>
</div>



## クラウドネイティブとK8S

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">クラウドネイティブ＆K8s<br></th>
    <th colspan="1">セルフホスト<br></th>
    <th colspan="2">MQTT as a Service<br></th>
    <th rowspan="2">備考およびリンク<br></th>
  </tr>
  <tr>
    <td>EMQX Enterprise</td>
    <td>EMQX Serverless</td>
    <td>EMQX Dedicated</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>Docker</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td><a href="https://hub.docker.com/r/emqx/emqx-enterprise">Docker Hub</a></td>
  </tr>
  <tr>
    <td><b>Kubernetesオペレーター</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td><a href="https://www.emqx.com/en/emqx-kubernetes-operator">EMQX Kubernetes Operator</a></td>
  </tr>
</tbody>
</table>
</div>



## クラウドプラットフォームの対応状況

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">クラウドプラットフォーム<br></th>
    <th colspan="1">セルフホスト<br></th>
    <th colspan="2">MQTT as a Service<br></th>
    <th rowspan="2">備考およびリンク<br></th>
  </tr>
  <tr>
    <td>EMQX Enterprise</td>
    <td>EMQX Serverless</td>
    <td>EMQX Dedicated</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>AWSマーケットプレイス</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>EMQXはAWSマーケットプレイスで利用可能です。<br><a href="https://aws.amazon.com/marketplace/pp/prodview-cwa2e6xbrwtzi">AWS Marketplace: EMQX Enterprise on Ubuntu 20.04</a> </td>
  </tr>
  <tr>
    <td><b>Azureマーケットプレイス</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>GCPマーケットプレイス</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td></td>
  </tr>
  <tr>
    <td><b>AWS</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td></td>
  </tr>
  <tr>
    <td><b>Azure</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td></td>
  </tr>
  <tr>
    <td><b>GCP</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td></td>
  </tr>
</tbody>
</table>
</div>



## MQTTツールとSDK

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">MQTTツール＆SDK<br></th>
    <th colspan="1">セルフホスト<br></th>
    <th colspan="2">MQTT as a Service<br></th>
    <th rowspan="2">備考およびリンク<br></th>
  </tr>
  <tr>
    <td>EMQX Enterprise</td>
    <td>EMQX Serverless</td>
    <td>EMQX Dedicated</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>MQTTデスクトップクライアント</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>MQTT X - MQTT学習に最適なツール。<br><a href="https://mqttx.app/">MQTTX: オールインワンMQTTクライアントツールボックス</a></td>
  </tr>
  <tr>
    <td><b>MQTT CLI</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://mqttx.app/cli">MQTTX CLI: 強力で使いやすいMQTT CLIツール</a></td>
  </tr>
  <tr>
    <td><b>MQTT Webツール</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>多機能で使いやすい。<br><a href="https://mqttx.app/web">MQTTX Web: 使いやすいMQTT WebSocketクライアントツール</a></td>
  </tr>
  <tr>
    <td><b>MQTTベンチマーク</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://github.com/emqx/emqtt-bench">GitHub - emqx/emqtt-bench: Erlangで書かれた軽量MQTTベンチマークツール</a></td>
  </tr>
  <tr>
    <td><b>MQTT負荷テスト</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> XMeter</td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>XMeter - 世界No.1のMQTT負荷テストツール</td>
  </tr>
  <tr>
    <td><b>MQTT & JMeter</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> XMeter</td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://github.com/emqx/mqtt-jmeter">GitHub - emqx/mqtt-jmeter: MQTT JMeterプラグイン</a></td>
  </tr>
  <tr>
    <td><b>C向けMQTT SDK</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> NanoSDK</td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://github.com/nanomq/NanoSDK">GitHub - nanomq/NanoSDK: NNGフレーバーでQUIC対応のMQTT 5.0準拠SDK</a></td>
  </tr>
  <tr>
    <td><b>MQTT Erlang SDK</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://github.com/emqx/emqtt">GitHub - emqx/emqtt: Erlang MQTT 5.0クライアント</a></td>
  </tr>
  <tr>
    <td><b>MQTT iOS SDK</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://github.com/emqx/CocoaMQTT">GitHub - emqx/CocoaMQTT: Swiftで書かれたiOS/macOS向けMQTT 5.0クライアントライブラリ</a></td>
  </tr>
  <tr>
    <td><b>MQTT QUICクライアント</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://github.com/emqx/quic">GitHub - emqx/quic: Erlang & Elixir向けQUICプロトコル</a></td>
  </tr>
</tbody>
</table>
</div>



## サポートサービス

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">サポートサービス<br></th>
    <th colspan="1">セルフホスト<br></th>
    <th colspan="2">MQTT as a Service<br></th>
    <th rowspan="2">備考およびリンク<br></th>
  </tr>
  <tr>
    <td>EMQX Enterprise</td>
    <td>EMQX Serverless</td>
    <td>EMQX Dedicated</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>テクニカルサポート</b></td>
    <td>5*8、7*24 グローバルサポート</td>
    <td>5*8 グローバルサポート</td>
    <td>5*8、7*24 グローバルサポート</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>アーキテクチャコンサルティング</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>プロジェクト統合</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>カスタム開発</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
</tbody>
</table>
</div>
