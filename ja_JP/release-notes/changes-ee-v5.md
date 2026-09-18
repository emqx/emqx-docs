# EMQX Enterprise Version 5

## 5.10.5

*リリース日: 2026-09-17*

EMQX 5.10.5 へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### プラグイン

- [#17449](https://github.com/emqx/emqx/pull/17449) EMQX Backup Sync プラグインを追加しました。このプラグインは、Data Backup API を通じてプライマリクラスターからセカンダリクラスターへ選択された設定およびサポートされているテーブルデータを定期的に同期します。プラグインはセカンダリクラスターのみにインストール・実行され、プライマリクラスターへの HTTPS 接続に対して設定可能な TLS オプションをサポートします。Dashboard ユーザーや API キーは同期対象に含まれません。

- [#17887](https://github.com/emqx/emqx/pull/17887) EMQX REST API を通じた同期 MQTT リクエスト/レスポンスフロー用の `emqx_sync_request` プラグインを追加しました。ノードローカルの CLI 診断機能としてリクエストカウンターと現在の保留状態も提供します。

#### パフォーマンス

- [#17964](https://github.com/emqx/emqx/pull/17964) 認証および認可リソースのバッファワーカープールの生成を停止しました。これらのリソースは常に `simple_sync_query` を通じてクエリされ、バッファワーカーをバイパスするため、以前生成されていたワーカーはアイドル状態で使用されていませんでした。

- [#18138](https://github.com/emqx/emqx/pull/18138) [#18186](https://github.com/emqx/emqx/pull/18186) サブスクリプションの HTTP API における深いページングクエリを改善しました。各ターゲットノードでメモリ内にサブスクリプション行を蓄積し、ページネーションごとに RPC を発生させるのを回避しています。

#### アクセス制御

- [#17864](https://github.com/emqx/emqx/pull/17864) Dashboard ユーザーおよび API キーのエンドポイントで、特権スコープ（`system`、`user_management`、`api_key_management`、`sso_management`）とその他のスコープが混在するスコープリストを拒否するようになりました。これら4つの特権スコープは管理者相当の権限を持つため、制限されたスコープリストと組み合わせても意味のある制限になりません。アカウントに管理者相当の能力を持たせるかどうかに応じて、特権のみのスコープリストか非特権のみのスコープリストを使用してください。混在したスコープセットを持つ既存のレコードは次回更新まで動作しますが、次回更新時にはリストを分割する必要があります。

#### データ統合

- [#17948](https://github.com/emqx/emqx/pull/17948) DynamoDB コネクターに AWS IAM ロール認証情報のサポートを追加しました。

  アクセスキーIDとシークレットアクセスキーの両方が省略された場合、EMQX は ECS タスクロールまたは EC2 インスタンスメタデータから一時的な認証情報を取得し、有効期限前に更新します。

- [#17472](https://github.com/emqx/emqx/pull/17472) IoTDB REST API コネクターのヘルスチェックのオーバーヘッドを削減しました。全データベースのリスト取得ではなく、制限付きのバージョンクエリを使用します。

- [#18917](https://github.com/emqx/emqx/pull/18917) Kafka、Confluent、Azure Event Hubs コネクターに IPv6 サポートを追加しました。

  - `bootstrap_hosts` は角括弧付き IPv6 アドレス（例: `[::1]:9092` や `[fd00::5]:9092,host2:9093`）を受け付けます。
  - コネクターは IPv6 アドレスのみを解決するホスト名や IPv6 アドレスを広告するブローカーに接続可能です。
  - 新しい `socket_opts.ip_family` オプションで IP アドレスファミリーを選択可能です。デフォルトの `auto` ではホスト名を IPv4 で試行し、失敗した場合に IPv6 を試みます。`ipv6` に設定すると IPv6 のみ、`ipv4` に設定すると IPv4 のみ接続します。

  Kafka クライアントライブラリのアップグレードにより、SASL 再認証中に保留中の同期プロデュース要求がタイムアウトする問題も修正されました。

#### デプロイメント

- [#17593](https://github.com/emqx/emqx/pull/17593) `emqx ctl relup upgrade` に `--force` フラグを追加しました。デフォルトでは、`data/patches/` に `*.beam` ホットパッチファイルが存在するとアップグレードを拒否します（アップグレード対象のモジュールを上書きするため）。`--force` を指定するとパッチを保持してアップグレードを続行します。

- [#18031](https://github.com/emqx/emqx/pull/18031) Red Hat Enterprise Linux 10、Rocky Linux 10、および互換ディストリビューション向けに Enterprise Linux 10 (EL10) パッケージを追加しました。

- [#18118](https://github.com/emqx/emqx/pull/18118) macOS 26 (Tahoe) パッケージのリリースを開始しました。

### バグ修正

#### コア MQTT 機能

- [#17729](https://github.com/emqx/emqx/pull/17729) WS/WSS リスナーのオプション更新時（例: TLS 証明書のローテーション）に発生することがあった一時的な「address already in use」エラーを修正しました。リスナーのポート再バインド時に OS が旧ソケットをまだ解放していない場合、EMQX は短時間リトライして更新失敗を回避します。

- [#17522](https://github.com/emqx/emqx/pull/17522) グローバルセッションレジストリから古いエントリを削除するスロットル付きバックグラウンドスイープを追加しました。セッション所有者がクリーンに登録解除せず終了した場合にエントリが残ることがありました。スイープはノードあたり毎秒最大500行を走査し、10分に1回以下の頻度で実行され、クリーンアップのオーバーヘッドを制限します。

- [#17573](https://github.com/emqx/emqx/pull/17573) MQTT v5 のユーザープロパティの解析コストを二次から線形に削減しました。

  以前は多くのユーザープロパティを含む CONNECT、PUBLISH、SUBSCRIBE パケットで、各プロパティをリスト末尾に追加するため接続プロセスのスケジューラ時間が超線形に増加していました。解析はエントリ数に対して線形にスケールし、ワイヤ順序を保持します。

- [#18356](https://github.com/emqx/emqx/pull/18356) ノード起動完了まで MQTT 接続を拒否するようにしました。これにより認証、認可、プラグインのセキュリティフックが有効になる前にリスナーがトラフィックを処理しなくなります。`GET /status` エンドポイントは起動中は HTTP 503 を返し、ロードバランサーがノードを回避可能です。

- [#18538](https://github.com/emqx/emqx/pull/18538) マルチテナンシークライアントリストが、異なるネームスペースで再接続した永続セッションに追従しない問題を修正しました。

  以前は、`clean_start=false` でセッションを再開したクライアントがネームスペースを変更した場合、`GET /api/v5/mt/ns/{ns}/client_list` は古いネームスペースにクライアントをリストし続け、新しいネームスペースのリストには含まれませんでした。現在はクライアントリストとネームスペースごとのクライアント数は常に接続時のネームスペースを反映します。

- [#18623](https://github.com/emqx/emqx/pull/18623) ノードシャットダウン時にアプリケーション停止前に MQTT リスナーを停止するようにしました。

  以前は、パブリッシュパスのアプリケーションが既に停止している間もリスナーがクライアントトラフィックを受け付けていました。そのため、パブリッシャーがルールエンジンなどで `hook_callback_exception` エラーを大量に発生させることがありました。現在はリスナーを先に停止し、アプリケーション停止中はクライアントトラフィックを処理しません。

  また、シャットダウン開始時点で `GET /status` はノードが稼働していないことを報告し、ロードバランサーが新規接続を停止します。

- [#18676](https://github.com/emqx/emqx/pull/18676) CONNECT パケット前に受信した不正なパケットで接続プロセスがクラッシュする問題を修正し、接続を閉じるようにしました。

#### セキュリティ強化

- [#18205](https://github.com/emqx/emqx/pull/18205) インポート時のデータバックアップアーカイブの検証を強化し、バックアップファイルの内容が対象のテーブルにのみ復元されるようにしました。

- [#17451](https://github.com/emqx/emqx/pull/17451) [#17553](https://github.com/emqx/emqx/pull/17553) バックアップファイルのダウンロード制限を強化し、Dashboard 管理者のみが Dashboard アカウントや API キー情報を含むアーカイブをダウンロード可能にしました。API キー呼び出しはそれらを含まないアーカイブのダウンロードは引き続き可能です。

- [#17531](https://github.com/emqx/emqx/pull/17531) JSON 処理中にまれに発生するメモリ安全性の問題を修正するため、`jiffy` を 1.1.4 にアップグレードしました。

- [#17652](https://github.com/emqx/emqx/pull/17652) Prometheus 設定 API が push gateway ヘッダーに保存された `Authorization` ヘッダー値を返す問題を修正し、レスポンスでこれらの値をマスクするようにしました。

- [#17857](https://github.com/emqx/emqx/pull/17857) ログおよびトレースにおける機密情報のマスキングを強化しました。対象は認証・認可バックエンドのクエリトレース、JWT 署名鍵、HTTP コネクターの機密リクエストヘッダー、Redis Sentinel パスワードなどです。

- [#18835](https://github.com/emqx/emqx/pull/18835) クラスター設定同期のデバッグログに成功した設定変更の結果を含めないようにしました。

  結果には HTTP 認証ヘッダーのテンプレートなどのコンパイル済みランタイム状態が含まれ、ログマスキングが適用されませんでした。

- [#18336](https://github.com/emqx/emqx/pull/18336) 読み取り専用 REST エンドポイントが秘密情報を平文で返さないようにしました。

  - `GET /listeners` と `GET /listeners/{id}` はリスナーの `ssl_options.password` を `******` と表示します。
  - `GET /exhooks` と `GET /exhooks/{name}` は gRPC クライアントの `ssl.password` を `******` と表示します。
  - `POST /license` の監査ログはリクエストボディを `******` と記録し、ライセンスキーが `GET /audit` に表示されません。

  リスナーや exhook サーバーの更新で `******` プレースホルダーを含むボディを送信しても、保存済み秘密は変更されません。

  HOCON を 0.46.3 にアップグレードし、配列型設定内の機密値を `******` と表示し、設定検証エラーログに機密値を出力しなくなりました。

- [#18262](https://github.com/emqx/emqx/pull/18262) [#18867](https://github.com/emqx/emqx/pull/18867) QUIC スタックを quicer 0.4.9（msquic 2.5.7）にアップグレードし、CVE-2026-32179 のセキュリティ修正を含めました。

#### アクセス制御

- [#17645](https://github.com/emqx/emqx/pull/17645) PingFederate などの厳格な ID プロバイダーで JWKS 取得が失敗する問題を修正しました。Erlang/OTP HTTP クライアントの空の `TE` ヘッダー送信により `503` 応答や TCP リセットが発生していました。EMQX はユーザー設定の `TE` ヘッダーがなければ JWKS リクエストで `TE: trailers` を送信します。

- [#17643](https://github.com/emqx/emqx/pull/17643) `plain` パスワードハッシュアルゴリズムが大文字・小文字の違いのみのパスワードを認証で受け入れてしまう問題を修正しました。

- [#17651](https://github.com/emqx/emqx/pull/17651) `POST /authentication` で認証器作成時にプロバイダーの秘密情報（JWT HMAC 秘密鍵、HTTP `Authorization` ヘッダー、リクエストボディのパスワード）がマスクされず返される問題を修正しました。作成レスポンスはリスト・取得エンドポイントと同様にマスクされます。

- [#18147](https://github.com/emqx/emqx/pull/18147) Dashboard と管理 API のスコープベース認可を強化しました。HTTP ルーターと同様にリクエストパスを正規化し、パーセントデコードや `.`、`..` セグメントの解決を行います。既知の API ルートにマッピングされないリクエストは、明示的スコープ制限のある Dashboard ユーザーに対して拒否されます。

- [#18200](https://github.com/emqx/emqx/pull/18200) スコープなしで作成された API キーの更新が失敗する問題を修正しました。API キーの作成・更新リクエストは `unset` またはロールデフォルトと一致するスコープリストをスコープなしとみなし、読み取り結果の再送信を許可し、将来互換性のある暗黙スコープを保持します。

  デフォルト管理者も明示的リストではなく暗黙のロールデフォルトスコープ（`unset`）を使用します。既存の明示的ロールデフォルトスコープを持つデフォルト管理者レコードは起動時に `unset` に変換されます。

- [#18997](https://github.com/emqx/emqx/pull/18997) スコープがロールのデフォルトセットと一致する Dashboard ユーザー保存時のエラーを修正しました。

  以前は API で返されるスコープリストを送信すると他のユーザー項目の変更が保存されないことがありました。管理者と閲覧者の切り替え時に「特権スコープは他のスコープと組み合わせ不可」や「非管理者は管理者専用スコープを持てない」エラーが発生することもありました。ユーザー API は `unset` またはロールデフォルトと一致するスコープリストをスコープなしとみなし、将来のロールデフォルト変更に追従します。

- [#18963](https://github.com/emqx/emqx/pull/18963) リクエストボディからオプションの `desc` または `enable` フィールドが省略された場合に `POST /api/v5/api_key` が HTTP 500 を返す問題を修正しました。キーは空のメモで作成され、デフォルトで有効になります。未知のリクエストフィールドは無視されます（例: `description` は `desc` のエイリアスではありません）。

#### データ統合

- [#17538](https://github.com/emqx/emqx/pull/17538) Redis Sentinel リソースが単一のグローバル Sentinel マネージャーを共有していた問題を修正しました。同一ノード上の複数 Redis Sentinel リソースは独立した Sentinel サーバーリストと認証情報を保持し、他リソースの Sentinel 設定経由で接続することを防止します。

- [#17627](https://github.com/emqx/emqx/pull/17627) PostgreSQL コネクターのプリペアドステートメント無効時のバッチ実行を修正しました。

  以前は異なる SQL テンプレートを用いた並行バッチ処理が同一 PostgreSQL 接続でインターリーブし、`protocol_violation` や `invalid_sql_statement_name` エラーを引き起こしていました。

- [#17701](https://github.com/emqx/emqx/pull/17701) PostgreSQL アクションで行を返す SQL（例: `SELECT ...`）をバッチ処理した際の混乱を招く `badarith` エラーを修正しました。

  PostgreSQL アクションのバッチ処理は行返却 SQL をサポートしません。EMQX はクラッシュせず明確な非対応 SQL エラーを返します。

- [#17954](https://github.com/emqx/emqx/pull/17954) 低書き込みレート時に GreptimeDB 非同期バッチがヘルスチェック後にフラッシュされず残る問題を修正しました。

- [#17414](https://github.com/emqx/emqx/pull/17414) Azure Blob Storage コネクターのヘルスチェックがコンテナ数過多でタイムアウトまたは大帯域コストを発生させる問題を修正しました。#16935 の関連修正です。

- [#17567](https://github.com/emqx/emqx/pull/17567) Kafka クライアントライブラリ `brod` を 4.5.4 から 4.5.5 にアップグレードしました。

  Kafka 2.2.0 対応で、ブローカーが `member_id_required` を返した際に brod が割り当てられたメンバー ID を破棄せず再試行に使用するよう修正しました。

- [#17597](https://github.com/emqx/emqx/pull/17597) 認証が必要な MongoDB 8.0+ への接続失敗を修正しました。ドライバーは認証前に `buildInfo` を問い合わせて認証機構を選択していましたが、MongoDB 8.0 は認証済み呼び出し元に制限しました。ドライバーはプローブをスキップし、すべてのサポート MongoDB バージョンが受け入れる SCRAM-SHA-1 を直接使用します。

- [#17605](https://github.com/emqx/emqx/pull/17605) Oracle アクションのプリペアドステートメント準備・状態チェックを修正し、SQL 実行なしで解析し、サポートされないトップレベル DDL/DCL/TCL 文を拒否します。ペイロードプレースホルダーが最後のバインドパラメータの場合の 4000 バイト超テキストペイロード対応も改善しました。

- [#17624](https://github.com/emqx/emqx/pull/17624) GCP PubSub Consumer Source が、トピックのサブスクリプション作成権限がないサービスアカウントで作成され、後に権限付与されても `connected` 状態にならない問題を修正しました。

- [#17716](https://github.com/emqx/emqx/pull/17716) Confluent プロデューサーコネクターに TLS ピア検証を有効化するオプションを追加しました。

- [#17720](https://github.com/emqx/emqx/pull/17720) GCP PubSub プロデューサーおよびコンシューマーコネクターに TLS ピア検証を有効化するオプションを追加しました。

- [#17947](https://github.com/emqx/emqx/pull/17947) HTTP コネクターの更新後にアクションバッファワーカーがブロックされ、メッセージが次のリトライ間隔までキューに残る問題を修正しました。

- [#17961](https://github.com/emqx/emqx/pull/17961) Kafka および Pulsar コネクターがヘルスチェックタイムアウト時に `disconnected` 状態になる問題を修正しました。`connecting` 状態に入り、内部バッファメッセージ破棄を防止します。

- [#17994](https://github.com/emqx/emqx/pull/17994) Kafka プロデューサーアクションのリトライメトリクスを修正しました。`retried`、`retried.success`、`retried.failed` カウンターはブローカー再接続後に内部バッファが再送したメッセージを反映し、オペレーターがリトライメッセージの最終成功・失敗を判別可能です。以前は内部リトライ数に関わらず `0` のままでした。`success` と `failed` カウンターは影響を受けず重複カウントされません。

- [#18082](https://github.com/emqx/emqx/pull/18082) Kafka プロデューサーでメモリモードバッファに対し `max_linger_time` が再度有効になりました（Kafka クライアントライブラリ wolff を 4.1.11 にアップグレード）。バッファに満たない場合は `max_linger_time` まで待機し、完全なバッチが揃うかタイムアウトで送信します。デフォルト `max_linger_time=0` は即時送信の従来動作を維持します。

  Azure Event Hubs と Confluent プロデューサーコネクターも同じクライアントライブラリを使用しており同様です。

- [#18251](https://github.com/emqx/emqx/pull/18251) 強制停止されたワーカー後に古い gRPC チャネルが残り再起動に失敗する GreptimeDB コネクターを修正しました。

- [#18920](https://github.com/emqx/emqx/pull/18920) 高負荷書き込み時に接続状態が `connected` と `disconnected` を行き来する GreptimeDB コネクターを修正しました。ヘルスチェックは保留中書き込みの後ろで待たず、GreptimeDB が応答しない場合のみ失敗します。

- [#18301](https://github.com/emqx/emqx/pull/18301) Elasticsearch アクションの `index` と `id` 値をリクエストパス作成時に URL エンコードするようにしました。`#` や `/` などの文字がリクエストターゲットを変更せず単一パスセグメント内のリテラル文字として扱われます。JSON リクエストボディは影響を受けません。

- [#18317](https://github.com/emqx/emqx/pull/18317) GCP PubSub プロデューサー・コンシューマーコネクターの API レスポンスで `service_account_json` 値がマスクされず返される問題を修正しました。コネクター作成・取得時は `******` と表示されます。

- [#18328](https://github.com/emqx/emqx/pull/18328) Snowflake コネクターが Snowflake エンドポイント接続時に設定された `ssl` オプションを適用するようにしました。以前は `ssl` 設定を無視し、サーバー証明書を検証していませんでした。

- [#18465](https://github.com/emqx/emqx/pull/18465) ClickHouse、TDengine、SQL Server アクションのテンプレート `INSERT` 文の検証と安全なレンダリングを改善し、MySQL アクションのバッチ挿入時も対応しました。EMQX は SQL コンテキストに応じて値をエスケープします。

  ClickHouse、TDengine、SQL Server はアクション作成時に無効なテンプレートを拒否します。MySQL は解析エラーをログに出しますがアクションは作成され、実行時にバッチリクエストが失敗します。

  これは互換性を破壊する変更です。コメントやサポートされない SQL 構文を含む既存テンプレートは新パーサーで使用前に更新が必要です。サポート構文は定数、文字列・文字列補間、算術、関数、条件、条件演算子です。MySQL は `ON DUPLICATE KEY UPDATE`、ClickHouse は `FORMAT Values` と `FORMAT JSONCompactEachRow`、TDengine は `INSERT ... USING ... TAGS` とテーブル識別子補間をサポートします。

  MySQL ブリッジはすべての接続で `ANSI_QUOTES` と `NO_BACKSLASH_ESCAPES` を無効化します。ClickHouse ブリッジは SQL テンプレートからバッチ値区切り文字を推論し、設定された `batch_value_separator` を無視します。

- [#18846](https://github.com/emqx/emqx/pull/18846) データ統合の SQL テンプレートレンダリングを修正しました。Doris バッチ挿入は Doris 互換の構文とテキスト・バイナリ値のエスケープを使用します。MySQL テンプレートはエスケープされたドル記号を正しく処理します。

- [#18762](https://github.com/emqx/emqx/pull/18762) TDengine アクションのエラー報告を修正しました。アクションが見つからない場合、エラーにコネクター ID ではなくアクション ID を表示するようにしました。以前は有効なコネクター ID が無効と誤解される内容でした。

#### プラグイン

- [#17875](https://github.com/emqx/emqx/pull/17875) クラスターのプラグイン設定に存在せずローカルで実行もされていない古い展開済みプラグインディレクトリを無視するようにプラグイン管理 HTTP API を修正しました。

  これらの古いパッケージはプラグイン一覧・詳細・設定・スキーマレスポンスに表示されず、プラグイン操作 API で操作できず、HTTP インストール API で同一パッケージの再インストールを妨げません。設定済みのプリインストールプラグインは引き続き表示され、ドキュメント化されたプリインストールワークフローに従います。

  EMQX は起動時と HTTP API アクセス時に、展開済みで `plugins.states` に有効・無効いずれにも設定されていないプラグインパッケージに対してエラーをログに出力します。

- [#17710](https://github.com/emqx/emqx/pull/17710) CLI 経由でプラグインをインストールする際の `failed_to_get_plugin_config_from_cluster` 警告の多発を修正しました。

  `emqx ctl plugins install` コマンドは HTTP API と同様に新規インストール時にクラスター設定の検索をスキップする `fresh_install` モードでインストールし、クラスター内の全ノードで `config_not_found_on_node` 警告が繰り返されるのを防止します。

  クラスター全体インストール用に `emqx ctl plugins install` に `--cluster` フラグを追加しました。指定時は単一コマンドで全稼働ノードにパッケージを配布・インストールします。

- [#17934](https://github.com/emqx/emqx/pull/17934) プラグインパッケージインストール時に、コードロード前にアプリケーション宣言、設定スキーマ、デフォルト設定の検証を行うように修正しました。

- [#18334](https://github.com/emqx/emqx/pull/18334) ノード再起動後にプラグインが起動に失敗する問題を修正しました。

  `emqx_plugins` アプリケーションを依存として宣言するプラグインはノード起動が 10 秒遅延し、再起動後に有効だが実行されていない状態になります。EMQX はこの依存宣言を無視し警告をログに出します。バンドルプラグインは宣言しません。起動タイムアウト時のエラーログには起動していない宣言済みアプリケーションを列挙します。

- [#18338](https://github.com/emqx/emqx/pull/18338) プラグインの起動順序を変更し、すべての EMQX アプリケーション起動後にプラグインを起動するようにしました。プラグインは任意の EMQX アプリケーションを `applications` リストに宣言可能です。以前は起動遅延アプリケーション（例: `emqx_management`）を宣言するプラグインはノード再起動後に起動失敗していました。

- [#18469](https://github.com/emqx/emqx/pull/18469) ホットアップグレード（relup）プラグインがターゲットバージョン文字列の検証とアップグレードパス互換性チェックを追加し、不適合または不正なアップグレードパッケージを拒否し、インストール済みリリースを削除・上書きしないようにしました。

#### ゲートウェイ

- [#17885](https://github.com/emqx/emqx/pull/17885) LwM2M ゲートウェイが登録・更新 MQTT レポートに `password`、`secret`、`private_key`、`access_token` などの機密 REGISTER クエリフィールドを含める問題を修正しました。

- [#18044](https://github.com/emqx/emqx/pull/18044) LwM2M 登録クエリパラメータを含む構造化 CoAP パケットのデバッグログから機密フィールドをマスクしました。

- [#17395](https://github.com/emqx/emqx/pull/17395) CoAP ゲートウェイの Observe 通知が `gateway.coap.notify_type` 設定を尊重し、ACK 待ち中に保留中の確認可能 Observe 通知をキューに入れ、無音破棄しないように修正しました。

- [#17427](https://github.com/emqx/emqx/pull/17427) JT/T 808 ゲートウェイのスキーマ検証で、`allow_anonymous` が `true` の場合に `registry` と `authentication` の URL が空または省略可能にしました。以前は `not_empty` バリデータが両フィールドに適用され、匿名モードで空文字を送信すると 400 エラーになっていました。

- [#17581](https://github.com/emqx/emqx/pull/17581) JT/T 808 ゲートウェイが認証時に受け入れた電話番号を接続識別子として使用し、不一致の登録コード認証試行や異なる電話番号のアップリンクフレームを拒否するように修正しました。

- [#18825](https://github.com/emqx/emqx/pull/18825) CoAP Observe 通知が ACK 受信失敗後に再送されず、後続通知が保留キューでブロックされる問題を修正しました。

- [#18652](https://github.com/emqx/emqx/pull/18652) MQTT-SN がスリーピングクライアントのスリープ期間満了時に設定された Will メッセージをパブリッシュし、通常切断時には Will メッセージをパブリッシュしないように修正しました。

#### クラスタリング

- [#17770](https://github.com/emqx/emqx/pull/17770) クラスター RPC レイヤーが予期しない理由で中断した場合に、設定更新コマンド（REST API と CLI）が `function_clause` クラッシュレポートでクラッシュする問題を修正しました。これらの失敗は呼び出し元に構造化エラーとして返されます。

- [#18000](https://github.com/emqx/emqx/pull/18000) クラスタリング対応ライセンスを持つピアが存在するクラスターにコミュニティ（シングルノード）ライセンスのノードが参加すると起動クラッシュループする問題を修正しました。

  以前はピアのライセンスが参加ノードに複製される前にクラスターメンバーシップが確立すると、ノードは `SINGLE_NODE_LICENSE` エラーで起動を拒否し、自動再起動監督下でクラッシュループしました。現在はクラスタリングライセンスの同期を有界猶予期間待機してから起動します。猶予期間経過後もクラスタリングライセンスを持つノードが存在しなければ起動拒否します。

- [#18013](https://github.com/emqx/emqx/pull/18013) ローカル設定と異なる永続化された `mqtt.max_packet_size` を持つクラスターに参加中のノードが終了する問題を修正しました。EMQX はリスナー起動前にリスナーリフレッシュの副作用をスキップし、EMQX アプリケーション起動時に同期設定からリスナーを作成します。

- [#18861](https://github.com/emqx/emqx/pull/18861) `emqx_router_tool:scan_missing_routes/1` と `emqx_router_tool:reconcile_missing_routes/1` に渡されるオプションの検証を追加しました。

  以前は無効な `chunk` や `sleep_ms` 値がスキャンスロットリングを無効化したり、低レベルのランタイムエラーを引き起こしていました。ツールは無効な値や未知のオプションキー（例: 誤字の `chunks`）を拒否し、問題のあるオプションを特定するエラーを発生させます。

#### 可観測性

- [#17709](https://github.com/emqx/emqx/pull/17709) JSON ログフォーマッターのクラッシュを修正しました。ログフィールドにタプル値（例: 認証トレースイベントの `result` フィールド）が含まれ、アクティブなフォーマッター設定に `chars_limit` がない場合に発生していました。これにより接続を受け入れた認証器のイベントが出力されませんでした。現在は正しくフォーマットされます。

- [#17886](https://github.com/emqx/emqx/pull/17886) Prometheus にパブリッシュクオータ超過パケットメトリクス `emqx_packets_publish_quota_exceeded` を公開しました。

- [#18107](https://github.com/emqx/emqx/pull/18107) クラスター参加中のノードで Dashboard メトリクス API (`GET /api/v5/monitor_current` と `GET /api/v5/monitor`) が `500 INTERNAL_ERROR` を返す問題を修正しました。

  参加ノードがアプリケーション再起動中にメトリクスサンプリングが失敗していました。現在は失敗を許容し、残りの到達可能ノードの集計を返し警告ログを出力します。`DELETE /api/v5/monitor` の成功時に毎回出ていた `clear_monitor_metrics_rpc_errors` 警告も修正しました。

- [#18694](https://github.com/emqx/emqx/pull/18694) SSO 認証ユーザーが作成した監査レコードを含む結果ページで `GET /api/v5/audit` が HTTP 500 を返す問題を修正しました。

#### API

- [#18067](https://github.com/emqx/emqx/pull/18067) 非 ASCII 文字（例: 中国語）を含むファイル名のファイルをリストできないファイル転送ファイル API (`GET /api/v5/file_transfer/files`) の問題を修正しました。

- [#18385](https://github.com/emqx/emqx/pull/18385) 無効な Unicode エスケープシーケンスを含む設定を `PUT /configs` で送信すると内部エラーになる問題を修正しました。現在は無効なエスケープを特定する検証エラーを返します。

- [#18816](https://github.com/emqx/emqx/pull/18816) [#18820](https://github.com/emqx/emqx/pull/18820) リクエストボディに `enable` フィールドがない場合に `PUT /api/v5/telemetry/status` が Erlang スタックトレース付きで `500 INTERNAL_ERROR` を返す問題を修正しました。

  現在は検証メッセージ付きの `400 BAD_REQUEST` を返します。`enable` は必須でありデフォルト値はなく、API ドキュメントも更新されました。

#### CLI

- [#18823](https://github.com/emqx/emqx/pull/18823) `emqx ctl listeners` 出力のフィールド名誤字を修正しました。

  リスナーの有効フラグが `enbale` と表示されていました。現在は `enable` と表示されます。出力を解析するスクリプトは修正が必要です。

## 5.10.4

*リリース日: 2026-06-01*

EMQX 5.10.4 へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### セキュリティ強化

- [#17039](https://github.com/emqx/emqx/pull/17039) API キーによる Dashboard ユーザーアカウント管理エンドポイントへのアクセスを制限しました。

  以前は `administrator` ロールの API キーが HTTP Basic 認証で Dashboard ユーザー管理エンドポイント `POST/DELETE /users/:username/mfa` と `POST /users/:username/change_pwd` を呼び出せました。これにより API キーが他の Dashboard ユーザーの MFA をリセット・無効化したりパスワードを変更できていました。

  これらのエンドポイントは API キー経由アクセス時に `401 API_KEY_NOT_ALLOW` を返すようになり、既存の `/users`、`/users/:username`、`/logout`、`/api_key` への API キーアクセス拒否ポリシーと整合します。Dashboard ユーザーは従来通り Dashboard UI からベアラートークン（JWT）セッションで自身の MFA とパスワードを管理可能です。

- [#17169](https://github.com/emqx/emqx/pull/17169) API キーによる Dashboard アカウントと API キーのデータバックアップエンドポイントからのエクスポート・インポートを制限しました。

  API キーで呼ばれた `POST /data/export` は結果アーカイブから `dashboard_users` と `api_keys` の mnesia テーブルセットを静かに除外します。API キーで呼ばれた `POST /data/import` はアップロードされたバックアップにこれらのテーブルセットが含まれる場合に `403 FORBIDDEN` を返します。

  Dashboard のベアラートークン（ログイン）呼び出しは影響を受けず、Dashboard ユーザーと API キーを含む完全なデータベースのバックアップと復元が可能です。

  これは特権昇格のギャップを解消します。既存の `/users` と `/api_key` エンドポイントは API キーによる Dashboard ログイン資格情報と API キー記録へのアクセスを拒否しますが、API キーホルダーはデータバックアップエンドポイント経由でこれらの制限を回避できていました。

- [#17188](https://github.com/emqx/emqx/pull/17188) 認証不要な `GET /status?format=json` レスポンスから EMQX リリースバージョン (`rel_vsn`) を削除し、ブローカーのバージョン情報漏洩を防止しました。バージョンは認証済みノード情報 API で引き続き取得可能です。

- [#17200](https://github.com/emqx/emqx/pull/17200) アップロードされた tarball のパス・トラバーサルに対するプラグインインストールエンドポイントの強化。

  - インストールパスは、エントリに `..` セグメントが含まれるかプラグインインストールディレクトリ外に解決される tarball の展開を拒否します。
  - `emqx ctl plugins allow <name-vsn>` エントリーは発行から 5 分で期限切れになり、SHA-256 ハッシュでパッケージをピン留め可能です。`emqx ctl plugins allow <name-vsn> sha256:<HEX>` は 64 文字の小文字 16 進ダイジェストを受け付け、内容が一致しないアップロードを `403 Forbidden` で拒否します。`sha256:` 引数省略時は `<name-vsn>.tar.gz` 名の任意ペイロードを受け入れる従来動作を維持します。

- [#17202](https://github.com/emqx/emqx/pull/17202) `POST /api/v5/plugins/install`（および Dashboard アップロード）による成功したプラグインインストールは、クラスターワイドの許可エントリーを即時取り消し、同一許可の再利用を防止します。5 分の TTL は維持されますが、一般的な経路で窓口を早期に閉じます。

- [#17253](https://github.com/emqx/emqx/pull/17253) 公式ダウンロードサイトでプラグインパッケージの `.sha256` チェックサムサイドカーを公開し、ダウンロードしたプラグインアーカイブの整合性検証を可能にしました。

- [#17276](https://github.com/emqx/emqx/pull/17276) 公式 EMQX Docker イメージの強化。

  - ランタイムイメージビルド時に Debian セキュリティアップグレードを適用し、最新のパッチ済み `libssl3t64` を取得。
  - 未使用の `libgnutls30t64` パッケージを削除。EMQX は Erlang/OTP 経由で OpenSSL を使用し GnuTLS をリンクしないため、`curl` の推移的依存としてのみ存在していました。
  - Debian `curl` パッケージは `librtmp1` 経由で `libgnutls30t64` を再導入するため、OpenSSL ベースのスタティックリンク済みバイナリ（[stunnel/static-curl](https://github.com/stunnel/static-curl)）に置換。`curl` を呼ぶコンテナヘルスチェックは変更なしで動作します。

- [#17314](https://github.com/emqx/emqx/pull/17314) PROXY-Protocol v2 の SSL Common Name / Subject をサニタイズし、制御文字の混入を防止しました。

  `proxy_protocol = true` 設定のリスナーは、PROXY-Protocol SSL TLV バイトに ASCII 制御文字が含まれる接続を拒否します（MQTT 取り込みの `clientid`、`username`、`password` で既に拒否されているバイトクラスと同じ）。これにより攻撃者制御のバイトが `${cert_common_name}` や `${cert_subject}` テンプレート経由で HTTP 認証・認可・ルールエンジンヘッダーに持ち込まれるのを防ぎます。

  HTTP 認証・認可クライアントは、レンダリングされたヘッダー名や値に CR、LF、NUL バイトが含まれる場合にリクエスト送信を拒否します。

- [#17322](https://github.com/emqx/emqx/pull/17322) MQTT の `clientid`、`username`、`password` に適用されるバイトクラスチェックを、`ClientInfo` と HTTP リクエストテンプレートの他のフィールドにも拡張しました。

  - `peersni`（TLS Server Name Indication、PROXY-Protocol v2 `authority` TLV 由来も含む）を接続取り込み境界で検証し、制御文字があると接続拒否・警告ログ出力。
  - `mqtt.client_attrs_init` Variform 式で生成されるクライアント属性値は制御文字を含む場合に破棄し（警告付き）、`${client_attrs.tns}` などのテンプレートで注入バイトを下流に持ち込めないように。
  - HTTP アクション・ブリッジコネクタのヘッダー描画は、レンダリング結果に NUL、CR、LF が含まれるヘッダーを破棄。

- [#17581](https://github.com/emqx/emqx/pull/17581) JT/T 808 ゲートウェイが認証時に受け入れた電話番号を接続識別子として使用し、不一致の登録コード認証試行や異なる電話番号のアップリンクフレームを拒否するように修正しました。

- [#17276](https://github.com/emqx/emqx/pull/17276) 公式 EMQX Docker イメージの強化（詳細は上記参照）。

---

以降のバージョンについては、同様に公式リリースノートを参照してください。
