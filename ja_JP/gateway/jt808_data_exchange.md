# JT/T 808 ゲートウェイデータ交換フォーマット

本ページでは、**emqx_jt808** と **EMQX** 間のデータ交換フォーマットについて定義します。

## プロトコルバージョン対応

ゲートウェイは **JT/T 808-2013** および **JT/T 808-2019** の両プロトコルバージョンをサポートしています。  
プロトコルバージョンはメッセージヘッダーのバージョンフラグに基づいて自動判別されます：

- **2013年版**: メッセージヘッダーのプロパティ bit14 = 0
  - 端末電話番号: BCD[6]（12桁）
  - メッセージヘッダー長: 12 / 16 バイト
- **2019年版**: メッセージヘッダーのプロパティ bit14 = 1
  - 端末電話番号: BCD[10]（20桁）
  - メッセージヘッダー長: 17 / 21 バイト

JSONベースのデータ交換では、2019年版のメッセージは `header` に `proto_ver` フィールドを含み、プロトコルバージョンを示します。

## 慣例

- ペイロードは JSON 形式で組み立てられます。
- JSON のキー名はすべて小文字です。

## JSON 構造例

### 端末 → サーバー

```json
{
  "header" : {
    "msg_id" : 1,
    "encrypt": 0,
    "len": VAL,
    "phone": 13900000000,
    "msg_sn": 0
  },
  "body": {
    "seq": 1,
    "id": 1,
    "result": 0
  }
}
```

### サーバー → 端末

```json
{
  "header": {
    "msg_id": 32769,
    "encrypt": 0,
    "phone": 13900000000,
    "msg_sn": 0
  },
  "body": {
    "seq": 1,
    "id": 1,
    "result": 0
  }
}
```

## データ型対応表

| JT808 定義型 | JSON 型   | コメント      |
| :----------: | :-------: | :------------ |
| BYTE         | integer   | 10進数表現    |
| WORD         | integer   | 10進数表現    |
| DWORD        | integer   | 10進数表現    |
| BYTE(n)      | string    |               |
| BCD(n)       | string    |               |
| STRING       | string    |               |

## フィールドマッピング表

### メッセージヘッダー項目

#### JT/T 808-2013 メッセージヘッダー

| フィールド名           | JSON キー  | 値の型    | JSON 内の型  |
| :--------------------: | :--------: | :-------: | :----------: |
| メッセージID           | msg_id     | word      | integer      |
| 暗号化方式             | encrypt    | word      | integer      |
| 端末電話番号           | phone      | bcd(6)    | string       |
| メッセージシリアル番号 | msg_sn     | word      | integer      |

オプション項目：

| フィールド名         | JSON キー  | 値の型    | JSON 内の型  |
| :------------------: | :--------: | :-------: | :----------: |
| 総分割数             | frag_total | word      | integer      |
| 分割番号             | frag_sn    | word      | integer      |

#### JT/T 808-2019 メッセージヘッダー

| フィールド名           | JSON キー  | 値の型    | JSON 内の型  |
| :--------------------: | :--------: | :-------: | :----------: |
| メッセージID           | msg_id     | word      | integer      |
| 暗号化方式             | encrypt    | word      | integer      |
| プロトコルバージョン   | proto_ver  | byte      | integer      |
| 端末電話番号           | phone      | bcd(10)   | string       |
| メッセージシリアル番号 | msg_sn     | word      | integer      |

オプション項目：

| フィールド名         | JSON キー  | 値の型    | JSON 内の型  |
| :------------------: | :--------: | :-------: | :----------: |
| 総分割数             | frag_total | word      | integer      |
| 分割番号             | frag_sn    | word      | integer      |

**備考**：

- `proto_ver` は JT/T 808-2019 のみ存在し、現在の値は `1` です。
- `frag_total` と `frag_sn` が存在する場合、メッセージ本文は分割された長大メッセージです。

### メッセージ本文項目

#### 端末からの一般応答 `"msg_id": 1` (0x0001)

| フィールド名           | JSON キー | 値の型 | JSON 内の型 |
| :--------------------: | :-------: | :----: | :---------: |
| 応答シリアル番号       | seq       | word   | integer     |
| 応答ID                 | id        | word   | integer     |
| 結果                   | result    | byte   | integer     |

#### プラットフォームからの一般応答 `"msg_id": 32769` (0x8001)

| フィールド名           | JSON キー | 値の型 | JSON 内の型 |
| :--------------------: | :-------: | :----: | :---------: |
| 応答シリアル番号       | seq       | word   | integer     |
| 応答ID                 | id        | word   | integer     |
| 結果                   | result    | byte   | integer     |

#### 端末ハートビート `"msg_id": 2` (0x0002)

空の JSON

#### 分包再送要求 `"msg_id": 32771` (0x8003)

##### JT/T 808-2013

| フィールド名               | JSON キー | 値の型          | JSON 内の型       |
| :------------------------: | :-------: | :-------------: | :---------------: |
| 元メッセージシリアル番号   | seq       | word            | integer           |
| 再送パッケージ総数         | length    | byte            | integer           |
| 再送パッケージIDリスト     | ids       | byte(2*length)  | 整数のリスト      |

##### JT/T 808-2019

| フィールド名               | JSON キー | 値の型          | JSON 内の型       |
| :------------------------: | :-------: | :-------------: | :---------------: |
| 元メッセージシリアル番号   | seq       | word            | integer           |
| 再送パッケージ総数         | length    | word            | integer           |
| 再送パッケージIDリスト     | ids       | byte(2*length)  | 整数のリスト      |

**備考**：2019年版では再送パッケージ総数の型が BYTE から WORD に変更されています。

#### サーバー時刻照会応答 `"msg_id": 32772` (0x8004、2019年追加)

| フィールド | JSON キー | 値の型 | JSON 内の型 |
| :--------: | :-------: | :----: | :---------: |
| 年         | year      | word   | integer     |
| 月         | month     | byte   | integer     |
| 日         | day       | byte   | integer     |
| 時         | hour      | byte   | integer     |
| 分         | minute    | byte   | integer     |
| 秒         | second    | byte   | integer     |

#### 端末登録 `"msg_id": 256` (0x0100)

##### JT/T 808-2013

| フィールド名           | JSON キー    | 値の型    | JSON 内の型 |
| :--------------------: | :----------: | :-------: | :---------: |
| 省ID                  | province     | word      | integer     |
| 市ID                  | city         | word      | integer     |
| 製造商ID              | manufacturer | byte(5)   | string      |
| 端末モデル            | model        | byte(20)  | string      |
| 端末ID                | dev_id       | byte(7)   | string      |
| 車両色                | color        | byte      | integer     |
| 車両識別番号          | license_number | string   | string      |

##### JT/T 808-2019

| フィールド名           | JSON キー    | 値の型    | JSON 内の型 |
| :--------------------: | :----------: | :-------: | :---------: |
| 省ID                  | province     | word      | integer     |
| 市ID                  | city         | word      | integer     |
| 製造商ID              | manufacturer | byte(11)  | string      |
| 端末モデル            | model        | byte(30)  | string      |
| 端末ID                | dev_id       | byte(30)  | string      |
| 車両色                | color        | byte      | integer     |
| 車両識別番号          | license_number | string   | string      |

**備考**：2019年版では製造商ID（5→11バイト）、端末モデル（20→30バイト）、端末ID（7→30バイト）のフィールド長が拡張されています。

#### 端末登録応答 `"msg_id": 33024` (0x8100)

| フィールド名           | JSON キー | 値の型 | JSON 内の型 |
| :--------------------: | :-------: | :----: | :---------: |
| 応答シリアル番号       | seq       | word   | integer     |
| 結果                   | result    | byte   | integer     |

成功登録後のみ存在するフィールド：

| フィールド名           | JSON キー  | 値の型 | JSON 内の型 |
| :--------------------: | :--------: | :----: | :---------: |
| 認証コード             | auth_code  | string | string      |

#### 端末ログアウト `"msg_id": 3` (0x0003)

空の JSON

#### サーバー時刻照会要求 `"msg_id": 4` (0x0004、2019年追加)

空の JSON

#### 端末再送分包要求 `"msg_id": 5` (0x0005、2019年追加)

| フィールド名             | JSON キー | 値の型          | JSON 内の型       |
| :----------------------: | :-------: | :-------------: | :---------------: |
| 元メッセージシリアル番号 | seq       | word            | integer           |
| 再送総数                 | length    | word            | integer           |
| 再送パケットIDリスト     | ids       | byte(2*length)  | 整数のリスト      |

#### 端末認証 `"msg_id": 258` (0x0102)

##### JT/T 808-2013

| フィールド名   | JSON キー | 値の型 | JSON 内の型 |
| :------------: | :-------: | :----: | :---------: |
| 認証コード     | code      | string | string      |

##### JT/T 808-2019

| フィールド名             | JSON キー    | 値の型    | JSON 内の型 |
| :---------------------: | :----------: | :-------: | :---------: |
| 認証コード長さ          | code_len     | byte      | integer     |
| 認証コード              | code         | string    | string      |
| 端末IMEI                | imei         | byte(15)  | string      |
| ソフトウェアバージョン長 | sw_version_len | byte    | integer     |
| ソフトウェアバージョン   | sw_version   | string    | string      |

**備考**：2019年版では認証コード長さ、端末IMEI、ソフトウェアバージョン情報などのフィールドが追加されています。

#### 端末パラメータ設定 `"msg_id": 33027` (0x8103)

| フィールド名               | JSON キー | 値の型                                    | JSON 内の型                                  |
| :-----------------------: | :-------: | :--------------------------------------: | :------------------------------------------: |
| パラメータ総数            | length    | byte                                     | integer                                     |
| パラメータ項目リスト      | params    | list                                     | id と value のリスト `[{"id":ID, "value": VAL}, ...]` |
| パラメータ項目ID          | id        | dword                                    | integer                                     |
| パラメータ値              | value     | [パラメータ値型マッピング](#parameter-value-type-mapping)参照 | [パラメータ値型マッピング](#parameter-value-type-mapping)参照 |

プロトコルに基づくパラメータIDの説明。

##### パラメータ値型マッピング

パラメータ値の JSON 型はパラメータIDに対応するデータ型により決まります：

| パラメータID範囲           | データ型 | JSON 内の型           | 説明                                         |
| :-----------------------: | :------: | :-------------------: | :------------------------------------------: |
| BYTE型パラメータ           | byte     | integer               | 例: 0x0084, 0x0090–0x0095                    |
| WORD型パラメータ           | word     | integer               | 例: 0x0031, 0x0081–0x0082                    |
| DWORD型パラメータ          | dword    | integer               | 例: 0x0001–0x0007, 0x0018–0x001C             |
| STRING型パラメータ         | string   | string                | 例: 0x0010–0x0017, 0x0040–0x0049, 0x0083     |
| 0x0110–0x01FF             | byte[8]  | string (base64エンコード) | CANバスID独立取得パラメータ                   |
| その他不明パラメータ       | byte[n]  | string (base64エンコード) | 予約パラメータまたはベンダー定義パラメータ   |

例：

```json
{
  "header": { "msg_id": 33027, ... },
  "body": {
    "length": 3,
    "params": [
      {"id": 1, "value": 60},
      {"id": 16, "value": "cmnet"},
      {"id": 272, "value": "AQIDBAUG"}
    ]
  }
}
```

#### 端末パラメータ照会 `"msg_id": 33028` (0x8104)

空の JSON

#### 特定端末パラメータ照会 `"msg_id": 33030` (0x8106)

| フィールド名           | JSON キー | 値の型          | JSON 内の型              |
| :-------------------: | :-------: | :-------------: | :----------------------: |
| パラメータ総数        | length    | byte            | integer                  |
| パラメータIDリスト    | ids       | byte(2*length)  | 整数のリスト `[1, 2, 3, 4, ...]` |

パラメータIDリストの要素は整数です。

#### 端末応答パラメータ `"msg_id": 260` (0x0104)

| フィールド名               | JSON キー | 値の型 | JSON 内の型                                  |
| :-----------------------: | :-------: | :----: | :------------------------------------------: |
| 応答シリアル番号           | seq       | word   | integer                                     |
| 応答パラメータ数           | length    | byte   | integer                                     |
| パラメータ項目リスト       | params    | list   | id と value のリスト `[{"id":ID, "value": VAL}, ...]` |
| パラメータ項目ID           | id        | dword  | integer                                     |
| パラメータ値               | value     | --     | [0x8103 パラメータ値型マッピング](#parameter-value-type-mapping)参照 |

プロトコルに基づくパラメータIDの説明。

#### 端末制御 `"msg_id": 33029` (0x8105)

| フィールド名       | JSON キー | 値の型 | JSON 内の型 |
| :----------------: | :-------: | :----: | :---------: |
| コマンド語         | command   | byte   | integer     |
| コマンドパラメータ | param     | string | string      |

#### 端末属性照会 `"msg_id": 33031` (0x8107)

空の JSON

#### 端末属性応答 `"msg_id": 263` (0x0107)

##### JT/T 808-2013

| フィールド名               | JSON キー       | 値の型    | JSON 内の型 |
| :------------------------: | :-------------: | :-------: | :---------: |
| 端末タイプ                 | type            | word      | integer     |
| 製造商ID                  | manufacturer    | byte(5)   | string      |
| 端末モデル                 | model           | byte(20)  | string      |
| 端末ID                    | id              | byte(7)   | string      |
| 端末SIMカードICCID        | iccid           | byte(10)  | string      |
| 端末ハードウェアバージョン | hardware_version | string    | string      |
| 端末ファームウェア番号     | firmware_version | string    | string      |
| GNSSモジュール属性         | gnss_prop       | byte      | integer     |
| 通信モジュール属性         | comm_prop       | byte      | integer     |

##### JT/T 808-2019

| フィールド名               | JSON キー       | 値の型    | JSON 内の型 |
| :------------------------: | :-------------: | :-------: | :---------: |
| 端末タイプ                 | type            | word      | integer     |
| 製造商ID                  | manufacturer    | byte(11)  | string      |
| 端末モデル                 | model           | byte(30)  | string      |
| 端末ID                    | id              | byte(30)  | string      |
| 端末SIMカードICCID        | iccid           | byte(10)  | string      |
| 端末ハードウェアバージョン | hardware_version | string    | string      |
| 端末ファームウェア番号     | firmware_version | string    | string      |
| GNSSモジュール属性         | gnss_prop       | byte      | integer     |
| 通信モジュール属性         | comm_prop       | byte      | integer     |

**備考**：

- 2019年版では製造商ID（5→11バイト）、端末モデル（20→30バイト）、端末ID（7→30バイト）のフィールド長が拡張されています。
- 端末タイプに bit8 が追加され、トレーラーのサポート／接続を示します。
- 端末ハードウェアバージョン番号およびファームウェア番号の長さはバイナリメッセージ解析に使用され、外部には公開されません。

#### 端末アップグレードパッケージコマンド `"msg_id": 33032` (0x8108)

| フィールド名           | JSON キー   | 値の型    | JSON 内の型               |
| :-------------------: | :---------: | :-------: | :-----------------------: |
| アップグレード種別    | type        | byte      | integer                   |
| 製造商ID              | manufacturer| byte(5)   | string                    |
| バージョン番号長さ    | ver_len     | byte      | integer                   |
| バージョン番号        | version     | string    | string                    |
| アップグレードパッケージ長 | fw_len      | dword     | integer                   |
| アップグレードパッケージ | firmware    | binary    | string(base64エンコード)  |

#### 端末アップグレード結果通知 `"msg_id": 264` (0x0108)

| フィールド名       | JSON キー | 値の型 | JSON 内の型 |
| :----------------: | :-------: | :----: | :---------: |
| アップグレード種別 | type      | byte   | integer     |
| アップグレード結果 | result    | byte   | integer     |

#### 位置情報報告 `"msg_id": 512` (0x0200)

| フィールド名       | JSON キー   | 値の型  | JSON 内の型 |
| :----------------: | :---------: | :-----: | :---------: |
| 警報フラグ         | alarm       | dword   | integer     |
| 状態               | status      | dword   | integer     |
| 緯度               | latitude    | dword   | integer     |
| 経度               | longitude   | dword   | integer     |
| 高度               | altitude    | word    | integer     |
| 速度               | speed       | word    | integer     |
| 方向               | direction   | word    | integer     |
| 時刻               | time        | bcd(6)  | string      |

| オプション項目         | JSON キー | 値の型 | JSON 内の型 |
| :--------------------: | :-------: | :----: | :---------: |
| 追加位置情報項目       | extra     | -      | map         |

##### JT/T 808-2019 の変更点

- **新規警報フラグ**:  
  - bit15（違法運転警報）  
  - bit16（タイヤ空気圧警告）  
  - bit17（右折死角異常警報）

- **変更された警報フラグ**:  
  - bit29（衝突警告 → 衝突・転覆警報）  
  - bit31（違法ドア開放警報 → 予約）

- **新規状態ビット**:  
  - bit6（緊急制動システムによる前方衝突警告）  
  - bit7（車線逸脱警告）  
  - bit22（車両走行状態）

- **新規追加情報項目**:  
  - 0x05（タイヤ空気圧）  
  - 0x06（車内温度）

<!-- TODO: 警報の詳細をさらに精査 -->

- 位置情報追加情報項目は `extra` に含まれます。

| 追加情報項目名                          | JSON キー        | 値の型 | JSON 内の型               |
| :------------------------------------: | :--------------: | :----: | :-----------------------: |
| 走行距離                              | mileage          | dword  | integer                   |
| 燃料計                              | fuel_meter       | word   | integer                   |
| 運行記録機能による速度                | speed            | word   | integer                   |
| 手動確認が必要な警報イベントID        | alarm_id         | word   | integer                   |
| タイヤ空気圧（2019年追加）             | tire_pressure    | -      | string (base64エンコード) |
| 車内温度（2019年追加）                 | carriage_temp    | word   | integer                   |
| 超速警報追加情報（長さ1または5）       | overspeed_alarm  | -      | map                       |
| 出入区域・ルート警報追加情報           | in_out_alarm     | -      | map                       |
| ルート走行時間異常警報追加情報         | path_time_alarm  | -      | map                       |
| 拡張車両信号状態ビット                  | See Status Bit Table | -  | -                         |
| IO状態ビット                          | io_status        | -      | map                       |
| アナログ                             | analog           | -      | map                       |
| 無線通信ネットワーク信号強度           | rssi             | byte   | integer                   |
| GNSS衛星数                          | gnss_sat_num     | byte   | integer                   |
| 続くカスタム情報の長さ                 | custome          | -      | string(base64エンコード)  |

- 超速警報追加情報（長さ1または5）は `overspeed_alarm` マップ内にあります。

| フィールド名     | JSON キー | 値の型 | JSON 内の型 |
| :--------------: | :-------: | :----: | :---------: |
| 位置情報タイプ   | type      | byte   | integer     |

| オプション項目   | JSON キー | 値の型 | JSON 内の型 |
| :--------------: | :-------: | :----: | :---------: |
| 区域または区間ID | id        | dword  | integer     |

- 出入区域・ルート警報追加情報は `in_out_alarm` マップ内にあります。

| フィールド名     | JSON キー | 値の型 | JSON 内の型 |
| :--------------: | :-------: | :----: | :---------: |
| 位置情報タイプ   | type      | byte   | integer     |
| 区域または区間ID | id        | dword  | integer     |
| 方向             | direction | byte   | integer     |

- ルート走行時間異常警報追加情報は `path_time_alarm` マップ内にあります。

| フィールド名     | JSON キー | 値の型 | JSON 内の型 |
| :--------------: | :-------: | :----: | :---------: |
| 区間ID           | id        | dword  | integer     |
| ルート走行時間   | time      | word   | integer     |
| 結果             | result    | byte   | integer     |

- IO状態ビットは `io_status` マップ内にあります。

| フィールド名       | JSON キー   | 値の型 | JSON 内の型 |
| :----------------: | :---------: | :----: | :---------: |
| ディープスリープ状態 | deep_sleep  | 1 bit  | integer     |
| スリープ状態       | sleep       | 1 bit  | integer     |

- アナログは `analog` マップ内にあります。

| フィールド名 | JSON キー | 値の型   | JSON 内の型 |
| :----------: | :-------: | :------: | :---------: |
| アナログ0    | ad0       | 16ビット | integer     |
| アナログ1    | ad1       | 16ビット | integer     |

- 拡張車両信号状態ビットは `extra` マップ内にあります。

| フィールド名         | JSON キー       | 値の型    | JSON 内の型                                   |
| :------------------: | :-------------: | :-------: | :-------------------------------------------: |
| 信号                 | signal          | 2ビット   | map, `{"low_beam": VAL, "high_beam": VAL}`   |
| 右折信号             | right_turn      | 1ビット   | integer                                      |
| 左折信号             | left_turn       | 1ビット   | integer                                      |
| ブレーキ信号         | brake           | 1ビット   | integer                                      |
| リバース信号         | reverse         | 1ビット   | integer                                      |
| フォグライト信号     | fog             | 1ビット   | integer                                      |
| サイドマーカー       | side_marker     | 1ビット   | integer                                      |
| ホーン状態           | horn            | 1ビット   | integer                                      |
| エアコン状態         | air_conditioner | 1ビット   | integer                                      |
| ニュートラル信号     | neutral         | 1ビット   | integer                                      |
| リターダ作動         | retarder        | 1ビット   | integer                                      |
| ABS作動              | abs             | 1ビット   | integer                                      |
| ヒーター作動         | heater          | 1ビット   | integer                                      |
| クラッチ状態         | cluth           | 1ビット   | integer                                      |

- 信号状態は `signal` マップ内にあります。

| フィールド名       | JSON キー  | 値の型 | JSON 内の型 |
| :----------------: | :--------: | :----: | :---------: |
| ロービーム信号     | low_beam   | 1ビット | integer     |
| ハイビーム信号     | high_beam  | 1ビット | integer     |

例：

```json
{
  "header": {
    "msg_id": 1,
    "encrypt": 0,
    "len": VAL,
    "phone": 13900000000,
    "msg_sn": 0
  },
  "body": {
    "alarm": VAL,
    "status": VAL,
    "latitude": VAL,
    "longitude": VAL,
    "altitude": VAL,
    "speed": VAL,
    "direction": VAL,
    "time": VAL,
    "extra": {
      "mileage": VAL,
      "fuel_unit": VAL,
      "speed": VAL,
      "alarm_id": VAL,
      "overspeed_alarm": {
        "type": VAL,
        "id": VAL
      },
      "in_out_alarm": {
        "type": VAL,
        "id": VAL,
        "direction": VAL
      },
      "path_time_alarm": {
        "id": VAL,
        "time": VAL,
        "result": VAL
      },
      "signal": {
        "low_beam": VAL,
        "high_beam": VAL
      },
      "right_turn": VAL,
      "left_turn": VAL,
      "brake": VAL,
      "reverse": VAL,
      "fog": VAL,
      "side_marker": VAL,
      "horn": VAL,
      "air_conditioner": VAL,
      "neutral": VAL,
      "retarder": VAL,
      "abs": VAL,
      "heater": VAL,
      "cluth": VAL,
      "io_status": {
        "deep_sleep": VAL,
        "sleep": VAL
      },
      "analog": {
        "ad0": VAL,
        "ad1": VAL
      }
    }
  }
}
```

#### 位置情報照会 `"msg_id": 33281` (0x8201)

空の JSON

#### 位置情報照会応答 `"msg_id": 513` (0x0201)

| フィールド名       | JSON キー | 値の型 | JSON 内の型 |
| :----------------: | :-------: | :----: | :---------: |
| 応答シーケンス     | seq       | word   | integer     |
| 位置情報報告       | params    | -      | map         |

#### 一時位置追跡制御 `"msg_id": 33282` (0x8202)

| フィールド名       | JSON キー | 値の型 | JSON 内の型 |
| :----------------: | :-------: | :----: | :---------: |
| 間隔               | period    | word   | integer     |
| 追跡期間           | expiry    | dword  | integer     |

#### 手動警報確認メッセージ `"msg_id": 33283` (0x8203)

| フィールド名       | JSON キー | 値の型 | JSON 内の型 |
| :----------------: | :-------: | :----: | :---------: |
| 警報メッセージシーケンス | seq       | word   | integer     |
| 手動警報種別       | type      | dword  | integer     |

#### サーバー発信端末リンク検測要求 `"msg_id": 33284` (0x8204、2019年追加)

空の JSON

#### テキストメッセージ配信 `"msg_id": 33536` (0x8300)

##### JT/T 808-2013

| フィールド名   | JSON キー | 値の型 | JSON 内の型 |
| :------------: | :-------: | :----: | :---------: |
| フラグ         | flag      | byte   | integer     |
| テキスト情報   | text      | string | string      |

##### JT/T 808-2019

| フィールド名   | JSON キー | 値の型 | JSON 内の型 |
| :------------: | :-------: | :----: | :---------: |
| フラグ         | flag      | byte   | integer     |
| テキスト種別   | text_type | byte   | integer     |
| テキスト情報   | text      | string | string      |

**備考**：2019年版では新たにテキスト種別フィールドが追加され、フラグのbit0–1の意味が変更されています。

#### イベント設定 `"msg_id": 33537` (0x8301)

| フィールド名       | JSON キー | 値の型 | JSON 内の型                                  |
| :----------------: | :-------: | :----: | :------------------------------------------: |
| 設定種別           | type      | byte   | integer                                     |
| イベント総数       | length    | byte   | integer                                     |
| イベントリスト     | events    | list   | イベントのリスト `[{"id": ID, "length": LEN, "content": CON}, ...]` |
| イベントID         | id        | byte   | integer                                     |
| イベント長さ       | length    | byte   | integer                                     |
| イベント内容       | content   | string | string                                      |

#### イベント報告 `"msg_id": 769` (0x0301)

| フィールド名 | JSON キー | 値の型 | JSON 内の型 |
| :----------: | :-------: | :----: | :---------: |
| イベントID   | id        | byte   | integer     |

#### 質問配信 `"msg_id": 33538` (0x8302)

| フィールド名           | JSON キー | 値の型 | JSON 内の型                                  |
| :--------------------: | :-------: | :----: | :------------------------------------------: |
| フラグ                 | flag      | byte   | integer                                     |
| 質問長さ               | length    | byte   | integer                                     |
| 質問内容               | question  | string | string                                      |
| 回答候補リスト         | answers   | list   | 回答のリスト `[{"id": ID, "len": LEN, "answer": ANS}, ...]` |
| 回答ID                 | id        | byte   | integer                                     |
| 回答内容長さ           | len       | byte   | integer                                     |
| 回答内容               | answer    | string | string                                      |

#### 質問応答 `"msg_id": 770` (0x0302)

| フィールド名     | JSON キー | 値の型 | JSON 内の型 |
| :--------------: | :-------: | :----: | :---------: |
| 応答シーケンス   | seq       | word   | integer     |
| 回答ID           | id        | byte   | integer     |

#### 情報サービスメニュー設定 `"msg_id": 33539` (0x8303)

| フィールド名       | JSON キー | 値の型 | JSON 内の型 |
| :----------------: | :-------: | :----: | :---------: |
| 設定種別           | type      | byte   | integer     |
| 情報項目総数       | length    | byte   | integer     |
| 情報項目リスト     | menus     | list   | メニューのリスト |
| 情報種別           | type      | byte   | integer     |
| 情報名長さ         | length    | word   | integer     |
| 情報名             | info      | string | string      |

#### 情報サービス／キャンセル `"msg_id": 771` (0x0303)

| フィールド名       | JSON キー | 値の型 | JSON 内の型 |
| :----------------: | :-------: | :----: | :---------: |
| 情報種別           | id        | byte   | integer     |
| ダイヤル／キャンセルフラグ | flag      | byte   | integer     |

#### 情報サービス `"msg_id": 33540` (0x8304)

| フィールド名   | JSON キー | 値の型 | JSON 内の型 |
| :------------: | :-------: | :----: | :---------: |
| 情報種別       | type      | byte   | integer     |
| 情報長さ       | length    | word   | integer     |
| 情報内容       | info      | string | string      |

#### コールバック電話 `"msg_id": 33792` (0x8400)

| フィールド名   | JSON キー | 値の型 | JSON 内の型 |
| :------------: | :-------: | :----: | :---------: |
| フラグ         | type      | byte   | integer     |
| 電話番号       | phone     | string | string      |

#### 電話帳設定 `"msg_id": 33793` (0x8401)

| フィールド名       | JSON キー | 値の型 | JSON 内の型 |
| :----------------: | :-------: | :----: | :---------: |
| 設定種別           | type      | byte   | integer     |
| 連絡先総数         | length    | byte   | integer     |
| 連絡先項目リスト   | contacts  | list   | 連絡先のリスト |
| フラグ             | type      | byte   | integer     |
| 電話番号長さ       | phone_len | byte   | integer     |
| 電話番号           | phone     | string | string      |
| 連絡先名長さ       | name_len  | byte   | integer     |
| 連絡先名           | name      | string | string      |

連絡先項目例：

```json
[{"type": TYPE, "phone_len": PH_LEN, "phone": PHONE, "name_len": NAME_LEN, "name": NAME}, ...]
```

#### 車両制御 `"msg_id": 34048` (0x8500)

##### JT/T 808-2013

| フィールド名   | JSON キー | 値の型 | JSON 内の型 |
| :------------: | :-------: | :----: | :---------: |
| 制御フラグ     | flag      | byte   | integer     |

##### JT/T 808-2019

| フィールド名           | JSON キー | 値の型 | JSON 内の型           |
| :--------------------: | :-------: | :----: | :-------------------: |
| 制御フラグ             | flag      | word   | integer               |
| 制御種別数             | length    | byte   | integer               |
| 制御種別リスト         | controls  | list   | 制御のリスト           |
| 制御種別ID             | type_id   | byte   | integer               |
| 制御種別パラメータ     | param     | binary | string(base64エンコード) |

**備考**：2019年版ではメッセージ構造が大幅に変更され、制御種別数と制御種別リストで構成されます。

#### 車両制御応答 `"msg_id": 1280` (0x0500)

| フィールド名           | JSON キー | 値の型 | JSON 内の型     |
| :--------------------: | :-------: | :----: | :-------------: |
| 応答シリアル           | seq       | word   | integer         |
| 位置情報報告本文       | location  | map    | 位置情報のマップ |

#### 円形区域設定 `"msg_id": 34304` (0x8600)

##### JT/T 808-2013

| フィールド名           | JSON キー         | 値の型 | JSON 内の型 |
| :--------------------: | :---------------: | :----: | :---------: |
| 設定属性               | type              | byte   | integer     |
| 区域総数               | length            | byte   | integer     |
| 区域項目リスト         | areas             | list   | 区域のリスト |
| 区域ID                 | id                | dword  | integer     |
| 区域属性               | flag              | word   | integer     |
| 中心緯度               | center_latitude   | dword  | integer     |
| 中心経度               | center_longitude  | dword  | integer     |
| 半径                   | radius            | dword  | integer     |
| 開始時刻               | start_time        | string | string      |
| 終了時刻               | end_time          | string | string      |
| 最大速度               | max_speed         | word   | integer     |
| 超速継続時間           | overspeed_duration | byte   | integer     |

##### JT/T 808-2019 追加項目

| フィールド名           | JSON キー       | 値の型 | JSON 内の型 |
| :--------------------: | :-------------: | :----: | :---------: |
| 夜間最大速度           | night_max_speed | word   | integer     |
| 区域名長さ             | name_len       | word   | integer     |
| 区域名                 | name           | string | string      |

**備考**：2019年版では区域項目に夜間最大速度、区域名長さ、区域名のフィールドが追加されています。

区域リスト例：

```json
[{"id": ID,
   "flag": FLAG,
   "center_latitude": CEN_LAT,
   "center_longitude": CEN_LON,
   "radius": RADIUS,
   "start_time": START_TIME,
   "end_time": END_TIME,
   "max_speed": MAX_SPEED,
   "overspeed_duration": OVERSPEED_DURATION,
   "night_max_speed": NIGHT_MAX_SPEED,
   "name_len": NAME_LEN,
   "name": NAME
   },
  ...
 ]
```

#### 円形区域削除 `"msg_id": 34305` (0x8601)

| フィールド名       | JSON キー | 値の型 | JSON 内の型    |
| :----------------: | :-------: | :----: | :------------: |
| 区域総数           | length    | byte   | integer        |
| 区域IDリスト       | ids       | list   | IDのリスト     |
| 区域ID 1～n        | -         | dword  | integer        |

```
[ID1, ID2, ...]
```

#### 矩形区域設定 `"msg_id": 34306` (0x8602)

##### JT/T 808-2013

| フィールド名           | JSON キー         | 値の型 | JSON 内の型       |
| :--------------------: | :---------------: | :----: | :---------------: |
| 設定属性               | type              | byte   | integer           |
| 区域総数               | length            | byte   | integer           |
| 区域項目リスト         | areas             | list   | 矩形区域のリスト   |
| 区域ID                 | id                | dword  | integer           |
| 区域属性               | flag              | word   | integer           |
| 左上緯度               | lt_lat            | dword  | integer           |
| 左上経度               | lt_lng            | dword  | integer           |
| 右下緯度               | rb_lat            | dword  | integer           |
| 右下経度               | rb_lng            | dword  | integer           |
| 開始時刻               | start_time        | string | string            |
| 終了時刻               | end_time          | string | string            |
| 最大速度               | max_speed         | word   | integer           |
| 超速継続時間           | overspeed_duration | byte   | integer           |

##### JT/T 808-2019 追加項目

| フィールド名           | JSON キー       | 値の型 | JSON 内の型 |
| :--------------------: | :-------------: | :----: | :---------: |
| 夜間最大速度           | night_max_speed | word   | integer     |
| 区域名長さ             | name_len       | word   | integer     |
| 区域名                 | name           | string | string      |

**備考**：2019年版では区域項目に夜間最大速度、区域名長さ、区域名のフィールドが追加されています。

#### 矩形区域削除 `"msg_id": 34307` (0x8603)

| フィールド名       | JSON キー | 値の型 | JSON 内の型    |
| :----------------: | :-------: | :----: | :------------: |
| 区域総数           | length    | byte   | integer        |
| 区域IDリスト       | ids       | list   | IDのリスト     |
| 区域ID 1～n        | -         | dword  | integer        |

#### 多角形区域設定 `"msg_id": 34308` (0x8604)

##### JT/T 808-2013

| フィールド名           | JSON キー         | 値の型 | JSON 内の型 |
| :--------------------: | :---------------: | :----: | :---------: |
| 区域ID                 | id                | dword  | integer     |
| 区域属性               | flag              | dword  | integer     |
| 開始時刻               | start_time        | string | string      |
| 終了時刻               | end_time          | string | string      |
| 最大速度               | max_speed         | word   | integer     |
| 超速継続時間           | overspeed_duration | byte   | integer     |
| 頂点総数               | length            | word   | integer     |
| 頂点リスト             | points            | list   | 頂点のリスト |
| 頂点緯度               | lat               | dword  | integer     |
| 頂点経度               | lng               | dword  | integer     |

##### JT/T 808-2019 追加項目

| フィールド名           | JSON キー       | 値の型 | JSON 内の型 |
| :--------------------: | :-------------: | :----: | :---------: |
| 夜間最大速度           | night_max_speed | word   | integer     |
| 区域名長さ             | name_len       | word   | integer     |
| 区域名                 | name           | string | string      |

**備考**：2019年版では夜間最大速度、区域名長さ、区域名のフィールドが追加されています。

#### 多角形区域削除 `"msg_id": 34309` (0x8605)

| フィールド名       | JSON キー | 値の型 | JSON 内の型    |
| :----------------: | :-------: | :----: | :------------: |
| 区域総数           | length    | byte   | integer        |
| 区域IDリスト       | ids       | list   | IDのリスト     |
| 区域ID 1～n        | -         | dword  | integer        |

#### ルート設定 `"msg_id": 34310` (0x8606)

##### JT/T 808-2013

| フィールド名               | JSON キー         | 値の型 | JSON 内の型 |
| :------------------------: | :---------------: | :----: | :---------: |
| ルートID                  | id                | dword  | integer     |
| ルート属性                | flag              | word   | integer     |
| 開始時刻                  | start_time        | string | string      |
| 終了時刻                  | end_time          | string | string      |
| 曲がり角総数              | length            | word   | integer     |
| 曲がり角リスト            | points            | list   | 頂点のリスト |
| 曲がり角ID                | point_id          | dword  | integer     |
| 区間ID                    | path_id           | dword  | integer     |
| 曲がり角緯度              | point_lat         | dword  | integer     |
| 曲がり角経度              | point_lng         | dword  | integer     |
| 区間幅                    | width             | byte   | integer     |
| 区間属性                  | attrib            | byte   | integer     |
| 区間走行過長閾値          | passed            | word   | integer     |
| 区間走行不足閾値          | uncovered         | word   | integer     |
| 区間最大速度              | max_speed         | word   | integer     |
| 区間超速継続時間          | overspeed_duration | byte   | integer     |

##### JT/T 808-2019 追加項目

| フィールド名                   | JSON キー       | 値の型 | JSON 内の型 |
| :----------------------------: | :-------------: | :----: | :---------: |
| 区間夜間最大速度               | night_max_speed | word   | integer     |
| ルート名長さ                 | name_len       | word   | integer     |
| ルート名                     | name           | string | string      |

**備考**：2019年版では区間夜間最大速度、ルート名長さ、ルート名のフィールドが追加されています。

#### ルート削除 `"msg_id": 34311` (0x8607)

| フィールド名       | JSON キー | 値の型 | JSON 内の型    |
| :----------------: | :-------: | :----: | :------------: |
| ルート総数         | length    | byte   | integer        |
| ルートIDリスト     | ids       | list   | IDのリスト     |
| ルートID           | -         | dword  | integer        |

#### 区域・ルートデータ照会 `"msg_id": 34312` (0x8608、2019年追加)

| フィールド名       | JSON キー | 値の型          | JSON 内の型       |
| :----------------: | :-------: | :-------------: | :---------------: |
| 照会種別           | type      | byte            | integer           |
| 照会件数           | length    | dword           | integer           |
| 区域・ルートIDリスト | ids       | byte(4*length)  | 整数のリスト      |

照会種別値：  
- 1 – 円形区域  
- 2 – 矩形区域  
- 3 – 多角形区域  
- 4 – ルート

#### 区域・ルートデータ照会応答 `"msg_id": 1544` (0x0608、2019年追加)

| フィールド名       | JSON キー | 値の型 | JSON 内の型            |
| :----------------: | :-------: | :----: | :--------------------: |
| 照会種別           | type      | byte   | integer                |
| 区域・ルート件数   | length    | dword  | integer                |
| 区域・ルート項目リスト | items     | binary | string(base64エンコード) |

区域・ルート項目リストの内容は照会種別により異なります。詳細はプロトコル仕様書を参照してください。

#### 運行記録データ収集コマンド `"msg_id": 34560` (0x8700)

| フィールド名   | JSON キー | 値の型               | JSON 内の型 |
| :------------: | :-------: | :------------------: | :---------: |
| コマンド       | command   | byte                 | integer     |
| データブロック | param     | string(base64エンコード) | string      |

#### 運行記録データアップロード `"msg_id": 1792` (0x0700)

| フィールド名           | JSON キー | 値の型               | JSON 内の型 |
| :--------------------: | :-------: | :------------------: | :---------: |
| 応答シリアル番号       | seq       | word                 | integer     |
| コマンド               | command   | byte                 | integer     |
| データブロック         | data      | string(base64エンコード) | string      |

#### 運行記録パラメータ下行コマンド `"msg_id": 34561` (0x8701)

| フィールド名   | JSON キー | 値の型               | JSON 内の型 |
| :------------: | :-------: | :------------------: | :---------: |
| コマンド       | command   | byte                 | integer     |
| データブロック | param     | string(base64エンコード) | string      |

#### 電子運単報告 `"msg_id": 1793` (0x0701)

| フィールド名           | JSON キー | 値の型               | JSON 内の型 |
| :--------------------: | :-------: | :------------------: | :---------: |
| 電子運単長さ           | length    | dword                | integer     |
| 電子運単内容           | data      | string(base64エンコード) | string      |

#### 運転者身分情報アップロード要求 `"msg_id": 34562` (0x8702)

空の JSON

#### 運転者身分情報収集報告 `"msg_id": 1794` (0x0702)

##### JT/T 808-2013

| フィールド名             | JSON キー     | 値の型 | JSON 内の型 |
| :----------------------: | :-----------: | :----: | :---------: |
| 状態                     | status        | byte   | integer     |
| 時刻                     | time          | string | string      |
| ICカード読取結果         | ic_result     | byte   | integer     |
| 運転者名                 | driver_name   | string | string      |
| 職業資格証コード         | certificate   | string | string      |
| 発行機関名               | organization  | string | string      |
| 証明書有効期限           | cert_expiry   | string | string      |

##### JT/T 808-2019 追加項目

| フィールド名           | JSON キー | 値の型 | JSON 内の型 |
| :--------------------: | :-------: | :----: | :---------: |
| 運転者IDカード番号     | id_card   | string | string      |

**備考**：2019年版では運転者IDカード番号フィールドが追加されています。

#### 位置データ一括アップロード `"msg_id": 1796` (0x0704)

| フィールド名           | JSON キー | 値の型 | JSON 内の型 |
| :--------------------: | :-------: | :----: | :---------: |
| 位置データ種別         | type      | byte   | integer     |
| データ件数             | length    | word   | integer     |
| 位置報告データ項目     | location  | list   | 位置情報のリスト |

#### CANバスデータアップロード `"msg_id": 1797` (0x0705)

| フィールド名               | JSON キー  | 値の型          | JSON 内の型               |
| :------------------------: | :--------: | :-------------: | :-----------------------: |
| データ件数                 | length     | word            | integer                   |
| CANバスデータ受信時刻     | time       | bcd(5)          | integer                   |
| CANバスデータ項目         | can_data   | list            | CANデータのリスト         |
| CANバスチャネル番号       | channel    | 1ビット         | integer                   |
| フレームタイプ             | frame_type | 1ビット         | integer                   |
| データ収集方法             | data_method| 1ビット         | integer                   |
| CANバスID                  | id         | 29ビット        | integer                   |
| CANデータ                  | data       | binary          | string(base64エンコード)  |

#### マルチメディアイベント情報アップロード `"msg_id": 2048` (0x0800)

| フィールド名           | JSON キー | 値の型 | JSON 内の型 |
| :--------------------: | :-------: | :----: | :---------: |
| マルチメディアデータID | id        | dword  | integer     |
| マルチメディア種別     | type      | byte   | integer     |
| マルチメディア符号化形式 | format    | byte   | integer     |
| イベント項目コード     | event     | byte   | integer     |
| チャネルID             | channel   | byte   | integer     |

#### マルチメディアデータアップロード `"msg_id": 2049` (0x0801)

| フィールド名           | JSON キー  | 値の型 | JSON 内の型               |
| :--------------------: | :--------: | :----: | :-----------------------: |
| マルチメディアID       | id         | dword  | integer                   |
| マルチメディア種別     | type       | byte   | integer                   |
| マルチメディア符号化形式 | format     | byte   | integer                   |
| イベント項目コード     | event      | byte   | integer                   |
| チャネルID             | channel    | byte   | integer                   |
| 位置情報報告           | location   | byte(28)| map                      |
| マルチメディアデータパッケージ | multimedia | binary | string(base64エンコード)  |

#### マルチメディアデータアップロード応答 `"msg_id": 34816` (0x8800)

| フィールド名               | JSON キー  | 値の型 | JSON 内の型         |
| :------------------------: | :--------: | :----: | :-----------------: |
| マルチメディアID           | mm_id      | dword  | integer             |
| 再送パッケージ総数         | length     | byte   | integer             |
| 再送パッケージIDリスト     | retx_ids   | list   | 再送IDのリスト      |

#### 即時カメラ撮影コマンド `"msg_id": 34817` (0x8801)

| フィールド名           | JSON キー   | 値の型 | JSON 内の型 |
| :--------------------: | :---------: | :----: | :---------: |
| チャネルID             | channel_id  | byte   | integer     |
| 撮影コマンド           | command    | word   | integer     |
| 間隔／録画時間         | period     | word   | integer     |
| 保存フラグ             | save       | byte   | integer     |
| 解像度                 | resolution | byte   | integer     |
| 画像／動画品質         | quality    | byte   | integer     |
| 明るさ                 | bright    | byte   | integer     |
| コントラスト           | contrast   | byte   | integer     |
| 彩度                   | saturate   | byte   | integer     |
| 色相                   | chromaticity | byte | integer     |

**備考**：2019年版では解像度フィールドに新たに `0x00`（最低品質圧縮）および `0xFF`（最高品質圧縮）が追加されています。

#### 即時カメラ撮影応答 `"msg_id": 2053` (0x0805)

| フィールド名           | JSON キー | 値の型    | JSON 内の型 |
| :--------------------: | :-------: | :-------: | :---------: |
| 応答シリアル番号       | seq       | word      | integer     |
| 結果                   | result    | byte      | integer     |
| マルチメディアID総数   | length    | word      | integer     |
| マルチメディアIDリスト | ids       | byte(4*length) | integer |

#### 記憶マルチメディアデータ検索 `"msg_id": 34818` (0x8802)

| フィールド名           | JSON キー | 値の型 | JSON 内の型 |
| :--------------------: | :-------: | :----: | :---------: |
| マルチメディア種別     |           | byte   |             |
| チャネルID             |           | byte   |             |
| イベント項目コード     |           | byte   |             |
| 開始時刻               |           | string |             |
| 終了時刻               |           | string |             |

#### 記憶マルチメディアデータ検索応答 `"msg_id": 2050` (0x0802)

##### JT/T 808-2013

| フィールド名               | JSON キー | 値の型 | JSON 内の型           |
| :------------------------: | :-------: | :----: | :-------------------: |
| 応答シリアル番号           | seq       | word   | integer               |
| マルチメディアデータ総数   | length    | word   | integer               |
| 検索結果項目               | result    | list   | 検索結果のリスト       |
| マルチメディアID           | id        | dword  | integer               |
| マルチメディア種別         | type      | byte   | integer               |
| チャネルID                 | channel   | byte   | integer               |
| イベント項目コード         | event     | byte   | integer               |
| 位置情報報告               | location  | byte(28)| map                   |

##### JT/T 808-2019

| フィールド名               | JSON キー | 値の型 | JSON 内の型           |
| :------------------------: | :-------: | :----: | :-------------------: |
| 応答シリアル番号           | seq       | word   | integer               |
| マルチメディアデータ総数   | length    | word   | integer               |
| 検索結果項目               | result    | list   | 検索結果のリスト       |
| マルチメディアID           | id        | dword  | integer               |
| マルチメディア種別         | type      | byte   | integer               |
| チャネルID                 | channel   | byte   | integer               |
| イベント項目コード         | event     | byte   | integer               |
| 位置情報報告               | location  | variable | map                   |

**備考**：2019年版では位置情報報告のメッセージ本文が固定長28バイトから可変長に変更されています。

#### 記憶マルチメディアデータアップロードコマンド `"msg_id": 34819` (0x8803)

| フィールド名           | JSON キー   | 値の型 | JSON 内の型 |
| :--------------------: | :---------: | :----: | :---------: |
| マルチメディア種別     | type        | byte   | integer     |
| チャネルID             | channel     | byte   | integer     |
| イベント項目コード     | event       | byte   | integer     |
| 開始時刻               | start_time  | string | string      |
| 終了時刻               | end_time    | string | string      |
| 削除フラグ             | delete      | byte   | integer     |

#### 音声録音開始コマンド `"msg_id": 34820` (0x8804)

| フィールド名           | JSON キー | 値の型 | JSON 内の型 |
| :--------------------: | :-------: | :----: | :---------: |
| 録音コマンド           | command   | byte   | integer     |
| 録音時間               | time      | word   | integer     |
| 保存フラグ             | save      | byte   | integer     |
| 音声サンプリングレート | rate      | byte   | integer     |

#### 単一記憶マルチメディア項目取得アップロードコマンド `"msg_id": 34821` (0x8805)

| フィールド名           | JSON キー | 値の型 | JSON 内の型 |
| :--------------------: | :-------: | :----: | :---------: |
| マルチメディアID       | id        | dword  | integer     |
| 削除フラグ             | flag      | byte   | integer     |

#### 下行データ送信 `"msg_id": 35072` (0x8900)

| フィールド名           | JSON キー | 値の型 | JSON 内の型               |
| :--------------------: | :-------: | :----: | :-----------------------: |
| 送信メッセージ種別     | type      | byte   | integer                   |
| 送信メッセージ内容     | data      | binary | string(base64エンコード)  |

#### 上行データ送信 `"msg_id": 2304` (0x0900)

| フィールド名           | JSON キー | 値の型 | JSON 内の型               |
| :--------------------: | :-------: | :----: | :-----------------------: |
| 送信メッセージ種別     | type      | byte   | integer                   |
| 送信メッセージ内容     | data      | binary | string(base64エンコード)  |

#### データ圧縮報告 `"msg_id": 2305` (0x0901)

| フィールド名           | JSON キー | 値の型 | JSON 内の型               |
| :--------------------: | :-------: | :----: | :-----------------------: |
| 圧縮メッセージ長       | length    | dword  | integer                   |
| 圧縮メッセージ本文     | data      | binary | string(base64エンコード)  |

#### プラットフォームRSA公開鍵 `"msg_id": 35328` (0x8A00)

| フィールド名 | JSON キー | 値の型    | JSON 内の型               |
| :----------: | :-------: | :-------: | :-----------------------: |
| e            | e         | dword     | integer                   |
| n            | n         | byte(128) | string(base64エンコード)  |

#### 端末RSA公開鍵 `"msg_id": 2560` (0x0A00)

| フィールド名 | JSON キー | 値の型    | JSON 内の型               |
| :----------: | :-------: | :-------: | :-----------------------: |
| e            | e         | dword     | integer                   |
| n            | n         | byte(128) | string(base64エンコード)  |

#### 予約領域 0x8F00 ～ 0x8FFF

#### 予約領域 0x0F00 ～ 0x0FFF

#### ベンダー定義上行メッセージ 0xE000 ～ 0xEFFF (2019年追加)

ベンダー定義メッセージ。メッセージ本文フォーマットはベンダーが定義します。

#### ベンダー定義下行メッセージ 0xF000 ～ 0xFFFF (2019年追加)

ベンダー定義メッセージ。メッセージ本文フォーマットはベンダーが定義します。
