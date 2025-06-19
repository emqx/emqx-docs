# JT/T 808 ゲートウェイデータ交換フォーマット

このページでは、**emqx_jt808** と **EMQX** 間のデータ交換フォーマットを定義します。

規約:

- ペイロードは JSON 形式で組み立てられます。
- JSON のキー名はすべて小文字です。

## JSON 構造例

### 端末からサーバーへ

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

### サーバーから端末へ

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

| JT808 定義型 | JSON 型 | コメント |
| :----------: | :-----: | :-------: |
|     BYTE     | integer | 10進数   |
|     WORD     | integer | 10進数   |
|    DWORD     | integer | 10進数   |
|    BYTE(n)   | string  |          |
|    BCD(n)    | string  |          |
|    STRING    | string  |          |

## フィールド対応表

### メッセージヘッダーのフィールド

|       フィールド        | JSON キー名 | 値の型 | JSON 内の型 |
| :---------------------: | :---------: | :----: | :---------: |
|      メッセージID       |   msg_id    |  word  |  integer    |
|    暗号化方式           |   encrypt   |  word  |  integer    |
| 端末電話番号 (BCD6桁)   |   phone    |  bcd(6) |   string    |
| メッセージシリアル番号  |   msg_sn    |  word  |  integer    |

|     オプションフィールド     | JSON キー名 | 値の型 | JSON 内の型 |
| :--------------------------: | :---------: | :----: | :---------: |
| メッセージ総数（分割時）     | frag_total  |  word  |  integer    |
| メッセージ分割シリアル番号   |  frag_sn    |  word  |  integer    |

- `frag_total` と `frag_sn` が存在する場合、メッセージ本文が長く複数パッケージに分割されていることを示します。

### メッセージ本文のフィールド

#### 端末からの一般応答 `"msg_id": 1` 0x0001

|        フィールド         | JSON キー名 | 値の型 | JSON 内の型 |
| :----------------------: | :---------: | :----: | :---------: |
| 応答シリアル番号          |     seq     |  word  |  integer    |
| 応答ID                    |     id      |  word  |  integer    |
| 結果                      |   result    |  byte  |  integer    |

#### プラットフォームからの一般応答 `"msg_id": 32769` 0x8001

|        フィールド         | JSON キー名 | 値の型 | JSON 内の型 |
| :----------------------: | :---------: | :----: | :---------: |
| 応答シリアル番号          |     seq     |  word  |  integer    |
| 応答ID                    |     id      |  word  |  integer    |
| 結果                      |   result    |  byte  |  integer    |

#### 端末ハートビート `"msg_id": 2` 0x0002

空の JSON

#### サブパッケージ再送要求 `"msg_id": 32771` 0x8003

|               フィールド                | JSON キー名 |    値の型     | JSON 内の型     |
| :-----------------------------------: | :---------: | :-----------: | :-------------: |
| 元のメッセージシリアル番号             |     seq     |     word      |    integer      |
| 再送パッケージ総数                     |    length   |     byte      |    integer      |
| 再送パッケージIDリスト                 |     ids     | byte(2*length) | 整数のリスト    |

#### 端末登録 `"msg_id": 256` 0x0100

|         フィールド         | JSON キー名 | 値の型  | JSON 内の型 |
| :-----------------------: | :---------: | :-----: | :---------: |
| 省ID                      |  province   |  word   |  integer    |
| 市ID                      |    city     |  word   |  integer    |
| 製造商ID                  | manufacturer | byte(5) |  string     |
| 端末モデル                |    model    | byte(20) |  string     |
| 端末ID                    |   dev_id    | byte(7) |  string     |
| 車両色                    |    color    |  byte   |  integer    |
| 車両識別番号              | license_number | string |  string     |

#### 端末登録応答 `"msg_id": 33024` 0x8100

|         フィールド         | JSON キー名 | 値の型 | JSON 内の型 |
| :-----------------------: | :---------: | :----: | :---------: |
| 応答シリアル番号          |     seq     |  word  |  integer    |
| 結果                      |   result   |  byte  |  integer    |

- 登録成功後のみ存在するフィールド

|    オプションフィールド    | JSON キー名 | 値の型 | JSON 内の型 |
| :-----------------------: | :---------: | :----: | :---------: |
| 認証コード                |  auth_code  | string |  string     |

#### 端末ログアウト `"msg_id": 3` 0x0003

空の JSON

#### 端末認証 `"msg_id": 258` 0x0102

|        フィールド        | JSON キー名 | 値の型 | JSON 内の型 |
| :---------------------: | :---------: | :----: | :---------: |
| 認証コード              |    code     | string |  string     |

#### 端末パラメータ設定 `"msg_id": 33027` 0x8103

|           フィールド           | JSON キー名 | 値の型 |              JSON 内の型               |
| :----------------------------: | :---------: | :----: | :-----------------------------------: |
| パラメータ総数                |   length    |  byte  |              integer                  |
| パラメータ項目リスト          |   params    |  list  | id と値のリスト `[{"id":ID, "value": VAL}, ...]` |
| パラメータ項目                |     id      | dword  |              integer                  |
| パラメータ値                  |    value    |  byte  |              integer                  |

- プロトコルに基づくパラメータIDの説明あり

#### 端末パラメータ照会 `"msg_id": 33028` 0x8104

空の JSON

#### 特定端末パラメータ照会 `"msg_id": 33030` 0x8106

|           フィールド           | JSON キー名 |     値の型     |           JSON 内の型            |
| :----------------------------: | :---------: | :------------: | :-----------------------------: |
| パラメータ総数                |   length    |      byte      |            integer              |
| パラメータIDリスト            |     ids     | byte(2*length) | 整数のリスト `[1, 2, 3, 4, ...]` |

- パラメータIDリストの要素は整数

#### 端末応答パラメータ `"msg_id": 260` 0x0104

|             フィールド             | JSON キー名 | 値の型 |              JSON 内の型               |
| :-------------------------------: | :---------: | :----: | :-----------------------------------: |
| 応答シリアル番号                  |    seq      |  word  |              integer                  |
| 応答パラメータ数                  |   length    |  byte  |              integer                  |
| パラメータ項目リスト              |   params    |  list  | id と値のリスト `[{"id":ID, "value": VAL}, ...]` |
| パラメータ項目                    |     id      | dword  |              integer                  |
| パラメータ値                      |    value    |  byte  |              integer                  |

- プロトコルに基づくパラメータIDの説明あり

#### 端末制御 `"msg_id": 33029` 0x8105

|       フィールド       | JSON キー名 | 値の型 | JSON 内の型 |
| :-------------------: | :---------: | :----: | :---------: |
| コマンドワード        |  command    |  byte  |  integer    |
| コマンドパラメータ    |   param     | string |  string     |

#### 端末プロパティ照会 `"msg_id": 33031` 0x8107

空の JSON

#### 端末プロパティ応答 `"msg_id": 263` 0x0107

|              フィールド              | JSON キー名 | 値の型  | JSON 内の型 |
| :---------------------------------: | :---------: | :-----: | :---------: |
| 端末タイプ                        |    type     |  word   |  integer    |
| 製造商ID                         | manufacturer | byte(5) |  string     |
| 端末モデル                       |    model    | byte(20) |  string     |
| 端末ID                           |     id      | byte(7) |  string     |
| 端末SIMカードICCID               |   iccid     | byte(10) |  string     |
| 端末ハードウェアバージョン番号   | hardware_version | string |  string     |
| 端末ファームウェアバージョン番号 | firmware_version | string |  string     |
| GNSSモジュールプロパティ         |  gnss_prop  |  byte   |  integer    |
| 通信モジュールプロパティ         |  comm_prop  |  byte   |  integer    |

- 端末ハードウェアバージョン番号とファームウェア番号の長さはバイナリメッセージ解析に使用され、外部には公開されません。

#### 端末アップグレードパッケージコマンド `"msg_id": 33032` 0x8108

|         フィールド         | JSON キー名 | 値の型  |         JSON 内の型          |
| :-----------------------: | :---------: | :-----: | :--------------------------: |
| アップグレードタイプ       |    type     |  byte   |          integer             |
| 製造商ID                 | manufacturer | byte(5) |          string              |
| バージョン番号長さ       |   ver_len   |  byte   |          integer             |
| バージョン番号           |   version   | string  |          string              |
| アップグレードパッケージ長さ |   fw_len    | dword   |          integer             |
| アップグレードパッケージ   |  firmware  | binary  | string(base64 encoded)       |

#### 端末アップグレード結果通知 `"msg_id": 264` 0x0108

|       フィールド       | JSON キー名 | 値の型 | JSON 内の型 |
| :-------------------: | :---------: | :----: | :---------: |
| アップグレードタイプ   |    type     |  byte  |  integer    |
| アップグレード結果     |   result    |  byte  |  integer    |

#### 位置情報報告 `"msg_id": 512` 0x0200

|     フィールド     | JSON キー名 | 値の型 | JSON 内の型 |
| :----------------: | :---------: | :----: | :---------: |
| 警報フラグ          |   alarm     | dword  |  integer    |
| 状態                |   status    | dword  |  integer    |
| 緯度                |  latitude  | dword  |  integer    |
| 経度                |  longitude | dword  |  integer    |
| 高度                |  altitude  |  word  |  integer    |
| 速度                |   speed    |  word  |  integer    |
| 方向                | direction  |  word  |  integer    |
| 時刻                |    time    | bcd(6) |  string     |

|       オプションフィールド        | JSON キー名 | 値の型 | JSON 内の型 |
| :------------------------------: | :---------: | :----: | :---------: |
| 追加位置情報項目                |    extra    |   -    |    map      |

- 追加位置情報項目は `extra` に格納

|        フィールド（追加情報説明）        | JSON キー名 | 値の型 |        JSON 内の型         |
| :-------------------------------------: | :---------: | :----: | :-----------------------: |
| 走行距離                               |  mileage   | dword  |        integer            |
| 燃料計                                | fuel_meter |  word  |        integer            |
| 運転記録による速度                    |   speed   |  word  |        integer            |
| 手動確認が必要な警報イベントID       | alarm_id  |  word  |        integer            |
| 超速警報追加情報（長さ1または5）      | overspeed_alarm |   -    |          map              |
| 進入/退出エリア・ルート警報追加情報   | in_out_alarm |   -    |          map              |
| ルート走行時間短すぎ/長すぎ警報追加情報 | path_time_alarm |   -    |          map              |
| 拡張車両信号状態ビット                 | 状態ビット表参照 |   -    |           -               |
| IO状態ビット                         | io_status  |   -    |          map              |
| アナログ                             |  analog   |   -    |          map              |
| 無線通信ネットワーク信号強度          |   rssi    |  byte  |        integer            |
| GNSS衛星数                          | gnss_sat_num |  byte  |        integer            |
| 以降のカスタム情報の長さ             |  custome  |   -    | string(base64 encoded)    |

- 超速警報追加情報（長さ1または5）は `overspeed_alarm` マップ内

|       フィールド       | JSON キー名 | 値の型 | JSON 内の型 |
| :-------------------: | :---------: | :----: | :---------: |
| 位置情報タイプ         |    type     |  byte  |  integer    |

|     オプションフィールド     | JSON キー名 | 値の型 | JSON 内の型 |
| :------------------------: | :---------: | :----: | :---------: |
| エリアまたは区間ID        |     id      | dword  |  integer    |

- 進入/退出エリア・ルート警報追加情報は `in_out_alarm` マップ内

|       フィールド       | JSON キー名 | 値の型 | JSON 内の型 |
| :-------------------: | :---------: | :----: | :---------: |
| 位置情報タイプ         |    type     |  byte  |  integer    |
| エリアまたは区間ID     |     id      | dword  |  integer    |
| 方向                   | direction  |  byte  |  integer    |

- ルート走行時間短すぎ/長すぎ警報追加情報は `path_time_alarm` マップ内

|       フィールド       | JSON キー名 | 値の型 | JSON 内の型 |
| :-------------------: | :---------: | :----: | :---------: |
| 区間ID                 |     id      | dword  |  integer    |
| ルート走行時間         |    time     |  word  |  integer    |
| 結果                   |   result   |  byte  |  integer    |

- IO状態ビットは `io_status` マップ内

|       フィールド       | JSON キー名 | 値の型 | JSON 内の型 |
| :-------------------: | :---------: | :----: | :---------: |
| ディープスリープ状態   | deep_sleep | 1 bit  |  integer    |
| スリープ状態           |   sleep   | 1 bit  |  integer    |

- アナログは `analog` マップ内

|    フィールド    | JSON キー名 | 値の型 | JSON 内の型 |
| :--------------: | :---------: | :----: | :---------: |
| アナログ0        |    ad0      | 16 bit |  integer    |
| アナログ1        |    ad1      | 16 bit |  integer    |

- 拡張車両信号状態ビットは `extra` マップ内

|         フィールド         | JSON キー名 | 値の型  |           JSON 内の型            |
| :-----------------------: | :---------: | :-----: | :-----------------------------: |
| 信号（2ビット）            |   signal   | - 2 bits | map, `{"low_beam": VAL, "high_beam": VAL}` |
| 右ウインカー信号           | right_turn | 1 bit   |           integer               |
| 左ウインカー信号           | left_turn  | 1 bit   |           integer               |
| ブレーキ信号               |   brake   | 1 bit   |           integer               |
| バック信号                 |  reverse  | 1 bit   |           integer               |
| フォグライト信号           |    fog    | 1 bit   |           integer               |
| サイドマーカー             | side_marker | 1 bit  |           integer               |
| ホーン状態                 |   horn    | 1 bit   |           integer               |
| エアコン状態               | air_conditioner | 1 bit |         integer               |
| ニュートラル信号           |  neutral  | 1 bit   |           integer               |
| リターダ作動              |  retarder | 1 bit   |           integer               |
| ABS作動                   |    abs    | 1 bit   |           integer               |
| ヒーター作動              |  heater   | 1 bit   |           integer               |
| クラッチ状態              |   cluth   | 1 bit   |           integer               |

- 信号状態は `signal` マップ内

|       フィールド       | JSON キー名 | 値の型 | JSON 内の型 |
| :-------------------: | :---------: | :----: | :---------: |
| ロービーム信号         |  low_beam  | 1 bit  |  integer    |
| ハイビーム信号         | high_beam  | 1 bit  |  integer    |

例:

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

#### 位置情報照会 `"msg_id": 33281` 0x8201

空の JSON

#### 位置情報照会応答 `"msg_id": 513` 0x0201

|      フィールド       | JSON キー名 | 値の型 | JSON 内の型 |
| :-------------------: | :---------: | :----: | :---------: |
| 応答シーケンス         |    seq      |  word  |  integer    |
| 位置報告               |   params    |   -    |    map      |

#### 一時位置追跡制御 `"msg_id": 33282` 0x8202

|       フィールド       | JSON キー名 | 値の型 | JSON 内の型 |
| :-------------------: | :---------: | :----: | :---------: |
| 間隔（秒）             |   period    |  word  |  integer    |
| 追跡期間               |   expiry   | dword  |  integer    |

#### 手動警報確認メッセージ `"msg_id": 33283` 0x8203

|       フィールド       | JSON キー名 | 値の型 | JSON 内の型 |
| :-------------------: | :---------: | :----: | :---------: |
| 警報メッセージシーケンス |    seq      |  word  |  integer    |
| 手動警報タイプ         |    type     | dword  |  integer    |

#### テキストメッセージ送信 `"msg_id": 33536` 0x8300

|    フィールド    | JSON キー名 | 値の型 | JSON 内の型 |
| :--------------: | :---------: | :----: | :---------: |
| フラグ            |    flag     |  byte  |  integer    |
| テキスト情報      |    text     | string |  string     |

#### イベント設定 `"msg_id": 33537` 0x8301

|      フィールド      | JSON キー名 | 値の型 |                JSON 内の型                 |
| :------------------: | :---------: | :----: | :---------------------------------------: |
| 設定タイプ           |    type     |  byte  |                integer                    |
| イベント総数         |   length    |  byte  |                integer                    |
| イベントリスト       |   events    |  list  | イベントのリスト `[{"id": ID, "length": LEN, "content": CON}, ...]` |
| イベントID           |     id      |  byte  |                integer                    |
| イベント長さ         |   length    |  byte  |                integer                    |
| イベント内容         |   content   | string |                string                     |

#### イベント報告 `"msg_id": 769` 0x0301

|   フィールド   | JSON キー名 | 値の型 | JSON 内の型 |
| :------------: | :---------: | :----: | :---------: |
| イベントID     |     id      |  byte  |  integer    |

#### 質問送信 `"msg_id": 33538` 0x8302

|          フィールド          | JSON キー名 | 値の型 |                JSON 内の型                 |
| :-------------------------: | :---------: | :----: | :---------------------------------------: |
| フラグ                      |    flag     |  byte  |                integer                    |
| 質問長さ                    |   length    |  byte  |                integer                    |
| 質問内容                    |  question   | string |                string                     |
| 回答候補リスト              |   answers   |  list  | 回答のリスト `[{"id": ID, "len": LEN, "answer": ANS}, ...]` |
| 回答ID                      |     id      |  byte  |                integer                    |
| 回答内容長さ                |    len      |  byte  |                integer                    |
| 回答内容                    |   answer    | string |                string                     |

#### 質問応答 `"msg_id": 770` 0x0302

|     フィールド     | JSON キー名 | 値の型 | JSON 内の型 |
| :----------------: | :---------: | :----: | :---------: |
| 応答シーケンス      |    seq      |  word  |  integer    |
| 回答ID             |     id      |  byte  |  integer    |

#### 情報サービスメニュー設定 `"msg_id": 33539` 0x8303

|        フィールド        | JSON キー名 | 値の型 | JSON 内の型 |
| :---------------------: | :---------: | :----: | :---------: |
| 設定タイプ              |    type     |  byte  |  integer    |
| 情報項目総数            |   length    |  byte  |  integer    |
| 情報項目リスト          |    menus    |  list  | メニューのリスト |
| 情報タイプ              |    type     |  byte  |  integer    |
| 情報名長さ              |   length    |  word  |  integer    |
| 情報名                  |    info     | string |  string     |

#### 情報サービス／キャンセル `"msg_id": 771` 0x0303

|       フィールド        | JSON キー名 | 値の型 | JSON 内の型 |
| :--------------------: | :---------: | :----: | :---------: |
| 情報タイプ              |     id      |  byte  |  integer    |
| 発信／キャンセルフラグ  |    flag     |  byte  |  integer    |

#### 情報サービス `"msg_id": 33540` 0x8304

|      フィールド      | JSON キー名 | 値の型 | JSON 内の型 |
| :-----------------: | :---------: | :----: | :---------: |
| 情報タイプ          |    type     |  byte  |  integer    |
| 情報長さ            |   length    |  word  |  integer    |
| 情報内容            |    info     | string |  string     |

#### コールバック電話 `"msg_id": 33792` 0x8400

|      フィールド      | JSON キー名 | 値の型 | JSON 内の型 |
| :-----------------: | :---------: | :----: | :---------: |
| フラグ              |    type     |  byte  |  integer    |
| 電話番号            |    phone    | string |  string     |

#### 電話帳設定 `"msg_id": 33793` 0x8401

|        フィールド        | JSON キー名 | 値の型 | JSON 内の型 |
| :---------------------: | :---------: | :----: | :---------: |
| 設定タイプ              |    type     |  byte  |  integer    |
| 連絡先総数              |   length    |  byte  |  integer    |
| 連絡先項目              |  contacts   |  list  | 連絡先のリスト |
| フラグ                  |    type     |  byte  |  integer    |
| 電話番号長さ            |  phone_len  |  byte  |  integer    |
| 電話番号                |    phone    | string |  string     |
| 連絡先名長さ            |  name_len   |  byte  |  integer    |
| 連絡先名                |    name     | string |  string     |

連絡先項目例

```json
[{"type": TYPE, "phone_len": PH_LEN, "phone": PHONE, "name_len": NAME_LEN, "name": NAME}, ...]
```

#### 車両制御 `"msg_id": 34048` 0x8500

|      フィールド      | JSON キー名 | 値の型 | JSON 内の型 |
| :-----------------: | :---------: | :----: | :---------: |
| 制御フラグ          |    flag     |  byte  |  integer    |

#### 車両制御応答 `"msg_id": 1280` 0x0500

|          フィールド          | JSON キー名 | 値の型 |        JSON 内の型         |
| :-------------------------: | :---------: | :----: | :-----------------------: |
| 応答シリアル                |    seq      |  word  |        integer            |
| 位置報告本文                |  location   |  map   |  位置情報のマップ         |

#### 円形エリア設定 `"msg_id": 34304` 0x8600

|         フィールド         | JSON キー名 | 値の型 | JSON 内の型 |
| :-----------------------: | :---------: | :----: | :---------: |
| 設定属性                  |    type     |  byte  |  integer    |
| エリア総数                |   length    |  byte  |  integer    |
| エリア項目                |   areas    |  list  | エリアのリスト |
| エリアID                  |     id      | dword  |  integer    |
| エリアプロパティ          |    flag     | dword  |  integer    |
| 中心緯度                  | center_latitude | dword |  integer    |
| 中心経度                  | center_longitude | dword |  integer    |
| 半径                      |   radius   | dword  |  integer    |
| 開始時刻                  | start_time | string |  string     |
| 終了時刻                  |  end_time  | string |  string     |
| 最大速度                  | max_speed  |  word  |  integer    |
| 超速継続時間              | overspeed_duration | byte |  integer    |

エリアリスト例

```json
[{"id": ID,
  "flag": FLAG,
  "center_latitude": CEN_LAT,
  "center_longitude": CEN_LON,
  "radius": RADIUS,
  "start_time": START_TIME,
  "end_time": END_TIME,
  "max_speed": MAX_SPEED,
  "overspeed_duration": OVERSPEED_DURATION
  },
 ...
]
```

#### 円形エリア削除 `"msg_id": 34305` 0x8601

|       フィールド       | JSON キー名 | 値の型 | JSON 内の型 |
| :-------------------: | :---------: | :----: | :---------: |
| エリア数              |   length    |  byte  |  integer    |
| エリアIDリスト        |     ids     |  list  | IDのリスト  |
| エリアID 1～n         |      -      | dword  |  integer    |

```json
[ID1, ID2, ...]
```

#### 矩形エリア設定 `"msg_id": 34306` 0x8602

|         フィールド         | JSON キー名 | 値の型 |        JSON 内の型         |
| :-----------------------: | :---------: | :----: | :-----------------------: |
| 設定属性                  |    type     |  byte  |        integer            |
| エリア総数                |   length    |  byte  |        integer            |
| エリア項目                |   areas    |  list  | 矩形エリアのリスト        |
| エリアID                  |     id      | dword  |        integer            |
| エリアプロパティ          |    flag     | dword  |        integer            |
| 左上緯度                  |   lt_lat    | dword  |        integer            |
| 左上経度                  |   lt_lng    | dword  |        integer            |
| 右下緯度                  |   rb_lat    | dword  |        integer            |
| 右下経度                  |   rb_lng    | dword  |        integer            |
| 開始時刻                  | start_time  | string |        string             |
| 終了時刻                  |  end_time   | string |        string             |
| 最大速度                  | max_speed   |  word  |        integer            |
| 超速継続時間              | overspeed_duration | byte |    integer            |

#### 矩形エリア削除 `"msg_id": 34307` 0x8603

|       フィールド       | JSON キー名 | 値の型 | JSON 内の型 |
| :-------------------: | :---------: | :----: | :---------: |
| エリア数              |   length    |  byte  |  integer    |
| エリアIDリスト        |     ids     |  list  | IDのリスト  |
| エリアID 1～n         |      -      | dword  |  integer    |

#### 多角形エリア設定 `"msg_id": 34308` 0x8604

|           フィールド           | JSON キー名 | 値の型 | JSON 内の型 |
| :----------------------------: | :---------: | :----: | :---------: |
| エリアID                      |     id      | dword  |  integer    |
| エリアプロパティ              |    flag     | dword  |  integer    |
| 開始時刻                      | start_time  | string |  string     |
| 終了時刻                      |  end_time   | string |  string     |
| 最大速度                      | max_speed   |  word  |  integer    |
| 超速継続時間                  | overspeed_duration | byte | integer    |
| 頂点総数                      |   length    |  word  |  integer    |
| 頂点リスト                    |   points    |  list  | 頂点のリスト |
| 頂点緯度                      |    lat      | dword  |  integer    |
| 頂点経度                      |    lng      | dword  |  integer    |

#### 多角形エリア削除 `"msg_id": 34309` 0x8605

|       フィールド       | JSON キー名 | 値の型 | JSON 内の型 |
| :-------------------: | :---------: | :----: | :---------: |
| エリア数              |   length    |  byte  |  integer    |
| エリアIDリスト        |     ids     |  list  | IDのリスト  |
| エリアID 1～n         |      -      | dword  |  integer    |

#### ルート設定 `"msg_id": 34310` 0x8606

|             フィールド             | JSON キー名 | 値の型 | JSON 内の型 |
| :-------------------------------: | :---------: | :----: | :---------: |
| ルートID                         |     id      | dword  |  integer    |
| ルートプロパティ                 |    flag     |  word  |  integer    |
| 開始時刻                        | start_time  | string |  string     |
| 終了時刻                        |  end_time   | string |  string     |
| 曲がり角総数                    |   length    |  word  |  integer    |
| 曲がり角項目                    |   points    |  list  | 頂点のリスト |
| 曲がり角ID                     |  point_id   | dword  |  integer    |
| 区間ID                         |  path_id    | dword  |  integer    |
| 曲がり角緯度                   | point_lat   | dword  |  integer    |
| 曲がり角経度                   | point_lng   | dword  |  integer    |
| 区間幅                         |    width   |  byte  |  integer    |
| 区間属性                       |   attrib   |  byte  |  integer    |
| 区間走行超過閾値               |   passed   |  word  |  integer    |
| 区間走行不足閾値               |  uncovered |  word  |  integer    |
| 区間最大速度                   | max_speed  |  word  |  integer    |
| 区間超速継続時間               | overspeed_duration | byte | integer    |

#### ルート削除 `"msg_id": 34311` 0x8607

|       フィールド       | JSON キー名 | 値の型 | JSON 内の型 |
| :-------------------: | :---------: | :----: | :---------: |
| ルート数              |   length    |  byte  |  integer    |
| ルートIDリスト        |     ids     |  list  | IDのリスト  |
| ルートID              |      -      | dword  |  integer    |

#### 運転記録データ収集コマンド `"msg_id": 34560` 0x8700

|      フィールド      | JSON キー名 |        値の型         | JSON 内の型 |
| :------------------: | :---------: | :-------------------: | :---------: |
| コマンド             |  command    |         byte          |  integer    |
| データブロック       |   param     | string(base64 encoded) |  string     |

#### 運転記録データアップロード `"msg_id": 1792` 0x0700

|           フィールド           | JSON キー名 |        値の型         | JSON 内の型 |
| :----------------------------: | :---------: | :-------------------: | :---------: |
| 応答シリアル番号              |    seq      |         word          |  integer    |
| コマンド                     |  command    |         byte          |  integer    |
| データブロック               |    data     | string(base64 encoded) |  string     |

#### 運転記録パラメータ下行コマンド `"msg_id": 34561` 0x8701

|      フィールド      | JSON キー名 |        値の型         | JSON 内の型 |
| :------------------: | :---------: | :-------------------: | :---------: |
| コマンド             |  command    |         byte          |  integer    |
| データブロック       |   param     | string(base64 encoded) |  string     |

#### 電子運送状報告 `"msg_id": 1793` 0x0701

|           フィールド           | JSON キー名 |        値の型         | JSON 内の型 |
| :----------------------------: | :---------: | :-------------------: | :---------: |
| 電子運送状長さ               |   length    |        dword          |  integer    |
| 電子運送状内容               |    data     | string(base64 encoded) |  string     |

#### 運転者身分情報アップロード要求 `"msg_id": 34562` 0x8702

空の JSON

#### 運転者身分情報収集報告 `"msg_id": 1794` 0x0702

|           フィールド           | JSON キー名 | 値の型 | JSON 内の型 |
| :----------------------------: | :---------: | :----: | :---------: |
| 状態                          |   status    |  byte  |  integer    |
| 時刻                          |    time    | string |  string     |
| ICカード読み取り結果          |  ic_result  |  byte  |  integer    |
| 運転者名                      | driver_name | string |  string     |
| 職業資格証明書コード          | certificate | string |  string     |
| 発行機関名                    | organization | string |  string     |
| 証明書有効期限                | cert_expiry | string |  string     |

#### 位置データ一括アップロード `"msg_id": 1796` 0x0704

|           フィールド           | JSON キー名 | 値の型 | JSON 内の型 |
| :----------------------------: | :---------: | :----: | :---------: |
| 位置データタイプ              |    type     |  byte  |  integer    |
| データ項目数                  |   length    |  word  |  integer    |
| 位置報告データ項目            |  location   |  list  | 位置情報のリスト |

#### CANバスデータアップロード `"msg_id": 1797` 0x0705

|           フィールド           | JSON キー名 | 値の型 |          JSON 内の型           |
| :----------------------------: | :---------: | :----: | :----------------------------: |
| データ項目数                  |   length    |  word  |          integer               |
| CANバスデータ受信時刻         |    time     |  bcd(5) |          integer               |
| CANバスデータ項目             |  can_data   |  list  | CANデータのリスト              |
| CANバスチャネル番号           |   channel   | 1 bit  |          integer               |
| フレームタイプ                | frame_type  | 1 bit  |          integer               |
| データ収集方法                | data_method | 1 bit  |          integer               |
| CANバスID                    |     id     | 29 bits |          integer               |
| CANデータ                    |    data    | binary | string(base64 encoded)         |

#### マルチメディアイベント情報アップロード `"msg_id": 2048` 0x0800

|          フィールド          | JSON キー名 | 値の型 | JSON 内の型 |
| :--------------------------: | :---------: | :----: | :---------: |
| マルチメディアデータID      |     id      | dword  |  integer    |
| マルチメディアタイプ        |    type     |  byte  |  integer    |
| マルチメディアエンコード形式 |   format    |  byte  |  integer    |
| イベント項目コード          |   event     |  byte  |  integer    |
| チャンネルID                |  channel    |  byte  |  integer    |

#### マルチメディアデータアップロード `"msg_id": 2049` 0x0801

|          フィールド          | JSON キー名 | 値の型 |           JSON 内の型            |
| :--------------------------: | :---------: | :----: | :-----------------------------: |
| マルチメディアID            |     id      | dword  |           integer               |
| マルチメディアタイプ        |    type     |  byte  |           integer               |
| マルチメディアエンコード形式 |   format    |  byte  |           integer               |
| イベント項目コード          |   event     |  byte  |           integer               |
| チャンネルID                |  channel    |  byte  |           integer               |
| 位置報告                    |  location   | byte(28) |           map                  |
| マルチメディアデータパッケージ | multimedia | binary | string(base64 encoded)          |

#### マルチメディアデータアップロード応答 `"msg_id": 34816` 0x8800

|              フィールド              | JSON キー名 | 値の型 | JSON 内の型 |
| :---------------------------------: | :---------: | :----: | :---------: |
| マルチメディアID                  |    mm_id    | dword  |  integer    |
| 再送パッケージ総数                |   length    |  byte  |  integer    |
| 再送パッケージIDリスト            |  retx_ids   |  list  | 再送IDのリスト |

#### 即時カメラ撮影コマンド `"msg_id": 34817` 0x8801

|          フィールド          | JSON キー名 | 値の型 | JSON 内の型 |
| :-------------------------: | :---------: | :----: | :---------: |
| チャンネルID               | channel_id  |  byte  |  integer    |
| 撮影コマンド               |  command   |  word  |  integer    |
| 間隔／録画時間             |  period    |  word  |  integer    |
| 保存フラグ                 |   save    |  byte  |  integer    |
| 解像度                     | resolution |  byte  |  integer    |
| 画像／動画品質             | quality   |  byte  |  integer    |
| 明るさ                     |  bright   |  byte  |  integer    |
| コントラスト               | contrast  |  byte  |  integer    |
| 彩度                       | saturate  |  byte  |  integer    |
| 色相                       | chromaticity | byte |  integer    |

#### 即時カメラ撮影応答 `"msg_id": 2053` 0x0805

|           フィールド           | JSON キー名 |     値の型     | JSON 内の型 |
| :----------------------------: | :---------: | :------------: | :---------: |
| 応答シリアル番号              |    seq      |      word      |  integer    |
| 結果                          |   result    |      byte      |  integer    |
| マルチメディアID数            |   length    |      word      |  integer    |
| マルチメディアIDリスト        |    ids      | byte(4*length) |  integer    |

#### 保存マルチメディアデータ取得 `"msg_id": 34818` 0x8802

|        フィールド        | JSON キー名 | 値の型 | JSON 内の型 |
| :---------------------: | :---------: | :----: | :---------: |
| マルチメディアタイプ    |             |  byte  |             |
| チャンネルID           |             |  byte  |             |
| イベント項目コード     |             |  byte  |             |
| 開始時刻               |             | string |             |
| 終了時刻               |             | string |             |

#### 保存マルチメディアデータ取得応答 `"msg_id": 2050` 0x0802

|              フィールド              | JSON キー名 | 値の型 |           JSON 内の型            |
| :---------------------------------: | :---------: | :----: | :-----------------------------: |
| 応答シリアル番号                    |    seq      |  word  |           integer               |
| マルチメディアデータ総数            |   length    |  word  |           integer               |
| 検索結果項目                      |   result    |  list  | 検索結果のリスト                |
| マルチメディアID                  |     id      | dword  |           integer               |
| マルチメディアタイプ              |    type     |  byte  |           integer               |
| チャンネルID                     |  channel    |  byte  |           integer               |
| イベント項目コード               |   event     |  byte  |           integer               |
| 位置報告                       |  location   | byte(28) |           map                  |

#### 保存マルチメディアデータアップロードコマンド `"msg_id": 34819` 0x8803

|        フィールド        | JSON キー名 | 値の型 | JSON 内の型 |
| :---------------------: | :---------: | :----: | :---------: |
| マルチメディアタイプ    |    type     |  byte  |  integer    |
| チャンネルID           |  channel    |  byte  |  integer    |
| イベント項目コード     |   event     |  byte  |  integer    |
| 開始時刻               | start_time  | string |  string     |
| 終了時刻               |  end_time   | string |  string     |
| 削除フラグ             |   delete    |  byte  |  integer    |

#### 音声録音開始コマンド `"msg_id": 34820` 0x8804

|          フィールド          | JSON キー名 | 値の型 | JSON 内の型 |
| :-------------------------: | :---------: | :----: | :---------: |
| 録音コマンド               |  command   |  byte  |  integer    |
| 録音時間                   |    time    |  word  |  integer    |
| 保存フラグ                 |    save    |  byte  |  integer    |
| 音声サンプリングレート     |    rate    |  byte  |  integer    |

#### 単一保存マルチメディア項目取得アップロードコマンド `"msg_id": 34821` 0x8805

|       フィールド       | JSON キー名 | 値の型 | JSON 内の型 |
| :-------------------: | :---------: | :----: | :---------: |
| マルチメディアID      |     id      | dword  |  integer    |
| 削除フラグ           |    flag     |  byte  |  integer    |

#### 下行データ送信 `"msg_id": 35072` 0x8900

|            フィールド             | JSON キー名 | 値の型 |           JSON 内の型            |
| :------------------------------: | :---------: | :----: | :-----------------------------: |
| 送信メッセージタイプ             |    type     |  byte  |           integer               |
| 送信メッセージ内容               |    data     | binary | string(base64 encoded)          |

#### 上行データ送信 `"msg_id": 2304` 0x0900

|            フィールド             | JSON キー名 | 値の型 |           JSON 内の型            |
| :------------------------------: | :---------: | :----: | :-----------------------------: |
| 送信メッセージタイプ             |    type     |  byte  |           integer               |
| 送信メッセージ内容               |    data     | binary | string(base64 encoded)          |

#### データ圧縮報告 `"msg_id": 2305` 0x0901

|           フィールド           | JSON キー名 | 値の型 |           JSON 内の型            |
| :----------------------------: | :---------: | :----: | :-----------------------------: |
| 圧縮メッセージ長さ            |   length    | dword  |           integer               |
| 圧縮メッセージ本文            |    data     | binary | string(base64 encoded)          |

#### プラットフォームRSA公開鍵 `"msg_id": 35328` 0x8A00

| フィールド | JSON キー名 | 値の型  |           JSON 内の型            |
| :--------: | :---------: | :-----: | :-----------------------------: |
|     e      |     e       | dword   |           integer               |
|     n      |     n       | byte(128) | string(base64 encoded)          |

#### 端末RSA公開鍵 `"msg_id": 2560` 0x0A00

| フィールド | JSON キー名 | 値の型  |           JSON 内の型            |
| :--------: | :---------: | :-----: | :-----------------------------: |
|     e      |     e       | dword   |           integer               |
|     n      |     n       | byte(128) | string(base64 encoded)          |

#### 予約済み 0x8F00 ～ 0x8FFF

#### 予約済み 0x0F00 ～ 0x0FFF
