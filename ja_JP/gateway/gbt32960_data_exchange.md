# GB/T 32960 ゲートウェイデータ交換フォーマット

GB/T 32960ゲートウェイ（`emqx_gbt32960`）は、GB/T 32960準拠の電気自動車端末とEMQXをMQTT経由で接続します。本ページでは、そのデータ交換に使用されるJSONメッセージフォーマットを定義します。

## プロトコルバージョン対応

ゲートウェイは**gbt32960-2016**および**gbt32960-2025**の両プロトコルバージョンをサポートしています。プロトコルバージョンはフレームヘッダーから自動検出されます：

- **gbt32960-2016**：フレームヘッダーは `0x23 0x23`（ASCII `##`）
- **gbt32960-2025**：フレームヘッダーは `0x24 0x24`（ASCII `$$`）

バージョン間でフィールドや挙動が異なる場合は、本ドキュメントで明示的に記載しています。

## 表記規則

- ペイロードはJSON形式で組み立てられます。
- JSONのキー名はUpperCamelCase（パスカルケース）を使用します。

## アップストリーム

データフロー：端末 → emqx_gbt32960 → EMQX

### 車両ログイン

トピック：`gbt32960/${vin}/upstream/vlogin`

```json
// gbt32960-2016
{
    "Cmd": 1,
    "Encrypt": 1,
    "Vin": "1G1BL52P7TR115520",
    "Data": {
        "ICCID": "12345678901234567890",
        "Id": "C",
        "Length": 1,
        "Num": 1,
        "Seq": 1,
        "Time": {
            "Day": 29,
            "Hour": 12,
            "Minute": 19,
            "Month": 12,
            "Second": 20,
            "Year": 12
        }
    }
}
```

```json
// gbt32960-2025
{
    "Cmd": 1,
    "Encrypt": 1,
    "Vin": "1G1BL52P7TR115520",
    "Data": {
        "ICCID": "12345678901234567890",
        "Seq": 1,
        "Time": {
            "Day": 29,
            "Hour": 12,
            "Minute": 19,
            "Month": 12,
            "Second": 20,
            "Year": 12
        },
        "BmsNum": 2,
        "BatteryPackCounts": [1, 2],
        "BatteryPackEncodings": [ ["encoding-string-bms1-pack1"], ["encoding-string-bms2-pack1", "encoding-string-bms2-pack2"] ]
    }
}
```

トップレベルフィールド：

| フィールド | 型       | 説明                                                                                   |
|------------|----------|----------------------------------------------------------------------------------------|
| `Cmd`      | 整数     | コマンド識別子。`1`は車両ログインを意味します                                         |
| `Encrypt`  | 整数     | データ暗号化方式：`1`=暗号化なし、`2`=RSA、`3`=AES128、`254`=エラー、`255`=無効         |
| `Vin`      | 文字列   | 車両識別番号（VIN）                                                                    |
| `Data`     | オブジェクト | データ単位、JSONオブジェクト形式                                                    |

`Data`の**gbt32960-2016**用フィールド：

| フィールド | 型       | 説明                                                                                          |
|------------|----------|-----------------------------------------------------------------------------------------------|
| `Time`     | オブジェクト | データ収集日時（年、月、日、時、分、秒）                                                    |
| `Seq`      | 整数     | ログインシーケンス番号                                                                        |
| `ICCID`    | 文字列   | 20文字のSIMカードICCID                                                                        |
| `Num`      | 整数     | 充電式エネルギー貯蔵サブシステムの数；有効範囲0〜250                                       |
| `Length`   | 整数     | 充電式エネルギー貯蔵システムのエンコーディング長；有効範囲0〜50                             |
| `Id`       | 文字列   | システムエンコーディング；長さはNum×Length                                                  |

`Data`の**gbt32960-2025**用フィールド：

| フィールド             | 型               | 説明                                                                                   |
|-----------------------|------------------|----------------------------------------------------------------------------------------|
| `Time`                | オブジェクト     | データ収集日時（年、月、日、時、分、秒）                                             |
| `Seq`                 | 整数             | ログインシーケンス番号                                                                 |
| `ICCID`               | 文字列           | 20文字のSIMカードICCID                                                                 |
| `BmsNum`              | 整数             | バッテリーマネジメントシステム（BMS）の数                                             |
| `BatteryPackCounts`   | 配列             | 各BMSが管理するバッテリーパックの数                                                  |
| `BatteryPackEncodings`| 配列\<配列\<文字列\>\> | 各BMSごとのバッテリーパックエンコーディングリスト。各エンコーディングは24文字の文字列 |

### 車両ログアウト

トピック：`gbt32960/${vin}/upstream/vlogout`

`Cmd`値は`4`です。その他のフィールドは車両ログインと同じ構造です：

```json
{
    "Cmd": 4,
    "Encrypt": 1,
    "Vin": "1G1BL52P7TR115520",
    "Data": {
        "Seq": 1,
        "Time": {
            "Day": 1,
            "Hour": 2,
            "Minute": 59,
            "Month": 1,
            "Second": 0,
            "Year": 16
        }
    }
}
```

### リアルタイムデータレポート

トピック：`gbt32960/${vin}/upstream/info`

各レポートは`Infos`配列に複数の情報項目を含めることができます。`Type`フィールドで情報タイプを区別します。

#### 車両データ

```json
{
    "Cmd": 2,
    "Encrypt": 1,
    "Vin": "1G1BL52P7TR115520",
    "Data": {
        "Infos": [
            {
                "AcceleratorPedal": 90,
                "BrakePedal": 0,
                "Charging": 1,
                "Current": 15000,
                "DC": 1,
                "Gear": 5,
                "Mileage": 999999,
                "Mode": 1,
                "Resistance": 6000,
                "SOC": 50,
                "Speed": 2000,
                "Status": 1,
                "Type": "Vehicle",
                "Voltage": 5000
            }
        ],
        "Time": {
            "Day": 1,
            "Hour": 2,
            "Minute": 59,
            "Month": 1,
            "Second": 0,
            "Year": 16
        }
    }
}
```

| フィールド           | 型       | 説明                                                                                                         |
|----------------------|----------|--------------------------------------------------------------------------------------------------------------|
| `Type`               | 文字列   | 情報タイプ。この構造では`Vehicle`                                                                           |
| `Status`             | 整数     | 車両状態：`1`=走行中、`2`=停止、`3`=その他、`254`=エラー、`255`=無効                                       |
| `Charging`           | 整数     | 充電状態：`1`=駐車充電、`2`=走行充電、`3`=非充電、`4`=充電完了、`254`=エラー、`255`=無効                   |
| `Mode`               | 整数     | 動作モード：`1`=純電動、`2`=ハイブリッド、`3`=燃料、`254`=エラー、`255`=無効                               |
| `Speed`              | 整数     | 車速；有効範囲0〜2200（0〜220.0 km/h）、単位：0.1 km/h                                                     |
| `Mileage`            | 整数     | 累積走行距離；有効範囲0〜9,999,999（0〜999,999.9 km）、単位：0.1 km                                        |
| `Voltage`            | 整数     | 総電圧；有効範囲0〜10000（0〜1000 V）、単位：0.1 V                                                         |
| `Current`            | 整数     | 総電流；有効範囲0〜20000（オフセット1000、-1000 A〜+1000 A）、単位：0.1 A                                   |
| `SOC`                | 整数     | 充電状態（State of Charge）；有効範囲0〜100（0%〜100%）                                                    |
| `DC`                 | 整数     | DC/DC状態：`1`=動作中、`2`=切断、`254`=エラー、`255`=無効                                                 |
| `Gear`               | 整数     | ギア位置；プロトコルの表A.1に準じたギア表の整数表現                                                       |
| `Resistance`         | 整数     | 絶縁抵抗；有効範囲0〜60000（0〜60000 kΩ）                                                                  |
| `AcceleratorPedal`   | 整数     | **gbt32960-2016のみ**：アクセルペダル開度；有効範囲0〜100                                                  |
| `BrakePedal`         | 整数     | **gbt32960-2016のみ**：ブレーキペダル状態；有効範囲0〜100                                                  |

#### 駆動モーターデータ

```json
{
    "Cmd": 2,
    "Encrypt": 1,
    "Vin": "1G1BL52P7TR115520",
    "Data": {
        "Infos": [
            {
                "Motors": [
                    {
                        "CtrlTemp": 125,
                        "DCBusCurrent": 31203,
                        "InputVoltage": 30012,
                        "MotorTemp": 125,
                        "No": 1,
                        "Rotating": 30000,
                        "Status": 1,
                        "Torque": 25000
                    },
                    {
                        "CtrlTemp": 125,
                        "DCBusCurrent": 30200,
                        "InputVoltage": 32000,
                        "MotorTemp": 145,
                        "No": 2,
                        "Rotating": 30200,
                        "Status": 1,
                        "Torque": 25300
                    }
                ],
                "Number": 2,
                "Type": "DriveMotor"
            }
        ],
        "Time": {
            "Day": 1,
            "Hour": 2,
            "Minute": 59,
            "Month": 1,
            "Second": 0,
            "Year": 16
        }
    }
}
```

| フィールド  | 型       | 説明                                      |
|------------|----------|-------------------------------------------|
| `Type`     | 文字列   | 情報タイプ。この構造では`DriveMotor`      |
| `Number`   | 整数     | 駆動モーターの数；有効範囲1〜253          |
| `Motors`   | 配列     | 駆動モーターのデータリスト                  |

モーターエントリのフィールド：

| フィールド       | 型       | 説明                                                                                                                                               |
|------------------|----------|----------------------------------------------------------------------------------------------------------------------------------------------------|
| `No`             | 整数     | モーターシーケンス番号；有効範囲1〜253                                                                                                            |
| `Status`         | 整数     | モーター状態：`1`=消費中、`2`=発電中、`3`=停止、`4`=待機、`254`=エラー、`255`=無効                                                               |
| `CtrlTemp`       | 整数     | コントローラー温度；有効範囲0〜250（オフセット40℃、-40℃〜+210℃）、単位：1℃                                                                       |
| `Rotating`       | 整数     | ローター回転数；有効範囲0〜65531（オフセット20000、-20000〜45531 rpm）、単位：1 rpm                                                               |
| `Torque`         | 整数     | **gbt32960-2016**：モータートルク；有効範囲0〜65531（オフセット20000、-2000〜4553.1 N·m）、単位：0.1 N·m<br>**gbt32960-2025**：モータートルク；有効範囲0〜429496729（オフセット200000、-20000〜229496.729 N·m）、単位：0.001 N·m |
| `MotorTemp`      | 整数     | モーター温度；有効範囲0〜250（オフセット40℃、-40℃〜+210℃）、単位：1℃                                                                             |
| `InputVoltage`   | 整数     | **gbt32960-2016のみ**：コントローラー入力電圧；有効範囲0〜60000（0〜6000 V）、単位：0.1 V                                                        |
| `DCBusCurrent`   | 整数     | **gbt32960-2016のみ**：DCバス電流；有効範囲0〜20000（オフセット1000 A、-1000 A〜+1000 A）、単位：0.1 A                                           |

#### 燃料電池データ

```json
{
    "Cmd": 2,
    "Encrypt": 1,
    "Vin": "1G1BL52P7TR115520",
    "Data": {
        "Infos": [
            {
                "CellCurrent": 12000,
                "CellVoltage": 10000,
                "DCStatus": 1,
                "FuelConsumption": 45000,
                "H_ConcSensorCode": 11,
                "H_MaxConc": 35000,
                "H_MaxPress": 500,
                "H_MaxTemp": 12500,
                "H_PressSensorCode": 12,
                "H_TempProbeCode": 10,
                "ProbeNum": 2,
                "ProbeTemps": [120, 121],
                "Type": "FuelCell"
            }
        ],
        "Time": {
            "Day": 1,
            "Hour": 2,
            "Minute": 59,
            "Month": 1,
            "Second": 0,
            "Year": 16
        }
    }
}
```

| フィールド             | 型       | 説明                                                                                                 |
|------------------------|----------|------------------------------------------------------------------------------------------------------|
| `Type`                 | 文字列   | 情報タイプ。この構造では`FuelCell`                                                                   |
| `CellVoltage`          | 整数     | **gbt32960-2016のみ**：燃料電池電圧；有効範囲0〜20000（0〜2000 V）、単位：0.1 V                      |
| `CellCurrent`          | 整数     | **gbt32960-2016のみ**：燃料電池電流；有効範囲0〜20000（0〜2000 A）、単位：0.1 A                      |
| `FuelConsumption`      | 整数     | **gbt32960-2016のみ**：燃料消費率；有効範囲0〜60000（0〜600 kg/100km）、単位：0.01 kg/100km          |
| `ProbeNum`             | 整数     | **gbt32960-2016のみ**：燃料電池プローブの総数；有効範囲0〜65531                                     |
| `ProbeTemps`           | 配列     | **gbt32960-2016のみ**：各燃料電池プローブの温度値                                                   |
| `H_MaxTemp`            | 整数     | 最大水素系統温度；有効範囲0〜2400（オフセット40℃、-40℃〜+200℃）、単位：0.1℃                      |
| `H_TempProbeCode`      | 整数     | 最大水素温度のプローブコード；有効範囲1〜252                                                        |
| `H_MaxConc`            | 整数     | 最大水素濃度；有効範囲0〜60000（0〜50000 mg/kg）、単位：1 mg/kg                                    |
| `H_ConcSensorCode`     | 整数     | 最大水素濃度のセンサーコード；有効範囲1〜252                                                        |
| `H_MaxPress`           | 整数     | 最大水素圧力；有効範囲0〜1000（0〜100 MPa）、単位：0.1 MPa                                         |
| `H_PressSensorCode`    | 整数     | 最大水素圧力のセンサーコード；有効範囲1〜252                                                        |
| `DCStatus`             | 整数     | 高電圧DC/DC状態：`1`=動作中、`2`=切断                                                              |
| `RemainingH2`          | 整数     | **gbt32960-2025のみ**：水素系統の残量；単位：1 kg                                                  |
| `DCDCTemp`             | 整数     | **gbt32960-2025のみ**：高電圧DCDC温度；オフセット40℃、-40℃〜+210℃                                |

#### エンジンデータ

```json
{
    "Cmd": 2,
    "Encrypt": 1,
    "Vin": "1G1BL52P7TR115520",
    "Data": {
        "Infos": [
            {
                "CrankshaftSpeed": 2000,
                "FuelConsumption": 200,
                "Status": 1,
                "Type": "Engine"
            }
        ],
        "Time": {
            "Day": 1,
            "Hour": 22,
            "Minute": 59,
            "Month": 10,
            "Second": 0,
            "Year": 16
        }
    }
}
```

| フィールド           | 型       | 説明                                                                                         |
|----------------------|----------|----------------------------------------------------------------------------------------------|
| `Type`               | 文字列   | 情報タイプ。この構造では`Engine`                                                             |
| `Status`             | 整数     | **gbt32960-2016のみ**：エンジン状態：`1`=稼働中、`2`=停止                                  |
| `CrankshaftSpeed`    | 整数     | クランクシャフト回転数；有効範囲0〜60000（0〜60000 rpm）、単位：1 rpm                        |
| `FuelConsumption`    | 整数     | **gbt32960-2016のみ**：燃料消費率；有効範囲0〜60000（0〜600 L/100km）、単位：0.01 L/100km    |

#### 車両位置データ

```json
{
    "Cmd": 2,
    "Encrypt": 1,
    "Vin": "1G1BL52P7TR115520",
    "Data": {
        "Infos": [
            {
                "Latitude": 100,
                "Longitude": 10,
                "Status": 0,
                "Type": "Location"
            }
        ],
        "Time": {
            "Day": 1,
            "Hour": 22,
            "Minute": 59,
            "Month": 10,
            "Second": 0,
            "Year": 16
        }
    }
}
```

| フィールド           | 型       | 説明                                                                                     |
|----------------------|----------|------------------------------------------------------------------------------------------|
| `Type`               | 文字列   | 情報タイプ。この構造では`Location`                                                      |
| `Status`             | 整数     | 位置状態；全状態ビットの整数値（プロトコル表15参照）                                   |
| `CoordinateSystem`   | 整数     | **gbt32960-2025のみ**：座標系：`1`=WGS-84、`2`=GCJ-02                                  |
| `Longitude`          | 整数     | 経度（度×10^6）；小数点以下6桁の精度                                                   |
| `Latitude`           | 整数     | 緯度（度×10^6）；小数点以下6桁の精度                                                   |

#### 極値データ

> **gbt32960-2016のみ**

```json
{
    "Cmd": 2,
    "Encrypt": 1,
    "Vin": "1G1BL52P7TR115520",
    "Data": {
        "Infos": [
            {
                "MaxBatteryVoltage": 7500,
                "MaxTemp": 120,
                "MaxTempProbeNo": 12,
                "MaxTempSubsysNo": 14,
                "MaxVoltageBatteryCode": 10,
                "MaxVoltageBatterySubsysNo": 12,
                "MinBatteryVoltage": 2000,
                "MinTemp": 40,
                "MinTempProbeNo": 13,
                "MinTempSubsysNo": 15,
                "MinVoltageBatteryCode": 11,
                "MinVoltageBatterySubsysNo": 13,
                "Type": "Extreme"
            }
        ],
        "Time": {
            "Day": 30,
            "Hour": 12,
            "Minute": 22,
            "Month": 5,
            "Second": 59,
            "Year": 17
        }
    }
}
```

| フィールド                  | 型       | 説明                                                                                             |
|-----------------------------|----------|--------------------------------------------------------------------------------------------------|
| `Type`                      | 文字列   | 情報タイプ。この構造では`Extreme`                                                               |
| `MaxVoltageBatterySubsysNo` | 整数     | 最高電圧のバッテリーサブシステム番号；有効範囲1〜250                                            |
| `MaxVoltageBatteryCode`     | 整数     | 最高電圧のバッテリーセルコード；有効範囲1〜250                                                  |
| `MaxBatteryVoltage`         | 整数     | 最高個別セル電圧；有効範囲0〜15000（0〜15 V）、単位：0.001 V                                   |
| `MinVoltageBatterySubsysNo` | 整数     | 最低電圧のバッテリーサブシステム番号；有効範囲1〜250                                            |
| `MinVoltageBatteryCode`     | 整数     | 最低電圧のバッテリーセルコード；有効範囲1〜250                                                  |
| `MinBatteryVoltage`         | 整数     | 最低個別セル電圧；有効範囲0〜15000（0〜15 V）、単位：0.001 V                                   |
| `MaxTempSubsysNo`           | 整数     | 最高温度のサブシステム番号；有効範囲1〜250                                                      |
| `MaxTempProbeNo`            | 整数     | 最高温度のプローブ番号；有効範囲1〜250                                                          |
| `MaxTemp`                   | 整数     | 最高温度；有効範囲0〜250（オフセット40、-40℃〜+210℃）                                         |
| `MinTempSubsysNo`           | 整数     | 最低温度のサブシステム番号；有効範囲1〜250                                                      |
| `MinTempProbeNo`            | 整数     | 最低温度のプローブ番号；有効範囲1〜250                                                          |
| `MinTemp`                   | 整数     | 最低温度；有効範囲0〜250（オフセット40、-40℃〜+210℃）                                         |

#### アラームデータ

```json
{
    "Cmd": 2,
    "Encrypt": 1,
    "Vin": "1G1BL52P7TR115520",
    "Data": {
        "Infos": [
            {
                "FaultChargeableDeviceNum": 1,
                "FaultChargeableDeviceList": ["00C8"],
                "FaultDriveMotorNum": 0,
                "FaultDriveMotorList": [],
                "FaultEngineNum": 1,
                "FaultEngineList": ["006F"],
                "FaultOthersNum": 0,
                "FaultOthersList": [],
                "GeneralAlarmFlag": 3,
                "MaxAlarmLevel": 1,
                "Type": "Alarm"
            }
        ],
        "Time": {
            "Day": 20,
            "Hour": 22,
            "Minute": 23,
            "Month": 12,
            "Second": 59,
            "Year": 17
        }
    }
}
```

| フィールド                  | 型       | 説明                                                                                                            |
|-----------------------------|----------|-----------------------------------------------------------------------------------------------------------------|
| `Type`                      | 文字列   | 情報タイプ。この構造では`Alarm`                                                                                |
| `MaxAlarmLevel`             | 整数     | 最高アラームレベル。**gbt32960-2016**：有効範囲0〜3（"0"=異常なし、"1"=レベル1異常）<br>**gbt32960-2025**：有効範囲0〜4、新値"4"=熱イベント異常 |
| `GeneralAlarmFlag`          | 整数     | 一般アラームフラグビット（プロトコル表18参照）                                                                |
| `FaultChargeableDeviceNum`  | 整数     | 充電式エネルギー貯蔵装置の故障総数；有効範囲0〜252                                                           |
| `FaultChargeableDeviceList` | 配列     | 充電式エネルギー貯蔵装置の故障コードリスト                                                                    |
| `FaultDriveMotorNum`        | 整数     | 駆動モーター故障総数；有効範囲0〜252                                                                           |
| `FaultDriveMotorList`       | 配列     | 駆動モーター故障コードリスト                                                                                    |
| `FaultEngineNum`            | 整数     | エンジン故障総数；有効範囲0〜252                                                                               |
| `FaultEngineList`           | 配列     | エンジン故障コードリスト                                                                                        |
| `FaultOthersNum`            | 整数     | その他故障総数                                                                                                  |
| `FaultOthersList`           | 配列     | その他故障コードリスト                                                                                          |
| `FaultGeneralNum`           | 整数     | **gbt32960-2025のみ**：一般故障総数                                                                             |
| `FaultGeneralList`          | 配列     | **gbt32960-2025のみ**：一般故障リスト。各エントリはJSONオブジェクト`{"No":整数, "Level":整数}`                 |

#### 充電式エネルギー貯蔵電圧データ

> **gbt32960-2016のみ**

```json
{
    "Cmd": 2,
    "Encrypt": 1,
    "Vin": "1G1BL52P7TR115520",
    "Data": {
        "Infos": [
            {
                "Number": 2,
                "SubSystems": [
                    {
                        "CellsTotal": 2,
                        "CellsVoltage": [5000],
                        "ChargeableCurrent": 10000,
                        "ChargeableSubsysNo": 1,
                        "ChargeableVoltage": 5000,
                        "FrameCellsCount": 1,
                        "FrameCellsIndex": 0
                    },
                    {
                        "CellsTotal": 2,
                        "CellsVoltage": [5001],
                        "ChargeableCurrent": 10001,
                        "ChargeableSubsysNo": 2,
                        "ChargeableVoltage": 5001,
                        "FrameCellsCount": 1,
                        "FrameCellsIndex": 1
                    }
                ],
                "Type": "ChargeableVoltage"
            }
        ],
        "Time": {
            "Day": 1,
            "Hour": 22,
            "Minute": 59,
            "Month": 10,
            "Second": 0,
            "Year": 16
        }
    }
}
```

| フィールド       | 型       | 説明                                                                                      |
|------------------|----------|-------------------------------------------------------------------------------------------|
| `Type`           | 文字列   | 情報タイプ。この構造では`ChargeableVoltage`                                               |
| `Number`         | 整数     | 充電式エネルギー貯蔵サブシステム数；有効範囲1〜250                                       |
| `SubSystems`     | 配列     | サブシステム電圧データのリスト                                                            |

サブシステム電圧フィールド：

| フィールド            | 型       | 説明                                                                                                         |
|-----------------------|----------|--------------------------------------------------------------------------------------------------------------|
| `ChargeableSubsysNo`  | 整数     | サブシステム番号；有効範囲1〜250                                                                             |
| `ChargeableVoltage`   | 整数     | サブシステム電圧；有効範囲0〜10000（0〜1000 V）、単位：0.1 V                                               |
| `ChargeableCurrent`   | 整数     | サブシステム電流；有効範囲0〜20000（オフセット1000 A、-1000 A〜+1000 A）、単位：0.1 A                      |
| `CellsTotal`          | 整数     | バッテリーセル総数；有効範囲1〜65531                                                                       |
| `FrameCellsIndex`     | 整数     | 本フレームで報告されるセルの開始インデックス（1始まり）；有効範囲1〜65531。セル数が200を超える場合はフレーム分割される。 |
| `FrameCellsCount`     | 整数     | 本フレーム内のセル数；有効範囲1〜200                                                                         |
| `CellsVoltage`        | 配列     | 個別セル電圧；有効範囲0〜60000（0〜60.000 V）、単位：0.001 V                                             |

#### 充電式エネルギー貯蔵温度データ

> **gbt32960-2016のみ**

```json
{
    "Cmd": 2,
    "Encrypt": 1,
    "Vin": "1G1BL52P7TR115520",
    "Data": {
        "Infos": [
            {
                "Number": 2,
                "SubSystems": [
                    {
                        "ChargeableSubsysNo": 1,
                        "ProbeNum": 10,
                        "ProbesTemp": [0, 0, 0, 0, 0, 0, 0, 0, 19, 136]
                    },
                    {
                        "ChargeableSubsysNo": 2,
                        "ProbeNum": 1,
                        "ProbesTemp": [100]
                    }
                ],
                "Type": "ChargeableTemp"
            }
        ],
        "Time": {
            "Day": 1,
            "Hour": 22,
            "Minute": 59,
            "Month": 10,
            "Second": 0,
            "Year": 16
        }
    }
}
```

| フィールド       | 型       | 説明                                                                                 |
|------------------|----------|--------------------------------------------------------------------------------------|
| `Type`           | 文字列   | 情報タイプ。この構造では`ChargeableTemp`                                            |
| `Number`         | 整数     | 温度情報リスト内のサブシステム数                                                    |
| `SubSystems`     | 配列     | サブシステム温度データのリスト                                                      |

サブシステム温度フィールド：

| フィールド            | 型       | 説明                                                                                 |
|-----------------------|----------|--------------------------------------------------------------------------------------|
| `ChargeableSubsysNo`  | 整数     | サブシステム番号；有効範囲1〜250                                                    |
| `ProbeNum`            | 整数     | 本サブシステム内の温度プローブ数                                                    |
| `ProbesTemp`          | 配列     | 各プローブの温度読み値                                                              |

#### パワーバッテリーパック電圧データ

> **gbt32960-2025のみ**

```json
{
    "Cmd": 2,
    "Encrypt": 1,
    "Vin": "1G1BL52P7TR115520",
    "Data": {
        "Infos": [
            {
                "Type": "MinVoltageOfPowerBattery",
                "Number": 2,
                "SubSystems": [
                    {
                        "BatteryPackNo": 1,
                        "BatteryPackVoltage": 3000,
                        "BatteryPackCurrent": 1000,
                        "MinParallelUnitTotal": 2,
                        "MinParallelUnitVoltage": [1200, 1201]
                    },
                    {
                        "BatteryPackNo": 2,
                        "BatteryPackVoltage": 3100,
                        "BatteryPackCurrent": 1100,
                        "MinParallelUnitTotal": 2,
                        "MinParallelUnitVoltage": [1300, 1301]
                    }
                ]
            }
        ],
        "Time": {
            "Day": 13,
            "Hour": 15,
            "Minute": 30,
            "Month": 11,
            "Second": 0,
            "Year": 25
        }
    }
}
```

| フィールド               | 型       | 説明                                                                                   |
|--------------------------|----------|----------------------------------------------------------------------------------------|
| `Type`                   | 文字列   | `MinVoltageOfPowerBattery`                                                            |
| `Number`                 | 整数     | パワーバッテリーパックの数                                                            |
| `SubSystems`             | 配列     | バッテリーパックのリスト                                                               |
| `BatteryPackNo`          | 整数     | バッテリーパックシーケンス番号                                                        |
| `BatteryPackVoltage`     | 整数     | バッテリーパック電圧；単位：0.1 V                                                    |
| `BatteryPackCurrent`     | 整数     | バッテリーパック電流；オフセット1000 A、単位：0.1 A                                   |
| `MinParallelUnitTotal`   | 整数     | 最小並列ユニットの総数                                                                 |
| `MinParallelUnitVoltage` | 配列     | 最小並列ユニットの電圧リスト；オフセット4 V、単位：1 mV                               |

#### パワーバッテリーパック温度データ

> **gbt32960-2025のみ**

```json
{
    "Cmd": 2,
    "Encrypt": 1,
    "Vin": "1G1BL52P7TR115520",
    "Data": {
        "Infos": [
            {
                "Type": "TempOfPowerBattery",
                "Number": 2,
                "SubSystems": [
                    {
                        "BatteryPackNo": 1,
                        "ProbeNum": 5,
                        "ProbesTemp": [120, 121, 122, 123, 124]
                    },
                    {
                        "BatteryPackNo": 2,
                        "ProbeNum": 3,
                        "ProbesTemp": [130, 131, 132]
                    }
                ]
            }
        ],
        "Time": {
            "Day": 13,
            "Hour": 15,
            "Minute": 30,
            "Month": 11,
            "Second": 0,
            "Year": 25
        }
    }
}
```

| フィールド       | 型       | 説明                                                                                 |
|------------------|----------|--------------------------------------------------------------------------------------|
| `Type`           | 文字列   | `TempOfPowerBattery`                                                                 |
| `Number`         | 整数     | パワーバッテリーパックの数                                                          |
| `SubSystems`     | 配列     | バッテリーパックのリスト                                                             |
| `BatteryPackNo`  | 整数     | バッテリーパックシーケンス番号                                                      |
| `ProbeNum`       | 整数     | 温度プローブの数                                                                     |
| `ProbesTemp`     | 配列     | プローブ温度リスト；オフセット40℃、単位：1℃                                        |

#### 燃料電池スタックデータ

> **gbt32960-2025のみ**

```json
{
    "Cmd": 2,
    "Encrypt": 1,
    "Vin": "1G1BL52P7TR115520",
    "Data": {
        "Infos": [
            {
                "Type": "FuelCellStack",
                "Number": 1,
                "Stacks": [
                    {
                        "FuelCellStackNo": 1,
                        "Voltage": 2000,
                        "Current": 300,
                        "H2InletPressure": 1200,
                        "AirInletPressure": 1100,
                        "AirInletTemp": 50,
                        "StackProbeNum": 3,
                        "StackProbeTemp": [100, 101, 102]
                    }
                ]
            }
        ],
        "Time": {
            "Day": 13,
            "Hour": 15,
            "Minute": 30,
            "Month": 11,
            "Second": 0,
            "Year": 25
        }
    }
}
```

| フィールド             | 型       | 説明                                                                                 |
|------------------------|----------|--------------------------------------------------------------------------------------|
| `Type`                 | 文字列   | `FuelCellStack`                                                                     |
| `Number`               | 整数     | 燃料電池スタックの数                                                                |
| `Stacks`               | 配列     | 燃料電池スタックのリスト                                                             |
| `FuelCellStackNo`      | 整数     | 燃料電池スタックシーケンス番号                                                      |
| `Voltage`              | 整数     | 燃料電池スタック電圧；単位：0.1 V                                                  |
| `Current`              | 整数     | 燃料電池スタック電流；単位：0.1 A                                                  |
| `H2InletPressure`      | 整数     | 水素入口圧力；単位：0.1 bar                                                        |
| `AirInletPressure`     | 整数     | 空気入口圧力；単位：0.1 bar                                                        |
| `AirInletTemp`         | 整数     | 空気入口温度；オフセット40℃、単位：1℃                                             |
| `StackProbeNum`        | 整数     | スタック温度プローブの数                                                            |
| `StackProbeTemp`       | 配列     | スタック温度プローブの読み値                                                        |

#### スーパーキャパシタデータ

> **gbt32960-2025のみ**

```json
{
    "Cmd": 2,
    "Encrypt": 1,
    "Vin": "1G1BL52P7TR115520",
    "Data": {
        "Infos": [
            {
                "Type": "SuperCapacitor",
                "ManagerSysNo": 1,
                "TotalVoltage": 1000,
                "TotalCurrent": 2000,
                "CellsTotal": 3,
                "CellsVoltage": [1200, 1201, 1202],
                "ProbeNum": 2,
                "ProbeTemp": [100, 101]
            }
        ],
        "Time": {
            "Day": 13,
            "Hour": 15,
            "Minute": 30,
            "Month": 11,
            "Second": 0,
            "Year": 25
        }
    }
}
```

| フィールド         | 型       | 説明                                                                                 |
|--------------------|----------|--------------------------------------------------------------------------------------|
| `Type`             | 文字列   | `SuperCapacitor`                                                                     |
| `ManagerSysNo`     | 整数     | 管理システム番号                                                                     |
| `TotalVoltage`     | 整数     | 総電圧；単位：0.1 V                                                                 |
| `TotalCurrent`     | 整数     | 総電流；オフセット1000 A、単位：0.1 A                                              |
| `CellsTotal`       | 整数     | セル総数                                                                             |
| `CellsVoltage`     | 配列     | セル電圧リスト；単位：1 mV                                                          |
| `ProbeNum`         | 整数     | 温度プローブ数                                                                       |
| `ProbeTemp`        | 配列     | プローブ温度リスト；オフセット40℃、単位：1℃                                       |

#### スーパーキャパシタ極値データ

> **gbt32960-2025のみ**

```json
{
    "Cmd": 2,
    "Encrypt": 1,
    "Vin": "1G1BL52P7TR115520",
    "Data": {
        "Infos": [
            {
                "Type": "SuperCapacitorExtreme",
                "MaxVoltageManagerSysNo": 1,
                "MaxVoltageCellCode": 10,
                "MaxVoltageCellValue": 7500,
                "MinVoltageManagerSysNo": 2,
                "MinVoltageCellCode": 11,
                "MinVoltageCellValue": 2000,
                "MaxTempManagerSysNo": 3,
                "MaxTempProbeCode": 12,
                "MaxTempValue": 120,
                "MinTempManagerSysNo": 4,
                "MinTempProbeCode": 13,
                "MinTempValue": 40
            }
        ],
        "Time": {
            "Day": 13,
            "Hour": 15,
            "Minute": 30,
            "Month": 11,
            "Second": 0,
            "Year": 25
        }
    }
}
```

| フィールド                | 型       | 説明                                                                                 |
|---------------------------|----------|--------------------------------------------------------------------------------------|
| `Type`                   | 文字列   | `SuperCapacitorExtreme`                                                             |
| `MaxVoltageManagerSysNo` | 整数     | 最大電圧の管理システム番号                                                          |
| `MaxVoltageCellCode`     | 整数     | 最大電圧のセルコード                                                                 |
| `MaxVoltageCellValue`    | 整数     | 最大電圧値；単位：1 mV                                                              |
| `MinVoltageManagerSysNo` | 整数     | 最小電圧の管理システム番号                                                          |
| `MinVoltageCellCode`     | 整数     | 最小電圧のセルコード                                                                 |
| `MinVoltageCellValue`    | 整数     | 最小電圧値；単位：1 mV                                                              |
| `MaxTempManagerSysNo`    | 整数     | 最大温度の管理システム番号                                                          |
| `MaxTempProbeCode`       | 整数     | 最大温度のプローブコード                                                             |
| `MaxTempValue`           | 整数     | 最大温度値                                                                           |
| `MinTempManagerSysNo`    | 整数     | 最小温度の管理システム番号                                                          |
| `MinTempProbeCode`       | 整数     | 最小温度のプローブコード                                                             |
| `MinTempValue`           | 整数     | 最小温度値                                                                           |

#### デジタル署名

> **gbt32960-2025のみ**

```json
{
    "Cmd": 2,
    "Encrypt": 1,
    "Vin": "1G1BL52P7TR115520",
    "Data": {
        "Infos": [
            {
                "Type": "Signature",
                "SignatureType": 1,
                "RLength": 32,
                "RValue": "5256414C5256414C5256414C5256414C5256414C5256414C5256414C5256414C",
                "SLength": 32,
                "SValue": "5356414C5356414C5356414C5356414C5356414C5356414C5356414C5356414C"
            }
        ],
        "Time": {
            "Day": 13,
            "Hour": 15,
            "Minute": 30,
            "Month": 11,
            "Second": 0,
            "Year": 25
        }
    }
}
```

| フィールド         | 型       | 説明                                  |
|--------------------|----------|-------------------------------------|
| `Type`             | 文字列   | `Signature`                         |
| `SignatureType`    | 整数     | 署名タイプ                          |
| `RLength`          | 整数     | R値の長さ                          |
| `RValue`           | 文字列   | R値（16進エンコード文字列）        |
| `SLength`          | 整数     | S値の長さ                          |
| `SValue`           | 文字列   | S値（16進エンコード文字列）        |

### 過去データ再送信

トピック：`gbt32960/${vin}/upstream/reinfo`

データ形式：リアルタイムデータレポートと同じ。

### アクティベーション

> **gbt32960-2025のみ**

トピック：`gbt32960/${vin}/upstream/activation`

```json
{
    "Cmd": 9,
    "Encrypt": 1,
    "Vin": "VIN12345678901234",
    "Data": {
        "Time": {
            "Day": 1,
            "Hour": 12,
            "Minute": 0,
            "Month": 1,
            "Second": 0,
            "Year": 25
        },
        "ChipID": "CHIP123456789012",
        "PubKeyLen": 15,
        "PubKey": "5055424C49434B4559313233343536",
        "VIN": "VIN12345678901234",
        "Signature": {
            "SignatureType": 1,
            "RLength": 32,
            "RValue": "5252525252525252525252525252525252525252525252525252525252525252",
            "SLength": 32,
            "SValue": "5353535353535353535353535353535353535353535353535353535353535353"
        }
    }
}
```

| フィールド     | 型       | 説明                                         |
|----------------|----------|----------------------------------------------|
| `Cmd`         | 整数     | コマンド識別子。`9`はアクティベーションを意味 |
| `ChipID`      | 文字列   | 16バイトのチップ識別子                       |
| `PubKeyLen`   | 整数     | 公開鍵の長さ                                 |
| `PubKey`      | 文字列   | 公開鍵（16進エンコード）                      |
| `VIN`         | 文字列   | 車両識別番号                                 |
| `Signature`   | オブジェクト | デジタル署名（Signature情報タイプと同じ構造） |

## ダウンストリーム

リクエストフロー：EMQX → emqx_gbt32960 → 端末

レスポンスフロー：端末 → emqx_gbt32960 → EMQX

ダウンストリームトピック：`gbt32960/${vin}/dnstream`

アップストリームレスポンストピック：`gbt32960/${vin}/upstream/response`

### パラメータ照会

**リクエスト：**

```json
{
    "Action": "Query",
    "Total": 2,
    "Ids": ["0x01", "0x02"]
}
```

| フィールド | 型       | 説明                                           |
|------------|----------|------------------------------------------------|
| `Action`   | 文字列   | コマンド種別。この操作では`Query`               |
| `Total`    | 整数     | 照会するパラメータの総数                         |
| `Ids`      | 配列     | パラメータIDリスト。IDの意味はプロトコル表B.10参照 |

**レスポンス：**

```json
{
    "Cmd": 128,
    "Encrypt": 1,
    "Vin": "1G1BL52P7TR115520",
    "Data": {
        "Total": 2,
        "Params": [
            {"0x01": 6000},
            {"0x02": 10}
        ],
        "Time": {
            "Day": 2,
            "Hour": 11,
            "Minute": 12,
            "Month": 2,
            "Second": 12,
            "Year": 17
        }
    }
}
```

### パラメータ設定

**リクエスト：**

```json
{
    "Action": "Setting",
    "Total": 2,
    "Params": [{"0x01": 5000},
               {"0x02": 200}]
}
```

| フィールド | 型       | 説明                                           |
|------------|----------|------------------------------------------------|
| `Action`   | 文字列   | コマンド種別。この操作では`Setting`             |
| `Total`    | 整数     | 設定するパラメータの総数                         |
| `Params`   | 配列     | 設定するパラメータIDと値のリスト                 |

**レスポンス：**

```json
{
    "Cmd": 129,
    "Encrypt": 1,
    "Vin": "1G1BL52P7TR115520",
    "Data": {
        "Total": 2,
        "Params": [
            {"0x01": 5000},
            {"0x02": 200}
        ],
        "Time": {
            "Day": 2,
            "Hour": 11,
            "Minute": 12,
            "Month": 2,
            "Second": 12,
            "Year": 17
        }
    }
}
```

### 端末制御

コマンドによってパラメータが異なります。パラメータが不要なコマンドは`Param`フィールドを省略します。

**リモートアップグレードリクエスト：**

```json
{
    "Action": "Control",
    "Command": "0x01",
    "Param": {
        "DialingName": "hz203",
        "Username": "user001",
        "Password": "password01",
        "Ip": "192.168.199.1",
        "Port": 8080,
        "ManufacturerId": "BMWA",
        "HardwareVer": "1.0.0",
        "SoftwareVer": "1.0.0",
        "UpgradeUrl": "ftp://emqtt.io/ftp/server",
        "Timeout": 10
    }
}
```

| フィールド    | 型       | 説明                                             |
|--------------|----------|--------------------------------------------------|
| `Action`     | 文字列   | コマンド種別。この操作では`Control`               |
| `Command`    | 文字列   | コマンドID（プロトコル表B.15参照）               |
| `Param`      | オブジェクト | コマンドパラメータ（コマンドにより異なる。空の場合は省略） |

**車両端末シャットダウン**（`0x02`、パラメータなし）：

```json
{
    "Action": "Control",
    "Command": "0x02"
}
```

**車両端末アラーム**（`0x06`）：

```json
{
    "Action": "Control",
    "Command": "0x06",
    "Param": {"Level": 0, "Message": "alarm message"}
}
```
