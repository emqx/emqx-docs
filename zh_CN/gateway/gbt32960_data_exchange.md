# GB/T 32960 网关数据交换格式

GB/T 32960 网关（`emqx_gbt32960`）通过 MQTT 将符合 GB/T 32960 标准的电动汽车终端接入 EMQX。本文档定义该数据交换所使用的 JSON 消息格式。

## 协议版本支持

本网关同时支持 `gbt32960-2016` 和 `gbt32960-2025` 两个版本的协议，通过帧头自动识别协议版本：

- **gbt32960-2016**: 协议帧头为 `0x23 0x23`（ASCII `##`）
- **gbt32960-2025**: 协议帧头为 `0x24 0x24`（ASCII `$$`）

两个版本协议格式存在差异之处，本文档将逐一说明。

## 约定

- Payload 采用 JSON 格式进行组装
- JSON Key 采用大驼峰格式命名

## 上行数据（Upstream）

数据流向：Terminal → emqx_gbt32960 → EMQX

### 车辆登入

Topic: `gbt32960/${vin}/upstream/vlogin`

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

顶层字段含义：

| 字段      | 类型    | 描述                                                                                                                  |
|-----------|---------|-----------------------------------------------------------------------------------------------------------------------|
| `Cmd`     | Integer | 命令单元；`1` 表示车辆登入                                                                                            |
| `Encrypt` | Integer | 数据单元加密方式，`1` 表示不加密，`2` 数据经过 RSA 加密，`3` 数据经过 AES128 算法加密；`254` 表示异常；`255` 表示无效 |
| `Vin`     | String  | 唯一识别码，即车辆 VIN 码                                                                                             |
| `Data`    | Object  | 数据单元，JSON 对象格式                                                                                               |

`gbt32960-2016` 车辆登入的数据单元格式：

| 字段     | 类型    | 描述                                                          |
|----------|---------|---------------------------------------------------------------|
| `Time`   | Object  | 数据采集时间，按年、月、日、时、分、秒，格式见示例            |
| `Seq`    | Integer | 登入流水号                                                    |
| `ICCID`  | String  | 长度为 20 的字符串，SIM 卡的 ICCID 号                         |
| `Num`    | Integer | 可充电储能子系统数，有效值 0~250                              |
| `Length` | Integer | 可充电储能系统编码长度，有效值 0~50                           |
| `Id`     | String  | 可充电储能系统编码，长度为"子系统数"与"编码长度"值的乘积      |

`gbt32960-2025` 车辆登入的数据单元格式：

| 字段                   | 类型                 | 描述                                                               |
|------------------------|----------------------|--------------------------------------------------------------------|
| `Time`                 | Object               | 数据采集时间，按年、月、日、时、分、秒                             |
| `Seq`                  | Integer              | 登入流水号                                                         |
| `ICCID`                | String               | 长度为 20 的字符串，SIM 卡的 ICCID 号                              |
| `BmsNum`               | Integer              | 电池管理系统个数                                                   |
| `BatteryPackCounts`    | Array                | 每个电池管理系统所管理的电池包个数                                 |
| `BatteryPackEncodings` | Array\<Array\<String\>\> | 每个电池管理系统对应的电池包编码列表，每个编码为长度为 24 的字符串 |

### 车辆登出

Topic: `gbt32960/${vin}/upstream/vlogout`

车辆登出的 `Cmd` 值为 `4`，其余字段含义与登入相同：

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

### 实时信息上报

Topic: `gbt32960/${vin}/upstream/info`

不同信息类型上报，格式上只有 `Infos` 里面的对象属性不同，通过 `Type` 进行区分。`Infos` 为数组，代表车载终端每次报文可以上报多个信息。

#### 整车数据

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

其中，整车信息字段含义如下：

| 字段               | 类型    | 描述                                                                                                    |
|--------------------|---------|---------------------------------------------------------------------------------------------------------|
| `Type`             | String  | 数据类型，`Vehicle` 表示该结构为整车信息                                                                |
| `Status`           | Integer | 车辆状态，`1` 表示启动状态；`2` 表示熄火；`3` 表示其他状态；`254` 表示异常；`255` 表示无效             |
| `Charging`         | Integer | 充电状态，`1` 表示停车充电；`2` 行驶充电；`3` 未充电状态；`4` 充电完成；`254` 表示异常；`255` 表示无效  |
| `Mode`             | Integer | 运行模式，`1` 表示纯电；`2` 混动；`3` 燃油；`254` 表示异常；`255` 表示无效                              |
| `Speed`            | Integer | 车速，有效值 0~2200（表示 0~220.0 km/h），单位 0.1 km/h                                                  |
| `Mileage`          | Integer | 累计里程，有效值 0~9,999,999（表示 0~999,999.9 km），单位 0.1 km                                         |
| `Voltage`          | Integer | 总电压，有效值范围 0~10000（表示 0~1000 V），单位 0.1 V                                                  |
| `Current`          | Integer | 总电流，有效值 0~20000（偏移量 1000，表示 -1000 A~+1000 A），单位 0.1 A                                   |
| `SOC`              | Integer | SOC，有效值 0~100（表示 0%~100%）                                                                        |
| `DC`               | Integer | DC/DC 状态，`1` 工作；`2` 断开；`254` 表示异常；`255` 表示无效                                           |
| `Gear`             | Integer | 档位，参考原协议表 A.1，此值为其转换为整数的值                                                           |
| `Resistance`       | Integer | 绝缘电阻，有效范围 0~60000（表示 0~60000 kΩ）                                                            |
| `AcceleratorPedal` | Integer | **仅 gbt32960-2016**：加速踏板行程值，有效值 0~100                                                      |
| `BrakePedal`       | Integer | **仅 gbt32960-2016**：制动踏板状态，有效值 0~100                                                        |

#### 驱动电机数据

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

其中，驱动电机数据各个字段的含义是：

| 字段     | 类型    | 描述                             |
|----------|---------|----------------------------------|
| `Type`   | String  | 数据类型，此处为 `DriveMotor`    |
| `Number` | Integer | 驱动电机个数，有效值 1~253       |
| `Motors` | Array   | 驱动电机数据列表                 |

驱动电机数据字段为：

| 字段           | 类型    | 描述                                                                                                                                                                                              |
|----------------|---------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `No`           | Integer | 驱动电机序号，有效值 1~253                                                                                                                                                                        |
| `Status`       | Integer | 驱动电机状态，`1` 表示耗电；`2` 发电；`3` 关闭状态；`4` 准备状态；`254` 表示异常；`255` 表示无效                                                                                                 |
| `CtrlTemp`     | Integer | 驱动电机控制器温度，有效值 0~250（数值偏移 40°C，表示 -40°C~+210°C），单位 °C                                                                                                                     |
| `Rotating`     | Integer | 驱动电机转速，有效值 0~65531（数值偏移 20000，表示 -20000~45531 r/min），单位 1 r/min                                                                                                             |
| `Torque`       | Integer | **gbt32960-2016**：驱动电机转矩，有效值 0~65531（数据偏移量 20000，表示 -2000~4553.1 N·m），单位 0.1 N·m<br>**gbt32960-2025**：驱动电机转矩，有效值 0~429496729（数据偏移量 200000，表示 -20000~229496.729 N·m），单位 0.001 N·m |
| `MotorTemp`    | Integer | 驱动电机温度，有效值 0~250（数据偏移量 40°C，表示 -40°C~+210°C），单位 1°C                                                                                                                        |
| `InputVoltage` | Integer | **仅 gbt32960-2016**：电机控制器输入电压，有效值 0~60000（表示 0~6000 V），单位 0.1 V                                                                                                             |
| `DCBusCurrent` | Integer | **仅 gbt32960-2016**：电机控制器直流母线电流，有效值 0~20000（数值偏移 1000 A，表示 -1000~+1000 A），单位 0.1 A                                                                                    |

#### 燃料电池数据

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

其中，燃料电池数据各个字段的含义是：

| 字段                | 类型    | 描述                                                                                                     |
|---------------------|---------|----------------------------------------------------------------------------------------------------------|
| `Type`              | String  | 数据类型，此处为 `FuelCell`                                                                              |
| `CellVoltage`       | Integer | **仅 gbt32960-2016**：燃料电池电压，有效值范围 0~20000（表示 0~2000 V），单位 0.1 V                      |
| `CellCurrent`       | Integer | **仅 gbt32960-2016**：燃料电池电流，有效值范围 0~20000（表示 0~2000 A），单位 0.1 A                      |
| `FuelConsumption`   | Integer | **仅 gbt32960-2016**：燃料消耗率，有效值范围 0~60000（表示 0~600 kg/100km），单位 0.01 kg/100km           |
| `ProbeNum`          | Integer | **仅 gbt32960-2016**：燃料电池探针总数，有效值范围 0~65531                                               |
| `ProbeTemps`        | Array   | **仅 gbt32960-2016**：燃料电池每探针温度值                                                               |
| `H_MaxTemp`         | Integer | 氢系统最高温度，有效值 0~2400（偏移量 40°C，表示 -40°C~+200°C），单位 0.1°C                              |
| `H_TempProbeCode`   | Integer | 氢系统最高温度探针代号，有效值 1~252                                                                     |
| `H_MaxConc`         | Integer | 氢气最高浓度，有效值 0~60000（表示 0~50000 mg/kg），单位 1 mg/kg                                         |
| `H_ConcSensorCode`  | Integer | 氢气最高浓度传感器代号，有效值 1~252                                                                     |
| `H_MaxPress`        | Integer | 氢气最高压力，有效值 0~1000（表示 0~100 MPa），单位 0.1 MPa                                              |
| `H_PressSensorCode` | Integer | 氢气最高压力传感器代号，有效值 1~252                                                                     |
| `DCStatus`          | Integer | 高压 DC/DC 状态，`1` 表示工作；`2` 断开                                                                  |
| `RemainingH2`       | Integer | **仅 gbt32960-2025**：氢系统中氢气余量，单位 1 kg                                                        |
| `DCDCTemp`          | Integer | **仅 gbt32960-2025**：高压 DCDC 温度，偏移量 40°C，表示 -40°C~+210°C                                     |

#### 发动机数据

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

其中，发动机数据各个字段的含义是：

| 字段              | 类型    | 描述                                                                                              |
|-------------------|---------|---------------------------------------------------------------------------------------------------|
| `Type`            | String  | 数据类型，此处为 `Engine`                                                                         |
| `Status`          | Integer | **仅 gbt32960-2016**：发动机状态，`1` 表示启动；`2` 关闭                                          |
| `CrankshaftSpeed` | Integer | 曲轴转速，有效值 0~60000（表示 0~60000 r/min），单位 1 r/min                                      |
| `FuelConsumption` | Integer | **仅 gbt32960-2016**：燃料消耗率，有效范围 0~60000（表示 0~600 L/100km），单位 0.01 L/100km        |

#### 车辆位置数据

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

其中，车辆位置数据各个字段的含义是：

| 字段               | 类型    | 描述                                                   |
|--------------------|---------|--------------------------------------------------------|
| `Type`             | String  | 数据类型，此处为 `Location`                            |
| `Status`           | Integer | 定位状态，见原协议表 15，此处为所有比特位的整型值       |
| `CoordinateSystem` | Integer | **仅 gbt32960-2025**：坐标系，`1` WGS-84，`2` GCJ-02   |
| `Longitude`        | Integer | 经度，以度为单位的经度值乘以 10^6，精确到百万分之一度  |
| `Latitude`         | Integer | 纬度，以度为单位的纬度值乘以 10^6，精确到百万分之一度  |

#### 极值数据

> **仅 gbt32960-2016**

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

其中，极值数据各个字段的含义是：

| 字段                        | 类型    | 描述                                                           |
|-----------------------------|---------|----------------------------------------------------------------|
| `Type`                      | String  | 数据类型，此处为 `Extreme`                                     |
| `MaxVoltageBatterySubsysNo` | Integer | 最高电压电池子系统号，有效值 1~250                             |
| `MaxVoltageBatteryCode`     | Integer | 最高电压电池单体代号，有效值 1~250                             |
| `MaxBatteryVoltage`         | Integer | 电池单体电压最高值，有效值 0~15000（表示 0~15 V），单位 0.001 V |
| `MinVoltageBatterySubsysNo` | Integer | 最低电压电池子系统号，有效值 1~250                             |
| `MinVoltageBatteryCode`     | Integer | 最低电压电池单体代号，有效值 1~250                             |
| `MinBatteryVoltage`         | Integer | 电池单体电压最低值，有效值 0~15000（表示 0~15 V），单位 0.001 V |
| `MaxTempSubsysNo`           | Integer | 最高温度子系统号，有效值 1~250                                 |
| `MaxTempProbeNo`            | Integer | 最高温度探针序号，有效值 1~250                                 |
| `MaxTemp`                   | Integer | 最高温度值，有效值范围 0~250（偏移量 40，表示 -40°C~+210°C）   |
| `MinTempSubsysNo`           | Integer | 最低温度子系统号，有效值 1~250                                 |
| `MinTempProbeNo`            | Integer | 最低温度探针序号，有效值 1~250                                 |
| `MinTemp`                   | Integer | 最低温度值，有效值范围 0~250（偏移量 40，表示 -40°C~+210°C）   |

#### 报警数据

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

其中，报警数据各个字段的含义是：

| 字段                        | 类型    | 描述                                                                                                                                                  |
|-----------------------------|---------|-------------------------------------------------------------------------------------------------------------------------------------------------------|
| `Type`                      | String  | 数据类型，此处为 `Alarm`                                                                                                                              |
| `MaxAlarmLevel`             | Integer | 最高报警等级。**gbt32960-2016**：有效值范围 0~3，"0" 表示无故障，"1" 表示 1 级故障<br>**gbt32960-2025**：有效值范围 0~4，新增 "4" 表示热事件故障    |
| `GeneralAlarmFlag`          | Integer | 通用报警标志位，见原协议表 18                                                                                                                         |
| `FaultChargeableDeviceNum`  | Integer | 可充电储能装置故障总数，有效值 0~252                                                                                                                  |
| `FaultChargeableDeviceList` | Array   | 可充电储能装置故障代码列表                                                                                                                            |
| `FaultDriveMotorNum`        | Integer | 驱动电机故障总数，有效值范围 0~252                                                                                                                    |
| `FaultDriveMotorList`       | Array   | 驱动电机故障代码列表                                                                                                                                  |
| `FaultEngineNum`            | Integer | 发动机故障总数，有效值范围 0~252                                                                                                                      |
| `FaultEngineList`           | Array   | 发动机故障代码列表                                                                                                                                    |
| `FaultOthersNum`            | Integer | 其他故障总数                                                                                                                                          |
| `FaultOthersList`           | Array   | 其他故障代码列表                                                                                                                                      |
| `FaultGeneralNum`           | Integer | **仅 gbt32960-2025**：通用故障数量                                                                                                                    |
| `FaultGeneralList`          | Array   | **仅 gbt32960-2025**：通用故障列表，成员为 `{"No": integer, "Level": integer}` 的 JSON 对象                                                           |

#### 可充电储能装置电压数据

> **仅 gbt32960-2016**

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

其中，字段定义如下：

| 字段        | 类型    | 描述                                   |
|-------------|---------|----------------------------------------|
| `Type`      | String  | 数据类型，此处为 `ChargeableVoltage`   |
| `Number`    | Integer | 可充电储能子系统个数，有效范围 1~250   |
| `SubSystems` | Array  | 可充电储能子系统电压信息列表           |

可充电储能子系统电压信息数据格式：

| 字段                 | 类型    | 描述                                                                                               |
|----------------------|---------|----------------------------------------------------------------------------------------------------|
| `ChargeableSubsysNo` | Integer | 可充电储能子系统号，有效值范围 1~250                                                               |
| `ChargeableVoltage`  | Integer | 可充电储能装置电压，有效值范围 0~10000（表示 0~1000 V），单位 0.1 V                                 |
| `ChargeableCurrent`  | Integer | 可充电储能装置电流，有效值范围 0~20000（数值偏移量 1000 A，表示 -1000~+1000 A），单位 0.1 A          |
| `CellsTotal`         | Integer | 单体电池总数，有效值范围 1~65531                                                                    |
| `FrameCellsIndex`    | Integer | 本帧起始电池序号（从 1 开始），有效值范围 1~65531。当单体总数超过 200 时，需拆分为多帧传输。         |
| `FrameCellsCount`    | Integer | 本帧单体电池总数，有效值范围 1~200                                                                  |
| `CellsVoltage`       | Array   | 单体电池电压，有效值范围 0~60000（表示 0~60.000 V），单位 0.001 V                                   |

#### 可充电储能装置温度数据

> **仅 gbt32960-2016**

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

其中，数据格式为：

| 字段         | 类型    | 描述                              |
|--------------|---------|-----------------------------------|
| `Type`       | String  | 数据类型，此处为 `ChargeableTemp` |
| `Number`     | Integer | 可充电储能子系统温度信息列表长度  |
| `SubSystems` | Array   | 可充电储能子系统温度信息列表      |

可充电储能子系统温度信息格式为：

| 字段                 | 类型    | 描述                                    |
|----------------------|---------|-----------------------------------------|
| `ChargeableSubsysNo` | Integer | 可充电储能子系统号，有效值 1~250        |
| `ProbeNum`           | Integer | 可充电储能温度探针个数                  |
| `ProbesTemp`         | Array   | 可充电储能子系统各温度探针温度值列表    |

#### 动力电池包电压数据

> **仅 gbt32960-2025**

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

| 字段                        | 类型    | 描述                                         |
|-----------------------------|---------|----------------------------------------------|
| `Type`                      | String  | `MinVoltageOfPowerBattery`                   |
| `Number`                    | Integer | 动力电池包个数                               |
| `SubSystems`                | Array   | 动力电池包列表                               |
| `BatteryPackNo`             | Integer | 动力电池包序号                               |
| `BatteryPackVoltage`        | Integer | 动力电池包电压，单位 0.1 V                   |
| `BatteryPackCurrent`        | Integer | 动力电池包电流，偏移量 1000 A，单位 0.1 A    |
| `MinParallelUnitTotal`      | Integer | 最小并联单元总数                             |
| `MinParallelUnitVoltage`    | Array   | 最小并联单元电压列表，偏移量 4 V，单位 1 mV  |

#### 动力电池包温度数据

> **仅 gbt32960-2025**

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

| 字段            | 类型    | 描述                                    |
|-----------------|---------|-----------------------------------------|
| `Type`          | String  | `TempOfPowerBattery`                    |
| `Number`        | Integer | 动力电池包个数                          |
| `SubSystems`    | Array   | 动力电池包列表                          |
| `BatteryPackNo` | Integer | 动力电池包序号                          |
| `ProbeNum`      | Integer | 温度探针个数                            |
| `ProbesTemp`    | Array   | 温度探针列表，偏移量 40°C，单位 1°C     |

#### 燃料电池电堆数据

> **仅 gbt32960-2025**

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

| 字段               | 类型    | 描述                                    |
|--------------------|---------|------------------------------------------|
| `Type`             | String  | `FuelCellStack`                          |
| `Number`           | Integer | 燃料电池电堆个数                         |
| `Stacks`           | Array   | 燃料电池电堆列表                         |
| `FuelCellStackNo`  | Integer | 燃料电池电堆序号                         |
| `Voltage`          | Integer | 燃料电池电堆电压，单位 0.1 V             |
| `Current`          | Integer | 燃料电池电堆电流，单位 0.1 A             |
| `H2InletPressure`  | Integer | 氢气入口压力，单位 0.1 bar               |
| `AirInletPressure` | Integer | 空气入口压力，单位 0.1 bar               |
| `AirInletTemp`     | Integer | 空气入口温度，偏移量 40°C，单位 1°C      |
| `StackProbeNum`    | Integer | 电堆温度探针个数                         |
| `StackProbeTemp`   | Array   | 电堆温度探针温度列表                     |

#### 超级电容数据

> **仅 gbt32960-2025**

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

| 字段           | 类型    | 描述                                        |
|----------------|---------|---------------------------------------------|
| `Type`         | String  | `SuperCapacitor`                            |
| `ManagerSysNo` | Integer | 管理系统号                                  |
| `TotalVoltage` | Integer | 总电压，单位 0.1 V                          |
| `TotalCurrent` | Integer | 总电流，偏移量 1000 A，单位 0.1 A           |
| `CellsTotal`   | Integer | 单体总数                                    |
| `CellsVoltage` | Array   | 单体电压列表，单位 1 mV                     |
| `ProbeNum`     | Integer | 温度探针个数                                |
| `ProbeTemp`    | Array   | 温度探针温度列表，偏移量 40°C，单位 1°C     |

#### 超级电容极值数据

> **仅 gbt32960-2025**

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

| 字段                     | 类型    | 描述                      |
|--------------------------|---------|---------------------------|
| `Type`                   | String  | `SuperCapacitorExtreme`   |
| `MaxVoltageManagerSysNo` | Integer | 最高电压管理系统号        |
| `MaxVoltageCellCode`     | Integer | 最高电压单体代号          |
| `MaxVoltageCellValue`    | Integer | 最高电压值，单位 1 mV     |
| `MinVoltageManagerSysNo` | Integer | 最低电压管理系统号        |
| `MinVoltageCellCode`     | Integer | 最低电压单体代号          |
| `MinVoltageCellValue`    | Integer | 最低电压值，单位 1 mV     |
| `MaxTempManagerSysNo`    | Integer | 最高温度管理系统号        |
| `MaxTempProbeCode`       | Integer | 最高温度探针代号          |
| `MaxTempValue`           | Integer | 最高温度值                |
| `MinTempManagerSysNo`    | Integer | 最低温度管理系统号        |
| `MinTempProbeCode`       | Integer | 最低温度探针代号          |
| `MinTempValue`           | Integer | 最低温度值                |

#### 数字签名

> **仅 gbt32960-2025**

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

| 字段            | 类型    | 描述              |
|-----------------|---------|-------------------|
| `Type`          | String  | `Signature`       |
| `SignatureType` | Integer | 签名类型          |
| `RLength`       | Integer | R 值长度          |
| `RValue`        | String  | R 值（十六进制编码字符串） |
| `SLength`       | Integer | S 值长度          |
| `SValue`        | String  | S 值（十六进制编码字符串） |
### 数据补发

Topic: `gbt32960/${vin}/upstream/reinfo`

数据格式与实时信息上报相同。

### 激活

> **仅 gbt32960-2025**

Topic: `gbt32960/${vin}/upstream/activation`

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

| 字段          | 类型    | 描述                                          |
|---------------|---------|-----------------------------------------------|
| `Cmd`         | Integer | 命令单元；`9` 表示激活                        |
| `ChipID`      | String  | 16 字节芯片标识                               |
| `PubKeyLen`   | Integer | 公钥长度                                      |
| `PubKey`      | String  | 公钥（十六进制编码）                          |
| `VIN`         | String  | 车辆识别号                                    |
| `Signature`   | Object  | 数字签名（格式与签名信息类型相同）            |

## 下行数据（Downstream）

请求数据流向：EMQX → emqx_gbt32960 → Terminal

应答数据流向：Terminal → emqx_gbt32960 → EMQX

下行主题：`gbt32960/${vin}/dnstream`

上行应答主题：`gbt32960/${vin}/upstream/response`

### 参数查询

**请求（Req）：**

```json
{
    "Action": "Query",
    "Total": 2,
    "Ids": ["0x01", "0x02"]
}
```

| 字段     | 类型    | 描述                                               |
|----------|---------|----------------------------------------------------|
| `Action` | String  | 下发命令类型，此处为 `Query`                       |
| `Total`  | Integer | 查询参数总数                                       |
| `Ids`    | Array   | 需查询参数的 ID 列表，具体 ID 含义见原协议表 B.10  |

**应答（Response）：**

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

### 参数设置

**请求（Req）：**

```json
{
    "Action": "Setting",
    "Total": 2,
    "Params": [{"0x01": 5000},
               {"0x02": 200}]
}
```

| 字段     | 类型    | 描述                           |
|----------|---------|--------------------------------|
| `Action` | String  | 下发命令类型，此处为 `Setting` |
| `Total`  | Integer | 设置参数总数                   |
| `Params` | Array   | 需设置参数的 ID 和值           |

**应答（Response）：**

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

### 终端控制

命令不同，参数不同；无参数时省略 `Param` 字段。

**远程升级（`0x01`）请求：**

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

| 字段      | 类型   | 描述                                         |
|-----------|--------|----------------------------------------------|
| `Action`  | String | 下发命令类型，此处为 `Control`               |
| `Command` | String | 下发指令 ID，见原协议表 B.15                 |
| `Param`   | Object | 命令参数（根据命令不同而不同，无参数时省略） |

**车载终端关机**（`0x02`，无参数）：

```json
{
    "Action": "Control",
    "Command": "0x02"
}
```

**车载终端报警**（`0x06`）：

```json
{
    "Action": "Control",
    "Command": "0x06",
    "Param": {"Level": 0, "Message": "alarm message"}
}
```
