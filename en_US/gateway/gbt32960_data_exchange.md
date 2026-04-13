# GB/T 32960 Gateway Data Exchange Format

This page defines the format of data exchanged between **emqx_gbt32960** and **EMQX**.

## Protocol Version Support

The gateway supports both **gbt32960-2016** and **gbt32960-2025** protocol versions. The protocol version is automatically detected from the frame header:

- **gbt32960-2016**: Frame header is `0x23 0x23` (ASCII `##`)
- **gbt32960-2025**: Frame header is `0x24 0x24` (ASCII `$$`)

Where fields or behaviors differ between versions, this document notes those differences explicitly.

## Convention

- Payload is assembled in JSON format.
- JSON key names use UpperCamelCase (PascalCase).

## Upstream

Data flow: Terminal → emqx_gbt32960 → EMQX

### Vehicle Login

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

Top-level fields:

| Field     | Type    | Description                                                                                                                   |
|-----------|---------|-------------------------------------------------------------------------------------------------------------------------------|
| `Cmd`     | Integer | Command identifier; `1` means Vehicle Login                                                                                   |
| `Encrypt` | Integer | Data encryption method: `1` = no encryption, `2` = RSA, `3` = AES128; `254` = error; `255` = invalid                        |
| `Vin`     | String  | Vehicle Identification Number (VIN)                                                                                           |
| `Data`    | Object  | Data unit, JSON object format                                                                                                 |

`Data` fields for **gbt32960-2016**:

| Field    | Type    | Description                                                              |
|----------|---------|--------------------------------------------------------------------------|
| `Time`   | Object  | Data collection time (Year, Month, Day, Hour, Minute, Second)            |
| `Seq`    | Integer | Login sequence number                                                    |
| `ICCID`  | String  | 20-character SIM card ICCID                                              |
| `Num`    | Integer | Number of rechargeable energy storage sub-systems; valid range 0–250    |
| `Length` | Integer | Encoding length of rechargeable energy storage systems; valid range 0–50 |
| `Id`     | String  | System encodings; length = Num × Length                                  |

`Data` fields for **gbt32960-2025**:

| Field                  | Type                 | Description                                                                    |
|------------------------|----------------------|--------------------------------------------------------------------------------|
| `Time`                 | Object               | Data collection time (Year, Month, Day, Hour, Minute, Second)                  |
| `Seq`                  | Integer              | Login sequence number                                                          |
| `ICCID`                | String               | 20-character SIM card ICCID                                                    |
| `BmsNum`               | Integer              | Number of battery management systems                                           |
| `BatteryPackCounts`    | Array                | Number of battery packs managed by each BMS                                    |
| `BatteryPackEncodings` | Array\<Array\<String\>\> | Battery pack encoding lists per BMS; each encoding is a 24-character string |

### Vehicle Logout

Topic: `gbt32960/${vin}/upstream/vlogout`

The `Cmd` value is `4`. All other fields follow the same structure as Vehicle Login:

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

### Real-Time Data Report

Topic: `gbt32960/${vin}/upstream/info`

Each report can include multiple info items in the `Infos` array. The `Type` field distinguishes the info type.

#### Vehicle Data

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

| Field               | Type    | Description                                                                                                        |
|---------------------|---------|--------------------------------------------------------------------------------------------------------------------|
| `Type`              | String  | Info type; `Vehicle` for this structure                                                                            |
| `Status`            | Integer | Vehicle status: `1` = running; `2` = stalled; `3` = other; `254` = error; `255` = invalid                         |
| `Charging`          | Integer | Charge state: `1` = parked charging; `2` = driving charging; `3` = not charging; `4` = charge complete; `254` = error; `255` = invalid |
| `Mode`              | Integer | Operating mode: `1` = pure electric; `2` = hybrid; `3` = fuel; `254` = error; `255` = invalid                     |
| `Speed`             | Integer | Vehicle speed; valid range 0–2200 (0–220.0 km/h); unit: 0.1 km/h                                                  |
| `Mileage`           | Integer | Cumulative mileage; valid range 0–9,999,999 (0–999,999.9 km); unit: 0.1 km                                        |
| `Voltage`           | Integer | Total voltage; valid range 0–10000 (0–1000 V); unit: 0.1 V                                                        |
| `Current`           | Integer | Total current; valid range 0–20000 (offset 1000, representing -1000 A to +1000 A); unit: 0.1 A                    |
| `SOC`               | Integer | State of charge; valid range 0–100 (0%–100%)                                                                      |
| `DC`                | Integer | DC/DC status: `1` = working; `2` = disconnected; `254` = error; `255` = invalid                                   |
| `Gear`              | Integer | Gear position; integer representation of the gear table (see protocol Table A.1)                                   |
| `Resistance`        | Integer | Insulation resistance; valid range 0–60000 (0–60000 kΩ)                                                           |
| `AcceleratorPedal`  | Integer | **gbt32960-2016 only**: Accelerator pedal travel; valid range 0–100                                               |
| `BrakePedal`        | Integer | **gbt32960-2016 only**: Brake pedal state; valid range 0–100                                                      |

#### Drive Motor Data

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

| Field    | Type    | Description                                |
|----------|---------|--------------------------------------------|
| `Type`   | String  | Info type; `DriveMotor` for this structure |
| `Number` | Integer | Number of drive motors; valid range 1–253  |
| `Motors` | Array   | List of drive motor data                   |

Motor entry fields:

| Field          | Type    | Description                                                                                                                                                                        |
|----------------|---------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `No`           | Integer | Motor sequence number; valid range 1–253                                                                                                                                           |
| `Status`       | Integer | Motor status: `1` = consuming power; `2` = generating; `3` = off; `4` = ready; `254` = error; `255` = invalid                                                                     |
| `CtrlTemp`     | Integer | Controller temperature; valid range 0–250 (offset 40°C, -40°C to +210°C); unit: 1°C                                                                                              |
| `Rotating`     | Integer | Rotor speed; valid range 0–65531 (offset 20000, -20000 to 45531 r/min); unit: 1 r/min                                                                                             |
| `Torque`       | Integer | **gbt32960-2016**: motor torque; valid range 0–65531 (offset 20000, -2000 to 4553.1 N·m); unit: 0.1 N·m<br>**gbt32960-2025**: motor torque; valid range 0–429496729 (offset 200000, -20000 to 229496.729 N·m); unit: 0.001 N·m |
| `MotorTemp`    | Integer | Motor temperature; valid range 0–250 (offset 40°C, -40°C to +210°C); unit: 1°C                                                                                                   |
| `InputVoltage` | Integer | **gbt32960-2016 only**: Controller input voltage; valid range 0–60000 (0–6000 V); unit: 0.1 V                                                                                     |
| `DCBusCurrent` | Integer | **gbt32960-2016 only**: DC bus current; valid range 0–20000 (offset 1000 A, -1000 A to +1000 A); unit: 0.1 A                                                                      |

#### Fuel Cell Data

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

| Field               | Type    | Description                                                                                              |
|---------------------|---------|----------------------------------------------------------------------------------------------------------|
| `Type`              | String  | Info type; `FuelCell` for this structure                                                                 |
| `CellVoltage`       | Integer | **gbt32960-2016 only**: Fuel cell voltage; valid range 0–20000 (0–2000 V); unit: 0.1 V                  |
| `CellCurrent`       | Integer | **gbt32960-2016 only**: Fuel cell current; valid range 0–20000 (0–2000 A); unit: 0.1 A                  |
| `FuelConsumption`   | Integer | **gbt32960-2016 only**: Fuel consumption rate; valid range 0–60000 (0–600 kg/100km); unit: 0.01 kg/100km |
| `ProbeNum`          | Integer | **gbt32960-2016 only**: Total number of fuel cell probes; valid range 0–65531                            |
| `ProbeTemps`        | Array   | **gbt32960-2016 only**: Temperature values for each fuel cell probe                                      |
| `H_MaxTemp`         | Integer | Max hydrogen system temperature; valid range 0–2400 (offset 40°C, -40°C to +200°C); unit: 0.1°C         |
| `H_TempProbeCode`   | Integer | Probe code for max hydrogen temperature; valid range 1–252                                               |
| `H_MaxConc`         | Integer | Max hydrogen concentration; valid range 0–60000 (0–50000 mg/kg); unit: 1 mg/kg                          |
| `H_ConcSensorCode`  | Integer | Sensor code for max hydrogen concentration; valid range 1–252                                            |
| `H_MaxPress`        | Integer | Max hydrogen pressure; valid range 0–1000 (0–100 MPa); unit: 0.1 MPa                                    |
| `H_PressSensorCode` | Integer | Sensor code for max hydrogen pressure; valid range 1–252                                                 |
| `DCStatus`          | Integer | High-voltage DC/DC status: `1` = working; `2` = disconnected                                            |
| `RemainingH2`       | Integer | **gbt32960-2025 only**: Remaining hydrogen in the hydrogen system; unit: 1 kg                            |
| `DCDCTemp`          | Integer | **gbt32960-2025 only**: High-voltage DCDC temperature; offset 40°C, -40°C to +210°C                     |

#### Engine Data

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

| Field             | Type    | Description                                                                                        |
|-------------------|---------|----------------------------------------------------------------------------------------------------|
| `Type`            | String  | Info type; `Engine` for this structure                                                             |
| `Status`          | Integer | **gbt32960-2016 only**: Engine status: `1` = running; `2` = off                                   |
| `CrankshaftSpeed` | Integer | Crankshaft speed; valid range 0–60000 (0–60000 r/min); unit: 1 r/min                              |
| `FuelConsumption` | Integer | **gbt32960-2016 only**: Fuel consumption rate; valid range 0–60000 (0–600 L/100km); unit: 0.01 L/100km |

#### Vehicle Location Data

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

| Field              | Type    | Description                                                        |
|--------------------|---------|--------------------------------------------------------------------|
| `Type`             | String  | Info type; `Location` for this structure                           |
| `Status`           | Integer | Position status; integer value of all status bits (see protocol Table 15) |
| `CoordinateSystem` | Integer | **gbt32960-2025 only**: Coordinate system: `1` = WGS-84; `2` = GCJ-02 |
| `Longitude`        | Integer | Longitude in degrees × 10^6; accurate to one millionth of a degree |
| `Latitude`         | Integer | Latitude in degrees × 10^6; accurate to one millionth of a degree |

#### Extreme Value Data

> **gbt32960-2016 only**

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

| Field                       | Type    | Description                                                           |
|-----------------------------|---------|-----------------------------------------------------------------------|
| `Type`                      | String  | Info type; `Extreme` for this structure                               |
| `MaxVoltageBatterySubsysNo` | Integer | Sub-system number with highest battery voltage; valid range 1–250     |
| `MaxVoltageBatteryCode`     | Integer | Battery cell code with highest voltage; valid range 1–250             |
| `MaxBatteryVoltage`         | Integer | Highest individual cell voltage; valid range 0–15000 (0–15 V); unit: 0.001 V |
| `MinVoltageBatterySubsysNo` | Integer | Sub-system number with lowest battery voltage; valid range 1–250      |
| `MinVoltageBatteryCode`     | Integer | Battery cell code with lowest voltage; valid range 1–250              |
| `MinBatteryVoltage`         | Integer | Lowest individual cell voltage; valid range 0–15000 (0–15 V); unit: 0.001 V |
| `MaxTempSubsysNo`           | Integer | Sub-system number with highest temperature; valid range 1–250         |
| `MaxTempProbeNo`            | Integer | Probe number with highest temperature; valid range 1–250              |
| `MaxTemp`                   | Integer | Highest temperature; valid range 0–250 (offset 40, -40°C to +210°C)  |
| `MinTempSubsysNo`           | Integer | Sub-system number with lowest temperature; valid range 1–250          |
| `MinTempProbeNo`            | Integer | Probe number with lowest temperature; valid range 1–250               |
| `MinTemp`                   | Integer | Lowest temperature; valid range 0–250 (offset 40, -40°C to +210°C)   |

#### Alarm Data

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

| Field                       | Type    | Description                                                                                                                                |
|-----------------------------|---------|--------------------------------------------------------------------------------------------------------------------------------------------|
| `Type`                      | String  | Info type; `Alarm` for this structure                                                                                                      |
| `MaxAlarmLevel`             | Integer | Highest alarm level. **gbt32960-2016**: valid range 0–3 ("0" = no fault, "1" = level 1 fault).<br>**gbt32960-2025**: valid range 0–4, new value "4" = thermal event fault |
| `GeneralAlarmFlag`          | Integer | General alarm flag bits (see protocol Table 18)                                                                                            |
| `FaultChargeableDeviceNum`  | Integer | Total faults in rechargeable energy storage devices; valid range 0–252                                                                     |
| `FaultChargeableDeviceList` | Array   | Fault code list for rechargeable energy storage devices                                                                                    |
| `FaultDriveMotorNum`        | Integer | Total drive motor faults; valid range 0–252                                                                                                |
| `FaultDriveMotorList`       | Array   | Drive motor fault code list                                                                                                                |
| `FaultEngineNum`            | Integer | Total engine faults; valid range 0–252                                                                                                     |
| `FaultEngineList`           | Array   | Engine fault code list                                                                                                                     |
| `FaultOthersNum`            | Integer | Total other faults                                                                                                                         |
| `FaultOthersList`           | Array   | Other fault code list                                                                                                                      |
| `FaultGeneralNum`           | Integer | **gbt32960-2025 only**: Total general fault count                                                                                          |
| `FaulGeneralList`           | Array   | **gbt32960-2025 only**: General fault list; each entry is a JSON object `{"No": integer, "Level": integer}`                                |

#### Rechargeable Energy Storage Voltage Data

> **gbt32960-2016 only**

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

| Field      | Type    | Description                                           |
|------------|---------|-------------------------------------------------------|
| `Type`     | String  | Info type; `ChargeableVoltage` for this structure     |
| `Number`   | Integer | Number of rechargeable energy storage sub-systems; valid range 1–250 |
| `SubSystems` | Array | List of sub-system voltage data                       |

Sub-system voltage fields:

| Field                | Type    | Description                                                                                          |
|----------------------|---------|------------------------------------------------------------------------------------------------------|
| `ChargeableSubsysNo` | Integer | Sub-system number; valid range 1–250                                                                 |
| `ChargeableVoltage`  | Integer | Sub-system voltage; valid range 0–10000 (0–1000 V); unit: 0.1 V                                     |
| `ChargeableCurrent`  | Integer | Sub-system current; valid range 0–20000 (offset 1000 A, -1000 A to +1000 A); unit: 0.1 A            |
| `CellsTotal`         | Integer | Total number of battery cells; valid range 1–65531                                                   |
| `FrameCellsIndex`    | Integer | Starting cell index in this frame; split into multiple frames if count exceeds 200; valid range 1–65531 |
| `FrameCellsCount`    | Integer | Number of cells in this frame; valid range 1–200                                                     |
| `CellsVoltage`       | Array   | Individual cell voltages; valid range 0–60000 (0–60.000 V); unit: 0.001 V                           |

#### Rechargeable Energy Storage Temperature Data

> **gbt32960-2016 only**

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

| Field        | Type    | Description                                           |
|--------------|---------|-------------------------------------------------------|
| `Type`       | String  | Info type; `ChargeableTemp` for this structure        |
| `Number`     | Integer | Number of sub-systems in the temperature info list    |
| `SubSystems` | Array   | List of sub-system temperature data                   |

Sub-system temperature fields:

| Field                | Type    | Description                                         |
|----------------------|---------|-----------------------------------------------------|
| `ChargeableSubsysNo` | Integer | Sub-system number; valid range 1–250                |
| `ProbeNum`           | Integer | Number of temperature probes in this sub-system     |
| `ProbesTemp`         | Array   | Temperature readings for each probe                 |

#### Power Battery Pack Voltage Data

> **gbt32960-2025 only**

| Field                       | Type    | Description                                                  |
|-----------------------------|---------|--------------------------------------------------------------|
| `Type`                      | String  | `MinVoltageOfPowerBattery`                                   |
| `Number`                    | Integer | Number of power battery packs                                |
| `SubSystems`                | Array   | List of battery packs                                        |
| `BatteryPackNo`             | Integer | Battery pack sequence number                                 |
| `BatteryPackVoltage`        | Integer | Battery pack voltage; unit: 0.1 V                            |
| `BatteryPackCurrent`        | Integer | Battery pack current; offset 1000 A; unit: 0.1 A             |
| `MinParallelUnitTotal`      | Integer | Total number of minimum parallel units                       |
| `FrameMinParallelUnitIndex` | Integer | Starting minimum parallel unit index in this frame           |
| `FrameMinParallelUnitCount` | Integer | Number of minimum parallel units in this frame               |
| `MinParallelUnitVoltage`    | Array   | Voltage list for minimum parallel units; offset 4 V; unit: 1 mV |

#### Power Battery Pack Temperature Data

> **gbt32960-2025 only**

| Field           | Type    | Description                                          |
|-----------------|---------|------------------------------------------------------|
| `Type`          | String  | `TempOfPowerBattery`                                 |
| `Number`        | Integer | Number of power battery packs                        |
| `SubSystems`    | Array   | List of battery packs                                |
| `BatteryPackNo` | Integer | Battery pack sequence number                         |
| `ProbeNum`      | Integer | Number of temperature probes                         |
| `ProbesTemp`    | Array   | Probe temperature list; offset 40°C; unit: 1°C       |

#### Fuel Cell Stack Data

> **gbt32960-2025 only**

| Field              | Type    | Description                                     |
|--------------------|---------|-------------------------------------------------|
| `Type`             | String  | `FuelCellStack`                                 |
| `Number`           | Integer | Number of fuel cell stacks                      |
| `Stacks`           | Array   | List of fuel cell stacks                        |
| `FuelCellStackNo`  | Integer | Fuel cell stack sequence number                 |
| `Voltage`          | Integer | Fuel cell stack voltage; unit: 0.1 V            |
| `Current`          | Integer | Fuel cell stack current; unit: 0.1 A            |
| `H2InletPressure`  | Integer | Hydrogen inlet pressure; unit: 0.1 bar          |
| `AirInletPressure` | Integer | Air inlet pressure; unit: 0.1 bar               |
| `AirInletTemp`     | Integer | Air inlet temperature; offset 40°C; unit: 1°C   |
| `StackProbeNum`    | Integer | Number of stack temperature probes              |
| `StackProbeTemp`   | Array   | Stack temperature probe readings                |

#### Supercapacitor Data

> **gbt32960-2025 only**

| Field          | Type    | Description                                        |
|----------------|---------|----------------------------------------------------|
| `Type`         | String  | `SuperCapacitor`                                   |
| `ManagerSysNo` | Integer | Management system number                           |
| `TotalVoltage` | Integer | Total voltage; unit: 0.1 V                         |
| `TotalCurrent` | Integer | Total current; offset 1000 A; unit: 0.1 A          |
| `CellsTotal`   | Integer | Total number of cells                              |
| `CellsVoltage` | Array   | Cell voltage list; unit: 1 mV                      |
| `Temp`         | Integer | Temperature; offset 40°C; unit: 1°C                |
| `ProbeNum`     | Integer | Number of temperature probes                       |
| `ProbeTemp`    | Array   | Probe temperature list; offset 40°C; unit: 1°C     |

#### Supercapacitor Extreme Value Data

> **gbt32960-2025 only**

| Field                    | Type    | Description                         |
|--------------------------|---------|-------------------------------------|
| `Type`                   | String  | `SuperCapacitorExtreme`             |
| `MaxVoltageManagerSysNo` | Integer | Management system number with max voltage |
| `MaxVoltageCellCode`     | Integer | Cell code with max voltage          |
| `MaxVoltageCellValue`    | Integer | Max voltage value; unit: 1 mV       |
| `MinVoltageManagerSysNo` | Integer | Management system number with min voltage |
| `MinVoltageCellCode`     | Integer | Cell code with min voltage          |
| `MinVoltageCellValue`    | Integer | Min voltage value; unit: 1 mV       |
| `MaxTempManagerSysNo`    | Integer | Management system number with max temperature |
| `MaxTempProbeCode`       | Integer | Probe code with max temperature     |
| `MaxTempValue`           | Integer | Max temperature value               |
| `MinTempManagerSysNo`    | Integer | Management system number with min temperature |
| `MinTempProbeCode`       | Integer | Probe code with min temperature     |
| `MinTempValue`           | Integer | Min temperature value               |

#### Digital Signature

> **gbt32960-2025 only**

| Field           | Type    | Description         |
|-----------------|---------|---------------------|
| `Type`          | String  | `Signature`         |
| `SignatureType` | Integer | Signature type      |
| `RLength`       | Integer | Length of R value   |
| `RValue`        | Binary  | R value (hex string) |
| `SLength`       | Integer | Length of S value   |
| `SValue`        | Binary  | S value (hex string) |

### Historical Data Retransmission

Topic: `gbt32960/${vin}/upstream/reinfo`

Data format: same as Real-Time Data Report.

## Downstream

Request flow: EMQX → emqx_gbt32960 → Terminal

Response flow: Terminal → emqx_gbt32960 → EMQX

Downstream topic: `gbt32960/${vin}/dnstream`

Upstream response topic: `gbt32960/${vin}/upstream/response`

### Parameter Query

**Request:**

```json
{
    "Action": "Query",
    "Total": 2,
    "Ids": ["0x01", "0x02"]
}
```

| Field    | Type    | Description                                                    |
|----------|---------|----------------------------------------------------------------|
| `Action` | String  | Command type; `Query` for this operation                       |
| `Total`  | Integer | Total number of parameters to query                            |
| `Ids`    | Array   | List of parameter IDs; see protocol Table B.10 for ID meanings |

**Response:**

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

### Parameter Setting

**Request:**

```json
{
    "Action": "Setting",
    "Total": 2,
    "Params": [{"0x01": 5000},
               {"0x02": 200}]
}
```

| Field    | Type    | Description                                       |
|----------|---------|---------------------------------------------------|
| `Action` | String  | Command type; `Setting` for this operation        |
| `Total`  | Integer | Total number of parameters to set                 |
| `Params` | Array   | List of parameter IDs and their values to set     |

**Response:**

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

### Terminal Control

Different commands carry different parameters; commands with no parameters omit the `Param` field.

**Remote Upgrade request:**

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

| Field     | Type   | Description                                                  |
|-----------|--------|--------------------------------------------------------------|
| `Action`  | String | Command type; `Control` for this operation                   |
| `Command` | String | Command ID (see protocol Table B.15)                         |
| `Param`   | Object | Command parameters (varies by command; omitted when empty)   |

**Vehicle terminal shutdown** (`0x02`, no parameters):

```json
{
    "Action": "Control",
    "Command": "0x02"
}
```

**Vehicle terminal alarm** (`0x06`):

```json
{
    "Action": "Control",
    "Command": "0x06",
    "Param": {"Level": 0, "Message": "alarm message"}
}
```
