# JT/T 808 Gateway Data Exchange Format

This page defines the format of data exchange between **emqx_jt808** and **EMQX**.

## Protocol Version Support

The gateway supports both **JT/T 808-2013** and **JT/T 808-2019** protocol versions.
 The protocol version is automatically determined based on the version flag in the message header:

- **2013 version**: Message header property bit14 = 0
  - Terminal phone number: BCD[6] (12 digits)
  - Message header length: 12 / 16 bytes
- **2019 version**: Message header property bit14 = 1
  - Terminal phone number: BCD[10] (20 digits)
  - Message header length: 17 / 21 bytes

In JSON-based data exchange, messages using the 2019 version include a `proto_ver` field in the `header` to indicate the protocol version.

## Convention

- Payload is assembled in JSON format.
- JSON Key names are in lowercase.

## JSON Structure Example

### Terminal to Server

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

### Server to Terminal

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

## Data Type Correspondence Table

| JT808 Defined Type | In JSON Type |  Comment   |
| :----------------: | :----------: | :--------: |
|        BYTE        |   integer    | in decimal |
|        WORD        |   integer    | in decimal |
|       DWORD        |   integer    | in decimal |
|      BYTE(n)       |    string    |            |
|       BCD(n)       |    string    |            |
|       STRING       |    string    |            |

## Field Mapping Table

### Message Header Fields

#### JT/T 808-2013 Message Header

|       Field        | JSON Key | Value Type | Value Type in JSON |
| :----------------: | :------: | :--------: | :----------------: |
|     Message ID     |  msg_id  |    word    |      integer       |
| Encryption Method  | encrypt  |    word    |      integer       |
|   Terminal Phone   |  phone   |   bcd(6)   |       string       |
| Message Serial No. |  msg_sn  |    word    |      integer       |

Optional fields:

|      Field      |  JSON Key  | Value Type | Value Type in JSON |
| :-------------: | :--------: | :--------: | :----------------: |
| Total Fragments | frag_total |    word    |      integer       |
| Fragment Index  |  frag_sn   |    word    |      integer       |

#### JT/T 808-2019 Message Header

|       Field        | JSON Key  | Value Type | Value Type in JSON |
| :----------------: | :-------: | :--------: | :----------------: |
|     Message ID     |  msg_id   |    word    |      integer       |
| Encryption Method  |  encrypt  |    word    |      integer       |
|  Protocol Version  | proto_ver |    byte    |      integer       |
|   Terminal Phone   |   phone   |  bcd(10)   |       string       |
| Message Serial No. |  msg_sn   |    word    |      integer       |

Optional fields:

|      Field      |  JSON Key  | Value Type | Value Type in JSON |
| :-------------: | :--------: | :--------: | :----------------: |
| Total Fragments | frag_total |    word    |      integer       |
| Fragment Index  |  frag_sn   |    word    |      integer       |

**Notes**:

- `proto_ver` exists only in JT/T 808-2019. The current version value is `1`.
- If `frag_total` and `frag_sn` are present, the message body is a fragmented long message.

### Message Body Fields

#### General Response from Terminal `"msg_id": 1` 0x0001

|         Field          | JSON Key | Value Type | Value Type in JSON |
| :--------------------: | :------: | :--------: | :----------------: |
| Response Serial Number |   seq    |    word    |      integer       |
|      Response ID       |    id    |    word    |      integer       |
|         Result         |  result  |    byte    |      integer       |

#### General Response from Platform `"msg_id": 32769` 0x8001

|         Field          | JSON Key | Value Type | Value Type in JSON |
| :--------------------: | :------: | :--------: | :----------------: |
| Response Serial Number |   seq    |    word    |      integer       |
|      Response ID       |    id    |    word    |      integer       |
|         Result         |  result  |    byte    |      integer       |

#### Terminal Heartbeat `"msg_id": 2` 0x0002

Empty JSON

#### Re-transmission Request for Sub-package `"msg_id": 32771` 0x8003

##### JT/T 808-2013

|                  Field                   | JSON Key |   Value Type   | Value Type in JSON |
| :--------------------------------------: | :------: | :------------: | :----------------: |
|      Original Message Serial Number      |   seq    |      word      |      integer       |
| Total Number of Re-transmission Packages |  length  |      byte      |      integer       |
|     Re-transmission Package ID List      |   ids    | byte(2*length) |  list of integer   |

##### JT/T 808-2019

|                  Field                   | JSON Key |   Value Type   | Value Type in JSON |
| :--------------------------------------: | :------: | :------------: | :----------------: |
|      Original Message Serial Number      |   seq    |      word      |      integer       |
| Total Number of Re-transmission Packages |  length  |      word      |      integer       |
|     Re-transmission Package ID List      |   ids    | byte(2*length) |  list of integer   |

**Note**: The 2019 version changed the total number of re-transmission packages from BYTE to WORD.

#### Query Server Time Response `"msg_id": 32772` (0x8004, Added in 2019)

| Field  | JSON Key | Value Type | Value Type in JSON |
| :----: | :------: | :--------: | :----------------: |
|  Year  |   year   |    word    |      integer       |
| Month  |  month   |    byte    |      integer       |
|  Day   |   day    |    byte    |      integer       |
|  Hour  |   hour   |    byte    |      integer       |
| Minute |  minute  |    byte    |      integer       |
| Second |  second  |    byte    |      integer       |

#### Terminal Registration `"msg_id": 256` 0x0100

##### JT/T 808-2013

|         Field          |    JSON Key    | Value Type | Value Type in JSON |
| :--------------------: | :------------: | :--------: | :----------------: |
|      Province ID       |    province    |    word    |      integer       |
|        City ID         |      city      |    word    |      integer       |
|    Manufacturer ID     |  manufacturer  |  byte(5)   |       string       |
|     Terminal Model     |     model      |  byte(20)  |       string       |
|      Terminal ID       |     dev_id     |  byte(7)   |       string       |
|     Vehicle Color      |     color      |    byte    |      integer       |
| Vehicle Identification | license_number |   string   |       string       |

##### JT/T 808-2019

|         Field          |    JSON Key    | Value Type | Value Type in JSON |
| :--------------------: | :------------: | :--------: | :----------------: |
|      Province ID       |    province    |    word    |      integer       |
|        City ID         |      city      |    word    |      integer       |
|    Manufacturer ID     |  manufacturer  |  byte(11)  |       string       |
|     Terminal Model     |     model      |  byte(30)  |       string       |
|      Terminal ID       |     dev_id     |  byte(30)  |       string       |
|     Vehicle Color      |     color      |    byte    |      integer       |
| Vehicle Identification | license_number |   string   |       string       |

**Note**: The 2019 version extends the field lengths of the Manufacturer ID (from 5 to 11 bytes), Terminal Model (from 20 to 30 bytes), and Terminal ID (from 7 to 30 bytes).

#### Terminal Registration Response `"msg_id": 33024` 0x8100

|         Field          | JSON Key | Value Type | Value Type in JSON |
| :--------------------: | :------: | :--------: | :----------------: |
| Response Serial Number |   seq    |    word    |      integer       |
|         Result         |  result  |    byte    |      integer       |

This field is only present after a successful registration:

|   Optional Field    | JSON Key  | Value Type | Value Type in JSON |
| :-----------------: | --------- | ---------- | ------------------ |
| Authentication Code | auth_code | string     | string             |

#### Terminal Logout `"msg_id": 3` 0x0003

Empty JSON

#### Query Server Time Request `"msg_id": 4` 0x0004 (Added in 2019)

Empty JSON

#### Terminal Retransmission Sub-packet Request `"msg_id": 5` 0x0005 (Added in 2019)

|          Field           | JSON Key |   Value Type   | Value Type in JSON |
| :----------------------: | :------: | :------------: | :----------------: |
|   Original Message SN    |   seq    |      word      |      integer       |
|  Total Retransmit Count  |  length  |      word      |      integer       |
| Retransmitted Packet IDs |   ids    | byte(2*length) |  list of integer   |


#### Terminal Authentication `"msg_id": 258` 0x0102

##### JT/T 808-2013

| Field               | JSON Key | Value Type | Value Type in JSON |
| ------------------- | -------- | ---------- | ------------------ |
| Authentication Code | code     | string     | string             |

##### JT/T 808-2019

|           Field            | JSON Key name  | Value Type | Value Type in JSON |
| :------------------------: | :------------: | :--------: | :----------------: |
| Authentication code length |    code_len    |    byte    |      integer       |
|    Authentication code     |      code      |   string   |       string       |
|       Terminal IMEI        |      imei      |  byte(15)  |       string       |
|  Software version length   | sw_version_len |    byte    |      integer       |
|      Software version      |   sw_version   |   string   |       string       |

**Note**: The 2019 version adds fields such as authentication code length, terminal IMEI, and software version information.

#### Set Terminal Parameters `"msg_id": 33027”` 0x8103

|           Field            | JSON Key |                          Value Type                          |                      Value Type in JSON                      |
| :------------------------: | :------: | :----------------------------------------------------------: | :----------------------------------------------------------: |
| Total Number of Parameters |  length  |                             byte                             |                           integer                            |
|    Parameter Item List     |  params  |                             list                             |    list of id and value. `[{"id":ID, "value": VAL}, ...]`    |
|       Parameter Item       |    id    |                            dword                             |                           integer                            |
|      Parameter Value       |  value   | See [Parameter Value Type Mapping](#parameter-value-type-mapping) | See [Parameter Value Type Mapping](#parameter-value-type-mapping) |

Explanation of parameter IDs as per protocol.

##### Parameter Value Type Mapping

The JSON type of a parameter value is determined by the data type corresponding to the parameter ID:

|  Parameter ID Range  | Data Type |   Value Type in JSON    |                   Description                    |
| :------------------: | :-------: | :---------------------: | :----------------------------------------------: |
|   BYTE-type params   |   byte    |         integer         |            e.g. 0x0084, 0x0090–0x0095            |
|   WORD-type params   |   word    |         integer         |            e.g. 0x0031, 0x0081–0x0082            |
|  DWORD-type params   |   dword   |         integer         |        e.g. 0x0001–0x0007, 0x0018–0x001C         |
|  STRING-type params  |  string   |         string          |    e.g. 0x0010–0x0017, 0x0040–0x0049, 0x0083     |
|    0x0110–0x01FF     |  byte[8]  | string (base64 encoded) |  CAN bus ID independent acquisition parameters   |
| Other unknown params |  byte[n]  | string (base64 encoded) | Reserved parameters or vendor-defined parameters |

Example:

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

#### Query Terminal Parameters `"msg_id": 33028”` 0x8104

Empty JSON

#### Query Specific Terminal Parameters `"msg_id": 33030”` 0x8106

|           Field            | JSON Key |   Value Type   |       Value Type in JSON        |
| :------------------------: | :------: | :------------: | :-----------------------------: |
| Total Number of Parameters |  length  |      byte      |             integer             |
|     Parameter ID List      |   ids    | byte(2*length) | list of id. `[1, 2, 3, 4, ...]` |

Elements in the parameter ID list are integers.

#### Query Terminal Response Parameters `"msg_id": 260”` 0x0104

|             Field             | JSON Key | Value Type |                      Value Type in JSON                      |
| :---------------------------: | :------: | :--------: | :----------------------------------------------------------: |
|    Response Serial Number     |   seq    |    word    |                           integer                            |
| Number of Response Parameters |  length  |    byte    |                           integer                            |
|      Parameter Item List      |  params  |    list    |    list of id and value. `[{"id":ID, "value": VAL}, ...]`    |
|        Parameter Item         |    id    |   dword    |                           integer                            |
|        Parameter Value        |  value   |     --     | See [0x8103 Parameter Value Type Mapping](#parameter-value-type-mapping) |

Explanation of parameter IDs as per protocol.

#### Terminal Control `"msg_id": 33029”` 0x8105

|       Field       | JSON Key | Value Type | Value Type in JSON |
| :---------------: | :------: | :--------: | :----------------: |
|   Command Word    | command  |    byte    |      integer       |
| Command Parameter |  param   |   string   |       string       |

#### Query Terminal Properties `"msg_id": 33031”` 0x8107

Empty JSON

#### Query Terminal Property Response `"msg_id": 263”` 0x0107

##### JT/T 808-2013

|              Field               |     JSON Key     | Value Type | Value Type in JSON |
| :------------------------------: | :--------------: | :--------: | :----------------: |
|          Terminal Type           |       type       |    word    |      integer       |
|         Manufacturer ID          |   manufacturer   |  byte(5)   |       string       |
|          Terminal Model          |      model       |  byte(20)  |       string       |
|           Terminal ID            |        id        |  byte(7)   |       string       |
|     Terminal SIM Card ICCID      |      iccid       |  byte(10)  |       string       |
| Terminal Hardware Version Number | hardware_version |   string   |       string       |
|     Terminal Firmware Number     | firmware_version |   string   |       string       |
|      GNSS Module Properties      |    gnss_prop     |    byte    |      integer       |
| Communication Module Properties  |    comm_prop     |    byte    |      integer       |

##### JT/T 808-2019

|              Field               |     JSON Key     | Value Type | Value Type in JSON |
| :------------------------------: | :--------------: | :--------: | :----------------: |
|          Terminal Type           |       type       |    word    |      integer       |
|         Manufacturer ID          |   manufacturer   |  byte(11)  |       string       |
|          Terminal Model          |      model       |  byte(30)  |       string       |
|           Terminal ID            |        id        |  byte(30)  |       string       |
|     Terminal SIM Card ICCID      |      iccid       |  byte(10)  |       string       |
| Terminal Hardware Version Number | hardware_version |   string   |       string       |
|     Terminal Firmware Number     | firmware_version |   string   |       string       |
|      GNSS Module Properties      |    gnss_prop     |    byte    |      integer       |
| Communication Module Properties  |    comm_prop     |    byte    |      integer       |

**Note**:

- The 2019 version extends the field lengths of the Manufacturer ID (from 5 to 11 bytes), Terminal Model (from 20 to 30 bytes), and Terminal ID (from 7 to 30 bytes).
- A new bit8 is added to the terminal type to indicate support for / connection to a trailer.

- The length of the terminal hardware version number and terminal firmware number will be used for binary message parsing and are not exposed.

#### Terminal Upgrade Package Command `"msg_id": 33032` 0x8108

|         Field          |   JSON Key   | Value Type |   Value Type in JSON   |
| :--------------------: | :----------: | :--------: | :--------------------: |
|      Upgrade Type      |     type     |    byte    |        integer         |
|    Manufacturer ID     | manufacturer |  byte(5)   |         string         |
| Version Number Length  |   ver_len    |    byte    |        integer         |
|     Version Number     |   version    |   string   |         string         |
| Upgrade Package Length |    fw_len    |   dword    |        integer         |
|    Upgrade Package     |   firmware   |   binary   | string(base64 encoded) |

#### Terminal Upgrade Result Notification `"msg_id": 264` 0x0108

|     Field      | JSON Key | Value Type | Value Type in JSON |
| :------------: | :------: | :--------: | :----------------: |
|  Upgrade Type  |   type   |    byte    |      integer       |
| Upgrade Result |  result  |    byte    |      integer       |

#### Location Information Report `"msg_id": 512` 0x0200

|   Field    | JSON Key  | Value Type | Value Type in JSON |
| :--------: | :-------: | :--------: | :----------------: |
| Alarm Flag |   alarm   |   dword    |      integer       |
|   Status   |  status   |   dword    |      integer       |
|  Latitude  | latitude  |   dword    |      integer       |
| Longitude  | longitude |   dword    |      integer       |
|  Altitude  | altitude  |    word    |      integer       |
|   Speed    |   speed   |    word    |      integer       |
| Direction  | direction |    word    |      integer       |
|    Time    |   time    |   bcd(6)   |       string       |

|            Optional Field             | JSON Key | Value Type | Value Type in JSON |
| :-----------------------------------: | :------: | :--------: | :----------------: |
| Additional Location Information Items |  extra   |     -      |        map         |

##### JT/T 808-2019 Changes

- **New alarm flags**:  
  - bit15 (illegal driving alarm)  
  - bit16 (tire pressure warning)  
  - bit17 (right-turn blind spot anomaly alarm)

- **Modified alarm flags**:  
  - bit29 (collision warning -> collision/rollover alarm)  
  - bit31 (illegal door opening alarm -> reserved)

- **New status bits**:  
  - bit6 (forward collision warning collected by the emergency braking system)  
  - bit7 (lane departure warning)  
  - bit22 (vehicle is in driving state)

- **New additional information items**:  
  - 0x05 (tire pressure)  
  - 0x06 (carriage temperature)

<!-- TODO: refine alarm more details -->

- The list of location additional information items is included in `extra`.

|          Field (Additional Information Description)          |       JSON Key       | Value Type |   Value Type in JSON    |
| :----------------------------------------------------------: | :------------------: | :--------: | :---------------------: |
|                           Mileage                            |       mileage        |   dword    |         integer         |
|                          Fuel Meter                          |      fuel_meter      |    word    |         integer         |
|              Speed from Driving Record Function              |        speed         |    word    |         integer         |
|        ID of Alarm Events Needing Manual Confirmation        |       alarm_id       |    word    |         integer         |
|                Tire Pressure (Added in 2019)                 |    tire_pressure     |     -      | string (base64 encoded) |
|             Carriage Temperature (Added in 2019)             |     carriage_temp    |    word    |         integer         |
|    Overspeed Alarm Additional Information (Length 1 or 5)    |   overspeed_alarm    |     -      |           map           |
|      Entry/Exit Area/Route Alarm Additional Information      |     in_out_alarm     |     -      |           map           |
| Route Travel Time Too Short/Too Long Alarm Additional Information |   path_time_alarm    |     -      |           map           |
|             Extended Vehicle Signal Status Bits              | See Status Bit Table |     -      |            -            |
|                        IO Status Bits                        |      io_status       |     -      |           map           |
|                            Analog                            |        analog        |     -      |           map           |
|        Wireless Communication Network Signal Strength        |         rssi         |    byte    |         integer         |
|                     GNSS Satellite Count                     |     gnss_sat_num     |    byte    |         integer         |
|           Length of Subsequent Custom Information            |       custome        |     -      | string(base64 encoded)  |

- Overspeed alarm additional information (length 1 or 5), in map `overspeed_alarm`.

|     Field     | JSON Key | Value Type | Value Type in JSON |
| :-----------: | :------: | :--------: | :----------------: |
| Location Type |   type   |    byte    |      integer       |

|   Optional Field   | JSON Key | Value Type | Value Type in JSON |
| :----------------: | :------: | :--------: | :----------------: |
| Area or Segment ID |    id    |   dword    |      integer       |

- Entry/Exit Area/Route Alarm Additional Information, in map `in_out_alarm`

|       Field        | JSON Key  | Value Type | Value Type in JSON |
| :----------------: | :-------: | :--------: | :----------------: |
|   Location Type    |   type    |    byte    |      integer       |
| Area or Segment ID |    id     |   dword    |      integer       |
|     Direction      | direction |    byte    |      integer       |

- Route Travel Time Too Short/Too Long Alarm Additional Information, in map `path_time_alarm`

|       Field       | JSON Key | Value Type | Value Type in JSON |
| :---------------: | :------: | :--------: | :----------------: |
|    Segment ID     |    id    |   dword    |      integer       |
| Route Travel Time |   time   |    word    |      integer       |
|      Result       |  result  |    byte    |      integer       |

- IO Status Bits, in map `io_status`

|       Field       |  JSON Key  | Value Type | Value Type in JSON |
| :---------------: | :--------: | :--------: | :----------------: |
| Deep Sleep Status | deep_sleep |   1 bit    |      integer       |
|   Sleep Status    |   sleep    |   1 bit    |      integer       |

- Analog, in map `analog`

|  Field   | JSON Key | Value Type | Value Type in JSON |
| :------: | :------: | :--------: | :----------------: |
| Analog 0 |   ad0    |  16 bits   |      integer       |
| Analog 1 |   ad1    |  16 bits   |      integer       |

- Extended Vehicle Signal Status Bits, in map `extra`

|         Field          |    JSON Key     | Value Type |             Value Type in JSON             |
| :--------------------: | :-------------: | :--------: | :----------------------------------------: |
|         Signal         |     signal      |  - 2 bits  | map, `{"low_beam": VAL, "high_beam": VAL}` |
|   Right Turn Signal    |   right_turn    |   1 bit    |                  integer                   |
|    Left Turn Signal    |    left_turn    |   1 bit    |                  integer                   |
|      Brake Signal      |      brake      |   1 bit    |                  integer                   |
|     Reverse Signal     |     reverse     |   1 bit    |                  integer                   |
|    Fog Light Signal    |       fog       |   1 bit    |                  integer                   |
|      Side Marker       |   side_marker   |   1 bit    |                  integer                   |
|      Horn Status       |      horn       |   1 bit    |                  integer                   |
| Air Conditioner Status | air_conditioner |   1 bit    |                  integer                   |
|     Neutral Signal     |     neutral     |   1 bit    |                  integer                   |
|    Retarder Working    |    retarder     |   1 bit    |                  integer                   |
|      ABS Working       |       abs       |   1 bit    |                  integer                   |
|     Heater Working     |     heater      |   1 bit    |                  integer                   |
|     Clutch Status      |      cluth      |   1 bit    |                  integer                   |

- Signal Status, in map `signal`

|      Field       | JSON Key  | Value Type | Value Type in JSON |
| :--------------: | :-------: | :--------: | :----------------: |
| Low Beam Signal  | low_beam  |   1 bit    |      integer       |
| High Beam Signal | high_beam |   1 bit    |      integer       |

Example:

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

#### Position Information Query `"msg_id": 33281` 0x8201

Empty JSON

#### Position Information Query Response `"msg_id": 513` 0x0201

|      Field      | JSON Key | Value Type | Value Type in JSON |
| :-------------: | :------: | :--------: | :----------------: |
|  Response Seq   |   seq    |    word    |      integer       |
| Position Report |  params  |     -      |        map         |

#### Temporary Location Tracking Control `"msg_id": 33282` 0x8202

|       Field       | JSON Key | Value Type | Value Type in JSON |
| :---------------: | :------: | :--------: | :----------------: |
|     Interval      |  period  |    word    |      integer       |
| Tracking Duration |  expiry  |   dword    |      integer       |

#### Manual Alarm Confirmation Message `"msg_id": 33283` 0x8203

|       Field       | JSON Key | Value Type | Value Type in JSON |
| :---------------: | :------: | :--------: | :----------------: |
| Alarm Message Seq |   seq    |    word    |      integer       |
| Manual Alarm Type |   type   |   dword    |      integer       |

#### Server-Initiated Link Detection Request to Terminal `"msg_id": 33284` 0x8204 (Added in 2019)

Empty JSON

#### Text Message Dispatch `"msg_id": 33536` 0x8300

##### JT/T 808-2013

|   Field   | JSON Key | Value Type | Value Type in JSON |
| :-------: | :------: | :--------: | :----------------: |
|   Flag    |   flag   |    byte    |      integer       |
| Text Info |   text   |   string   |       string       |

##### JT/T 808-2019

|   Field   | JSON Key  | Value Type | Value Type in JSON |
| :-------: | :-------: | :--------: | :----------------: |
|   Flag    |   flag    |    byte    |      integer       |
| Text Type | text_type |    byte    |      Integer       |
| Text Info |   text    |   string   |       string       |

**Note**: In the 2019 version, a new text type field is added, and the meanings of flag bits 0–1 are changed.

#### Event Setting `"msg_id": 33537` 0x8301

|     Field     | JSON Key | Value Type |                      Value Type in JSON                      |
| :-----------: | :------: | :--------: | :----------------------------------------------------------: |
|   Set Type    |   type   |    byte    |                           integer                            |
| Total Events  |  length  |    byte    |                           integer                            |
|  Event List   |  events  |    list    | list of event. `[{"id": ID, "length": LEN, "content": CON}, ...]` |
|   Event ID    |    id    |    byte    |                           integer                            |
| Event Length  |  length  |    byte    |                           integer                            |
| Event Content | content  |   string   |                            string                            |

#### Event Report `"msg_id": 769` 0x0301

|  Field   | JSON Key | Value Type | Value Type in JSON |
| :------: | :------: | :--------: | :----------------: |
| Event ID |    id    |    byte    |      integer       |

#### Question Dispatch `"msg_id": 33538` 0x8302

|         Field         | JSON Key | Value Type |                      Value Type in JSON                      |
| :-------------------: | :------: | :--------: | :----------------------------------------------------------: |
|         Flag          |   flag   |    byte    |                           integer                            |
|    Question Length    |  length  |    byte    |                           integer                            |
|       Question        | question |   string   |                            string                            |
| Answer Candidate List | answers  |    list    | list of answer. `[{"id": ID, "len": LEN, "answer": ANS}, ...]` |
|       Answer ID       |    id    |    byte    |                           integer                            |
| Answer Content Length |   len    |    byte    |                           integer                            |
|    Answer Content     |  answer  |   string   |                            string                            |

#### Question Response `"msg_id": 770` 0x0302

|    Field     | JSON Key | Value Type | Value Type in JSON |
| :----------: | :------: | :--------: | :----------------: |
| Response Seq |   seq    |    word    |      integer       |
|  Answer ID   |    id    |    byte    |      integer       |

#### Information Service Menu Setting `"msg_id": 33539` 0x8303

|      Field       | JSON Key | Value Type | Value Type in JSON |
| :--------------: | :------: | :--------: | :----------------: |
|     Set Type     |   type   |    byte    |      integer       |
| Total Info Items |  length  |    byte    |      integer       |
|  Info Item List  |  menus   |    list    |    list of menu    |
|    Info Type     |   type   |    byte    |      integer       |
| Info Name Length |  length  |    word    |      integer       |
|    Info Name     |   info   |   string   |       string       |

#### Information Service/Cancel `"msg_id": 771` 0x0303

|      Field       | JSON Key | Value Type | Value Type in JSON |
| :--------------: | :------: | :--------: | :----------------: |
|    Info Type     |    id    |    byte    |      integer       |
| Dial/Cancel Flag |   flag   |    byte    |      integer       |

#### Information Service `"msg_id": 33540` 0x8304

|    Field     | JSON Key | Value Type | Value Type in JSON |
| :----------: | :------: | :--------: | :----------------: |
|  Info Type   |   type   |    byte    |      integer       |
| Info Length  |  length  |    word    |      integer       |
| Info Content |   info   |   string   |       string       |

#### Callback Phone `"msg_id": 33792` 0x8400

|    Field     | JSON Key | Value Type | Value Type in JSON |
| :----------: | :------: | :--------: | :----------------: |
|     Flag     |   type   |    byte    |      integer       |
| Phone Number |  phone   |   string   |       string       |

#### Phonebook Setting `"msg_id": 33793` 0x8401

|        Field        | JSON Key  | Value Type | Value Type in JSON |
| :-----------------: | :-------: | :--------: | :----------------: |
|      Set Type       |   type    |    byte    |      integer       |
|   Total Contacts    |  length   |    byte    |      integer       |
|    Contact Item     | contacts  |    list    |  list of contact.  |
|        Flag         |   type    |    byte    |      integer       |
| Phone Number Length | phone_len |    byte    |      integer       |
|    Phone Number     |   phone   |   string   |       string       |
|   Contact Length    | name_len  |    byte    |      integer       |
|       Contact       |   name    |   string   |       string       |

Contact Item Example:

```json
[{"type": TYPE, "phone_len", PH_LEN, "phone": PHONE, "name_len": NAME_LEN, "name": NAME}, ...]
```

#### Vehicle Control `"msg_id": 34048` 0x8500

##### JT/T 808-2013

|    Field     | JSON Key | Value Type | Value Type in JSON |
| :----------: | :------: | :--------: | :----------------: |
| Control Flag |   flag   |    byte    |      integer       |

##### JT/T 808-2019

|          Field          | JSON Key | Value Type |   Value Type in JSON    |
| :---------------------: | :------: | :--------: | :---------------------: |
|      Control Flag       |   flag   |    word    |         integer         |
| Number of Control Types |  length  |    byte    |         integer         |
|    Control Type List    | controls |    list    |     list of control     |
|     Control Type ID     | type_id  |    byte    |         integer         |
| Control Type Parameters |  param   |   binary   | string (base64 encoded) |

**Note**: In the 2019 version, the message structure is completely redesigned to consist of the number of control types plus a control type list.

#### Vehicle Control Response `"msg_id": 1280` 0x0500

|        Field         | JSON Key | Value Type | Value Type in JSON |
| :------------------: | :------: | :--------: | :----------------: |
|   Response Serial    |   seq    |    word    |      integer       |
| Location Report Body | location |    map     |  map of location   |

#### Set Circular Area `"msg_id": 34304` 0x8600

##### JT/T 808-2013

|         Field         |      JSON Key      | Value Type | Value Type in JSON |
| :-------------------: | :----------------: | :--------: | :----------------: |
|   Setting Attribute   |        type        |    byte    |      integer       |
| Total Number of Areas |       length       |    byte    |      integer       |
|       Area Item       |       areas        |    list    |    list of area    |
|        Area ID        |         id         |   dword    |      integer       |
|     Area Property     |        flag        |    word    |      integer       |
|    Center Latitude    |  center_latitude   |   dword    |      integer       |
|   Center Longitude    |  center_longitude  |   dword    |      integer       |
|        Radius         |       radius       |   dword    |      integer       |
|      Start Time       |     start_time     |   string   |       string       |
|       End Time        |      end_time      |   string   |       string       |
|     Maximum Speed     |     max_speed      |    word    |      integer       |
|  Overspeed Duration   | overspeed_duration |    byte    |      integer       |

##### JT/T 808-2019 Added Fields

|        Field        |    JSON Key     | Value Type | Value Type in JSON |
| :-----------------: | :-------------: | :--------: | :----------------: |
| Night maximum speed | night_max_speed |    word    |      integer       |
| Region name length  |    name_len     |    word    |      integer       |
|     Region name     |      name       |   string   |       string       |

**Note**: In the 2019 version, the region item adds the fields night maximum speed, region name length, and region name.

Area List Example:

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

#### Delete Circular Area `"msg_id": 34305` 0x8601

|      Field       | JSON Key | Value Type | Value Type in JSON |
| :--------------: | :------: | :--------: | :----------------: |
| Number of Areas  |  length  |    byte    |      integer       |
| List of Area IDs |   ids    |    list    |    list of id.     |
|   Area ID 1~n    |    -     |   dword    |      integer       |

```
[ID1, ID2, ...]
```

#### Set Rectangular Area `"msg_id": 34306` 0x8602

##### JT/T 808-2013

|         Field         |      JSON Key      | Value Type |   Value Type in JSON   |
| :-------------------: | :----------------: | :--------: | :--------------------: |
|   Setting Attribute   |        type        |    byte    |        integer         |
| Total Number of Areas |       length       |    byte    |        integer         |
|       Area Item       |       areas        |    list    | list of rectangle area |
|        Area ID        |         id         |   dword    |        integer         |
|     Area Property     |        flag        |    word    |        integer         |
|  Upper Left Latitude  |       lt_lat       |   dword    |        integer         |
| Upper Left Longitude  |       lt_lng       |   dword    |        integer         |
| Lower Right Latitude  |       rb_lat       |   dword    |        integer         |
| Lower Right Longitude |       rb_lng       |   dword    |        integer         |
|      Start Time       |     start_time     |   string   |         string         |
|       End Time        |      end_time      |   string   |         string         |
|     Maximum Speed     |     max_speed      |    word    |        integer         |
|  Overspeed Duration   | overspeed_duration |    byte    |        integer         |

##### JT/T 808-2019 Added Fields

|        Field        |    JSON Key     | Value Type | Value Type in JSON |
| :-----------------: | :-------------: | :--------: | :----------------: |
| Night maximum speed | night_max_speed |    word    |      integer       |
| Region name length  |    name_len     |    word    |      integer       |
|     Region name     |      name       |   string   |       string       |

**Note**: In the 2019 version, the region item adds the fields night maximum speed, region name length, and region name.

#### Delete Rectangular Area `"msg_id": 34307` 0x8603

|      Field       | JSON Key | Value Type | Value Type in JSON |
| :--------------: | :------: | :--------: | :----------------: |
| Number of Areas  |  length  |    byte    |      integer       |
| List of Area IDs |   ids    |    list    |    list of id.     |
|   Area ID 1~n    |    -     |   dword    |      integer       |

#### Set Polygonal Area `"msg_id": 34308` 0x8604

##### JT/T 808-2013

|          Field           |      JSON Key      | Value Type | Value Type in JSON |
| :----------------------: | :----------------: | :--------: | :----------------: |
|         Area ID          |         id         |   dword    |      integer       |
|      Area Property       |        flag        |   dword    |      integer       |
|        Start Time        |     start_time     |   string   |       string       |
|         End Time         |      end_time      |   string   |       string       |
|      Maximum Speed       |     max_speed      |    word    |      integer       |
|    Overspeed Duration    | overspeed_duration |    byte    |      integer       |
| Total Number of Vertices |       length       |    word    |      integer       |
|   List of Vertex Items   |       points       |    list    |   list of point    |
|     Vertex Latitude      |        lat         |   dword    |      integer       |
|     Vertex Longitude     |        lng         |   dword    |      integer       |

##### JT/T 808-2019 Added Fields

|        Field        |    JSON Key     | Value Type | Value Type in JSON |
| :-----------------: | :-------------: | :--------: | :----------------: |
| Night maximum speed | night_max_speed |    word    |      integer       |
| Region name length  |    name_len     |    word    |      integer       |
|     Region name     |      name       |   string   |       string       |

**Note**: The 2019 version adds the fields night maximum speed, region name length, and region name.

#### Delete Polygonal Area `"msg_id": 34309` 0x8605

|      Field       | JSON Key | Value Type | Value Type in JSON |
| :--------------: | :------: | :--------: | :----------------: |
| Number of Areas  |  length  |    byte    |      integer       |
| List of Area IDs |   ids    |    list    |     list of id     |
|   Area ID 1~n    |    -     |   dword    |      integer       |

#### Set Route `"msg_id": 34310` 0x8606

##### JT/T 808-2013

|                   Field                    |      JSON Key      | Value Type | Value Type in JSON |
| :----------------------------------------: | :----------------: | :--------: | :----------------: |
|                  Route ID                  |         id         |   dword    |      integer       |
|               Route Property               |        flag        |    word    |      integer       |
|                 Start Time                 |     start_time     |   string   |       string       |
|                  End Time                  |      end_time      |   string   |       string       |
|       Total Number of Turning Points       |       length       |    word    |      integer       |
|             Turning Point Item             |       points       |    list    |   list of point    |
|              Turning Point ID              |      point_id      |   dword    |      integer       |
|                 Segment ID                 |      path_id       |   dword    |      integer       |
|           Turning Point Latitude           |     point_lat      |   dword    |      integer       |
|          Turning Point Longitude           |     point_lng      |   dword    |      integer       |
|               Segment Width                |       width        |    byte    |      integer       |
|              Segment Property              |       attrib       |    byte    |      integer       |
|   Overlong Threshold for Segment Driving   |       passed       |    word    |      integer       |
| Insufficient Threshold for Segment Driving |     uncovered      |    word    |      integer       |
|          Maximum Speed of Segment          |     max_speed      |    word    |      integer       |
|       Overspeed Duration for Segment       | overspeed_duration |    byte    |      integer       |

##### JT/T 808-2019 Added Fields

|                Field                 |    JSON Key     | Value Type | Value Type in JSON |
| :----------------------------------: | :-------------: | :--------: | :----------------: |
| Night maximum speed for road segment | night_max_speed |    word    |      integer       |
|          Route name length           |    name_len     |    word    |      integer       |
|              Route name              |      name       |   string   |       string       |

**Note**: In the 2019 version, the fields night maximum speed for road segments, route name length, and route name are added.

#### Delete Route `"msg_id": 34311` 0x8607

|       Field       | JSON Key | Value Type | Value Type in JSON |
| :---------------: | :------: | :--------: | :----------------: |
| Number of Routes  |  length  |    byte    |      integer       |
| List of Route IDs |   ids    |    list    |     list of id     |
|     Route ID      |    -     |   dword    |      integer       |

#### Query Area or Route Data `"msg_id": 34312` 0x8608 (Added in 2019)

|       Field        | JSON Key |   Value Type   | Value Type in JSON |
| :----------------: | :------: | :------------: | :----------------: |
|     Query type     |   type   |      byte      |      integer       |
|    Query count     |  length  |     dword      |      integer       |
| Area/Route ID list |   ids    | byte(4*length) |  list of integer   |

Query type values:  
- 1 – Circular area  
- 2 – Rectangular area  
- 3 – Polygonal area  
- 4 – Route


#### Query Area or Route Data Response `"msg_id": 1544` 0x0608 (Added in 2019)

|        Field         | JSON Key | Value Type |   Value Type in JSON    |
| :------------------: | :------: | :--------: | :---------------------: |
|      Query type      |   type   |    byte    |         integer         |
|   Area/Route count   |  length  |   dword    |         integer         |
| Area/Route item list |  items   |   binary   | string (base64 encoded) |

The contents of the area/route item list vary depending on the query type. For details, refer to the protocol specification.

#### Drive Record Data Collection Command `"msg_id": 34560` 0x8700

|   Field    | JSON Key |       Value Type       | Value Type in JSON |
| :--------: | :------: | :--------------------: | :----------------: |
|  Command   | command  |          byte          |      integer       |
| Data Block |  param   | string(base64 encoded) |       string       |

#### Drive Record Data Upload `"msg_id": 1792` 0x0700

|         Field          | JSON Key |       Value Type       | Value Type in JSON |
| :--------------------: | :------: | :--------------------: | :----------------: |
| Response Serial Number |   seq    |          word          |      integer       |
|        Command         | command  |          byte          |      integer       |
|       Data Block       |   data   | string(base64 encoded) |       string       |

#### Drive Record Parameter Downward Command `"msg_id": 34561` 0x8701

|   Field    | JSON Key |       Value Type       | Value Type in JSON |
| :--------: | :------: | :--------------------: | :----------------: |
|  Command   | command  |          byte          |      integer       |
| Data Block |  param   | string(base64 encoded) |       string       |

#### Electronic Waybill Report `"msg_id": 1793` 0x0701

|           Field            | JSON Key |       Value Type       | Value Type in JSON |
| :------------------------: | :------: | :--------------------: | :----------------: |
| Electronic Waybill Length  |  length  |         dword          |      integer       |
| Electronic Waybill Content |   data   | string(base64 encoded) |       string       |

#### Request for Uploading Driver Identity Information `"msg_id": 34562` 0x8702

Empty JSON

#### Driver Identity Information Collection Report `"msg_id": 1794` 0x0702

##### JT/T 808-2013

|                    Field                    |   JSON Key   | Value Type | Value Type in JSON |
| :-----------------------------------------: | :----------: | :--------: | :----------------: |
|                   Status                    |    status    |    byte    |      integer       |
|                    Time                     |     time     |   string   |       string       |
|             IC Card Read Result             |  ic_result   |    byte    |      integer       |
|                 Driver Name                 | driver_name  |   string   |       string       |
| Professional Qualification Certificate Code | certificate  |   string   |       string       |
|           Issuing Authority Name            | organization |   string   |       string       |
|            Certificate Validity             | cert_expiry  |   string   |       string       |

##### JT/T 808-2019 Added Field

|         Field         | JSON Key | Value Type | Value Type in JSON |
| :-------------------: | :------: | :--------: | :----------------: |
| Driver ID card number | id_card  |   string   |       string       |

**Note**: The 2019 version adds the driver ID card number field.

#### Bulk Upload of Location Data `"msg_id": 1796` 0x0704

|             Field             | JSON Key | Value Type | Value Type in JSON |
| :---------------------------: | :------: | :--------: | :----------------: |
|      Location Data Type       |   type   |    byte    |      integer       |
|     Number of Data Items      |  length  |    word    |      integer       |
| Location Reporting Data Items | location |    list    |  list of location  |

#### CAN Bus Data Upload `"msg_id": 1797` 0x0705

|            Field            |  JSON Key   | Value Type |   Value Type in JSON   |
| :-------------------------: | :---------: | :--------: | :--------------------: |
|    Number of Data Items     |   length    |    word    |        integer         |
| CAN Bus Data Reception Time |    time     |   bcd(5)   |        integer         |
|     CAN Bus Data Items      |  can_data   |    list    |   list of can data.    |
|   CAN Bus Channel Number    |   channel   |   1 bit    |        integer         |
|         Frame Type          | frame_type  |   1 bit    |        integer         |
|   Data Collection Method    | data_method |   1 bit    |        integer         |
|         CAN Bus ID          |     id      |  29 bits   |        integer         |
|          CAN Data           |    data     |   binary   | string(base64 encoded) |

#### Multimedia Event Information Upload `"msg_id": 2048` 0x0800

|           Field            | JSON Key | Value Type | Value Type in JSON |
| :------------------------: | :------: | :--------: | :----------------: |
|     Multimedia Data ID     |    id    |   dword    |      integer       |
|      Multimedia Type       |   type   |    byte    |      integer       |
| Multimedia Encoding Format |  format  |    byte    |      integer       |
|      Event Item Code       |  event   |    byte    |      integer       |
|         Channel ID         | channel  |    byte    |      integer       |

#### Multimedia Data Upload `"msg_id": 2049` 0x0801

|           Field            |  JSON Key  | Value Type |   Value Type in JSON   |
| :------------------------: | :--------: | :--------: | :--------------------: |
|       Multimedia ID        |     id     |   dword    |        integer         |
|      Multimedia Type       |    type    |    byte    |        integer         |
| Multimedia Encoding Format |   format   |    byte    |        integer         |
|      Event Item Code       |   event    |    byte    |        integer         |
|         Channel ID         |  channel   |    byte    |        integer         |
|     Location Reporting     |  location  |  byte(28)  |          map           |
|  Multimedia Data Package   | multimedia |   binary   | string(base64 encoded) |

#### Response to Multimedia Data Upload `"msg_id": 34816` 0x8800

|                  Field                  | JSON Key name | Value Type | Value Type in JSON |
| :-------------------------------------: | :-----------: | :--------: | :----------------: |
|              Multimedia ID              |     mm_id     |   dword    |      integer       |
| Total Number of Retransmission Packages |    length     |    byte    |      integer       |
|   List of Retransmission Package IDs    |   retx_ids    |    list    | list of retry IDs  |

#### Immediate Camera Capture Command `"msg_id": 34817` 0x8801

|          Field          |   JSON Key   | Value Type | Value Type in JSON |
| :---------------------: | :----------: | :--------: | :----------------: |
|       Channel ID        |  channel_id  |    byte    |      integer       |
|     Capture Command     |   command    |    word    |      integer       |
| Interval/Recording Time |    period    |    word    |      integer       |
|        Save Flag        |     save     |    byte    |      integer       |
|       Resolution        |  resolution  |    byte    |      integer       |
|   Image/Video Quality   |   quality    |    byte    |      integer       |
|       Brightness        |    bright    |    byte    |      integer       |
|        Contrast         |   contrast   |    byte    |      integer       |
|       Saturation        |   saturate   |    byte    |      integer       |
|      Chromaticity       | chromaticity |    byte    |      integer       |

**Note**: The 2019 version adds new values to the resolution field: `0x00` (lowest quality compression) and `0xFF` (highest quality compression).

#### Immediate Camera Capture Response `"msg_id": 2053` 0x0805

|          Field           | JSON Key |   Value Type   | Value Type in JSON |
| :----------------------: | :------: | :------------: | :----------------: |
|  Response Serial Number  |   seq    |      word      |      integer       |
|          Result          |  result  |      byte      |      integer       |
| Number of Multimedia IDs |  length  |      word      |      integer       |
|  List of Multimedia IDs  |   ids    | byte(4*length) |      integer       |

#### Storage Multimedia Data Retrieval `"msg_id": 34818` 0x8802

|      Field      | JSON Key | Value Type | Value Type in JSON |
| :-------------: | :------: | :--------: | :----------------: |
| Multimedia Type |          |    byte    |                    |
|   Channel ID    |          |    byte    |                    |
| Event Item Code |          |    byte    |                    |
|   Start Time    |          |   string   |                    |
|    End Time     |          |   string   |                    |

#### Response to Storage Multimedia Data Retrieval `"msg_id": 2050` 0x0802

##### JT/T 808-2013

|                 Field                 | JSON Key | Value Type |  Value Type in JSON   |
| :-----------------------------------: | :------: | :--------: | :-------------------: |
|        Response Serial Number         |   seq    |    word    |        integer        |
| Total Number of Multimedia Data Items |  length  |    word    |        integer        |
|             Search Items              |  result  |    list    | list of search result |
|             Multimedia ID             |    id    |   dword    |        integer        |
|            Multimedia Type            |   type   |    byte    |        integer        |
|              Channel ID               | channel  |    byte    |        integer        |
|            Event Item Code            |  event   |    byte    |        integer        |
|          Location Reporting           | location |  byte(28)  |          map          |

##### JT/T 808-2019

|                 Field                 | JSON Key | Value Type |  Value Type in JSON   |
| :-----------------------------------: | :------: | :--------: | :-------------------: |
|        Response serial number         |   seq    |    word    |        integer        |
| Total number of multimedia data items |  length  |    word    |        integer        |
|          Search result items          |  result  |    list    | list of search result |
|             Multimedia ID             |    id    |   dword    |        integer        |
|            Multimedia type            |   type   |    byte    |        integer        |
|              Channel ID               | channel  |    byte    |        integer        |
|            Event item code            |  event   |    byte    |        integer        |
|      Location information report      | location |  variable  |          map          |

**Note**: In the 2019 version, the location information report message body changes from a fixed length of 28 bytes to a variable length.

#### Command for Uploading Stored Multimedia Data `"msg_id": 34819` 0x8803

|      Field      |  JSON Key  | Value Type | Value Type in JSON |
| :-------------: | :--------: | :--------: | :----------------: |
| Multimedia Type |    type    |    byte    |      integer       |
|   Channel ID    |  channel   |    byte    |      integer       |
| Event Item Code |   event    |    byte    |      integer       |
|   Start Time    | start_time |   string   |       string       |
|    End Time     |  end_time  |   string   |       string       |
|   Delete Flag   |   delete   |    byte    |      integer       |

#### Audio Recording Start Command `"msg_id": 34820` 0x8804

|        Field        | JSON Key | Value Type | Value Type in JSON |
| :-----------------: | :------: | :--------: | :----------------: |
|  Recording Command  | command  |    byte    |      integer       |
|   Recording Time    |   time   |    word    |      integer       |
|      Save Flag      |   save   |    byte    |      integer       |
| Audio Sampling Rate |   rate   |    byte    |      integer       |

#### Command for Single Stored Multimedia Item Retrieval Upload `"msg_id": 34821` 0x8805

|     Field     | JSON Key | Value Type | Value Type in JSON |
| :-----------: | :------: | :--------: | :----------------: |
| Multimedia ID |    id    |   dword    |      integer       |
|  Delete Flag  |   flag   |    byte    |      integer       |

#### Downward Data Transmission `"msg_id": 35072` 0x8900

|            Field            | JSON Key | Value Type |   Value Type in JSON   |
| :-------------------------: | :------: | :--------: | :--------------------: |
|  Transmitted Message Type   |   type   |    byte    |        integer         |
| Transmitted Message Content |   data   |   binary   | string(base64 encoded) |

#### Upward Data Transmission `"msg_id": 2304` 0x0900

|            Field            | JSON Key | Value Type |   Value Type in JSON   |
| :-------------------------: | :------: | :--------: | :--------------------: |
|  Transmitted Message Type   |   type   |    byte    |        integer         |
| Transmitted Message Content |   data   |   binary   | string(base64 encoded) |

#### Data Compression Report `"msg_id": 2305` 0x0901

|           Field           | JSON Key | Value Type |   Value Type in JSON   |
| :-----------------------: | :------: | :--------: | :--------------------: |
| Compressed Message Length | length   |   dword    |        integer         |
|  Compressed Message Body  | data     |   binary   | string(base64 encoded) |

#### Platform RSA Public Key `"msg_id": 35328` 0x8A00

| Field | JSON Key | Value Type |   Value Type in JSON   |
| :---: | :------: | :--------: | :--------------------: |
|   e   |    e     |   dword    |        integer         |
|   n   |    n     | byte(128)  | string(base64 encoded) |

#### Terminal RSA Public Key `"msg_id": 2560` 0x0A00

| Field | JSON Key | Value Type |   Value Type in JSON   |
| :---: | :------: | :--------: | :--------------------: |
|   e   |    e     |   dword    |        integer         |
|   n   |    n     | byte(128)  | string(base64 encoded) |

#### Reserved 0x8F00 ~ 0x8FFF

#### Reserved 0x0F00 ~ 0x0FFF

#### Vendor-Defined Uplink Messages 0xE000 ~ 0xEFFF (Added in 2019)

Vendor-defined messages. The message body format is defined by the vendor.

#### Vendor-Defined Downlink Messages 0xF000 ~ 0xFFFF (Added in 2019)

Vendor-defined messages. The message body format is defined by the vendor.
