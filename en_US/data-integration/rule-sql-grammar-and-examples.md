# SQL syntax and examples

## SQL syntax

### FROM, SELECT, and WHERE clauses

The basic format of the SQL statement of the rule engine is:
```sql
SELECT <fields> FROM <topic> [WHERE <any>]
```
- The `FROM` clause mounts rules to a topic
- The `SELECT` clause is used to select fields in the output
- The `WHERE` clause is used to filter messages based on conditions

The SELECT statement is used to determine the fields in the final output. such as:

The following SQL output will have only two fields of "a" and "b":

```
SELECT a, b FROM "t/#"
```

The WHERE statement is used to conditionally filter the fields available in this event or the fields defined in the SELECT statement. such as:

```
# Select the message from the terminal whose username is 'abc', and the output will be all available fields:

SELECT * FROM "#" WHERE username = 'abc'

## Select the message sent from the terminal whose clientid is 'abc', and the output will have only one field of cid.
## Note that the cid variable is defined in the SELECT statement, so it can be used in the WHERE statement:

SELECT clientid as cid FROM "#" WHERE cid = 'abc'

## Select the message sent from the terminal whose username is 'abc', and the output will have only one field of cid.
## Note that although only one field of cid is selected in the SELECT statement, all available fields in the message publishing event (such as clientid, username, etc.) can still be used in the WHERE statement:

SELECT clientid as cid FROM "#" WHERE username = 'abc'

## But the following SQL statement will not work, because the variable xyz is neither an available field in the message publishing event nor defined in the SELECT statement:

SELECT clientid as cid FROM "#" WHERE xyz = 'abc'
```

The FROM statement is used to select the source of the event. If the message is published, fill in the topic of the message, if it is an event, fill in the corresponding event topic.





### FOREACH, DO, and INCASE clauses

If you want to perform some operations and actions for each element of an array data, you need to use the `FOREACH-DO-INCASE` syntax. The basic format is:

```sql
FOREACH <Field name> [DO <Condition>] [INCASE <Condition>] FROM <Topic> [WHERE <Condition>]
```

- The `FOREACH` clause is used to select the field that needs to perform foreach operation. Note that the selected field must be an array type
- The `DO` clause is used to transform each element in the array selected by FOREACH and select the field of interest
- The `INCASE` clause is used to apply conditional filtering to a field selected by DO

DO is equivalent to the SELECT clause for objects in the current loop, while INCASE is equivalent to the WHERE statement for objects in the current loop.

```sql

FOREACH
    payload.sensors as e ## Note that the selected field must be an array type
DO                       ## The Do clause is used to select fields in the output
    clientid,
    e.name as name,
    e.idx as idx
INCASE
    e.idx >= 1          ## apply conditional filtering to a field selected
FROM "t/#"              ## mounts rules to a topic
```
The DO and INCASE clauses are optional.


#### Operational symbol
| Function | Purpose                                                      | Returned value              |      |
| -------- | ------------------------------------------------------------ | --------------------------- | ---- |
| `+`      | addition, or string concatenation                            | Sum, or concatenated string |      |
| `-`      | Subtraction                                                  | Difference                  |      |
| `*`      | multiplication                                               | product                     |      |
| `/`      | division                                                     | Quotient                    |      |
| `div`    | Integer division                                             | Integer quotient            |      |
| `mod`    | modulus                                                      | module                      |      |
| `=`      | Compare whether the two are completely equal. It can be used to compare variables and topics | true/false                  |      |
| `=~`     | Compare whether the topic can match the topic filter. It can only be used for topic matching | true/false                  |      |


### Compare symbol

| Function | Purpose | Returned value |
| ------ | ------------------- | ---------- |
| `>` | greater than | true/false |
| `<` | less than | true/false |
| `<=` | less than or equal | true/false |
| `>=` | greater than or equal | true/false |
| `<>` | not equal | true/false |
| `!=` | not equal | true/false |


## SQL statement example:
### Basic syntax examples

-  Extract all fields from the messages with a topic of "t/a":
    ```sql
    SELECT * FROM "t/a"
    ```
-  Extract all fields from the messages with a topic of "t/a" or "t/b":
    ```sql
    SELECT * FROM "t/a","t/b"
    ```
-  Extract all fields from the message with a topic that can match 't/#'.
    ```sql
    SELECT * FROM "t/#"
    ```
-  Extract the qos, username, and clientid fields from the message with a topic that can match 't/#' :
    ```sql
    SELECT qos, username, clientid FROM "t/#"
    ```
-  Extract the username field from any topic message with the filter criteria of username = 'Steven':
    ```sql
    SELECT username FROM "#" WHERE username='Steven'
    ```
- Extract the x field from the payload of message with any topic and create the alias x for use in the WHERE clause. The WHERE clause is restricted as x = 1. Note that the payload must be in JSON format. Example: This SQL statement can match the payload `{"x": 1}`, but can not match to the payload `{"x": 2}`:
    ```sql
    SELECT payload.x as x FROM "#" WHERE x = 1
    ```
- Similar to the SQL statement above, but nested extract the data in the payload, this SQL statement can match the payload{"x": {"y": 1}}`:
    ```sql
    SELECT payload FROM "#" WHERE payload.x.y = 1
    ```
-  If a MQTT client with clientid = 'c1' connected, extract its source IP address and port number:
    ```sql
    SELECT peername as ip_port FROM "$events/client_connected" WHERE clientid = 'c1'
    ```
- Filter all clientids that subscribe to the 't/#' topic and have a subscription level of QoS 1 :
    ```sql
    SELECT clientid FROM "$events/session_subscribed" WHERE topic = 't/#' and qos = 1
    ```
- Filter all clientids that subscribe to the 't/#' topic and subscription level is QoS 1. Note that the strict equality operator '=~' is used here, so it does not match subscription requests with the topic 't' or 't/+/a' :
    ```sql
    SELECT clientid FROM "$events/session_subscribed" WHERE topic =~ 't/#' and qos = 1
    ```
- For an MQTT 5.0 PUBLISH message, select the User Property with Key "foo":
    ```sql
    SELECT pub_props.'User-Property'.foo as foo FROM "t/#"
    ```

::: tip
- Topic after the FROM clause need to be enclosed in double quotes `""` or single quotes `''`.
- The WHERE clause is followed by the filter condition. If a string is used, it needs to be enclosed in single quotes `'' `.
- If there are multiple topics in the FROM clause, they need to be separated by commas `","`. For example,
    ```sql
    SELECT * FROM "t/1", "t/2".
    ```
- You can use the `"." `Symbol to nest select payloads
- If possible, don't create alias for payload, as this would cause performance degradation.
  i.e. Do not use `SELECT payload as p`
:::

### Examples of FOREACH-DO-INCASE

Suppose there is a message with ClientID of `c_steve` and topic of ` t/1`. The message body is in JSON format, and the sensors field is an array containing multiple Objects:

```json
{
    "date": "2020-04-24",
    "sensors": [
        {"name": "a", "idx":0},
        {"name": "b", "idx":1},
        {"name": "c", "idx":2}
    ]
}
```

**Example 1: It is required that each object in sensors is re-published as a data input to the topic of `sensors/${idx}` with the content of `${name}`. That means the final rule engine will issue 3 messages:**

1) Topic: sensors/0
   Content: a
2) Topic: sensors/1
   Content: b
3) Topic: sensors/2
   Content: c

To complete this rule, we need to configure the following actions:

- Action type: message republish
- Target topic: sensors/$ {idx}
- Target QoS: 0
- Message content template: $ {name}

And the following SQL statement:

```sql
FOREACH
    payload.sensors
FROM "t/#"
```

**Example analysis: **

In this SQL, the FOREACH clause specifies the array sensors that need to be traversed, then the selection result is:

```json
[
  {
    "name": "a",
    "idx": 0
  },
  {
    "name": "b",
    "idx": 1
  },
  {
    "name": "c",
    "idx": 2
  }
]
```

The FOREACH statement will perform a "message republish" action for each object in the result array, so the republish action will be performed 3 times.

**Example 2: It is required that each object in sensors with ids value greater than or equal to 1 is re-published as a data input to the topic of `sensors/${idx}` with the content of  `clientid=${clientid},name=${name},date=${date}`. That means the final rule engine will issue 2 messages:**

1) Topic: sensors/1
   Content: clientid=c_steve,name=b,date=2020-04-24
2) Topic: sensors/2
   Content: clientid=c_steve,name=c,date=2020-04-24

To complete this rule, we need to configure the following actions:

- Action type: message republish
- Target topic: sensors/$ {idx}
- Target QoS: 0
- Message content template: clientid=${clientid},name=${name},date=${date}

And the following SQL statement:

```sql
FOREACH
    payload.sensors
DO
    clientid,
    item.name as name,
    item.idx as idx
INCASE
    item.idx >= 1
FROM "t/#"
```

**Example analysis: **

In this SQL, the FOREACH clause specifies the array `sensors` that need to be traversed; the DO clause selects the fields required for each operation, and we select the outer clientid field here, and the two fields of `name` and `idx` of the current sensor object. Note that  item  represents the object of this loop in the sensors array. The INCASE clause is a filtering condition for the fields in the DO statement, only if idx> = 1 meets the condition. So the selection result of SQL is:

```json
[
  {
    "name": "b",
    "idx": 1,
    "clientid": "c_emqx"
  },
  {
    "name": "c",
    "idx": 2,
    "clientid": "c_emqx"
  }
]
```

The FOREACH statement will perform a "message republish" action for each object in the result array, so the republish action will be performed twice.

In DO and INCASE statements, you can use `item` to access the object of the current loop, or you can customize a variable name by using the `as` syntax in FOREACH. So the SQL statement in this example can be written as:

```sql
FOREACH
    payload.sensors as s
DO
    clientid,
    s.name as name,
    s.idx as idx
INCASE
    s.idx >= 1
FROM "t/#"
```

**Example 3: Based on Example 2, remove the `c_` prefix of `c_steve` in the clientid field**

Various SQL functions can be called in the FOREACH and DO statements. If you want to change `c_steve` into `steve`, you can change the SQL in Example 2 into:

```sql
FOREACH
    payload.sensors as s
DO
    nth(2, tokens(clientid,'_')) as clientid,
    s.name as name,
    s.idx as idx
INCASE
    s.idx >= 1
FROM "t/#"
```

In addition, multiple expressions can also be placed in the FOREACH clause, as long as the last expression specifies the array to traverse. For example, we can change the message body, and there is one more layer of Object outside the sensors:

```json
{
    "date": "2020-04-24",
    "data": {
        "sensors": [
            {"name": "a", "idx":0},
            {"name": "b", "idx":1},
            {"name": "c", "idx":2}
        ]
    }
}
```

Then FOREACH can select data before deciding which array to be traversed:

```sql
FOREACH
    payload.data as data
    data.sensors as s
...
```

### CASE-WHEN Syntax example

**Example 1: Limit the value of the x field in the message to the range of 0 ~ 7.**

```sql
SELECT
  CASE WHEN payload.x < 0 THEN 0
       WHEN payload.x > 7 THEN 7
       ELSE payload.x
  END as x
FROM "t/#"
```

Suppose the message is:

```json
{"x": 8}
```

Then the above SQL output is:

```json
{"x": 7}
```
