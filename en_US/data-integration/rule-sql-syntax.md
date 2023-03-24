# The Rule-Engine Language

The rule engine's SQL-like language has two types of statements: `SELECT` and
`FOREACH`. Each rule can have exactly one statement. The `SELECT` statement is
the most commonly used statement and is intended for situations where the result
of the SQL statement is a single message. The `FOREACH` statement is used to
produce zero or more messages from a single input message. 

The rule engine language also supports complex expressions that can be embedded within the
`SELECT` and `FOREACH` statements. The expressions can be used to transform
data, and filter messages based on conditions. Furthermore, the rule engine
language has a rich set of built-in functions that can be called from within
the expressions. The rule engine language also supports embedding [JQ
programs](https://stedolan.github.io/jq/) in expressions. This allows you to do
arbitrary complex data transformations when it is needed (JQ is a Turing
complete programming language optimized for doing transformations of structured
data).

It is important to note that even if it is possible to do arbitrary complex
data transformation in the rule-engine's SQL language (with the help of JQ), it
is usually not a good idea to do computationally expensive tasks in the
rule-engine as this may affect the performance of the EMQX message
broker negatively. Instead, it is usually better to route expensive tasks to
other services that can send back messages to EMQX with the results if needed.

The user interface for editing rules in the EMQX dashboard contains a test
feature that allows you to test your rules with sample messages (can be enabled
with an Enable Test switch). This is very convenient when one wants to try out
what the result of different statements will be.

We will start by looking at the `SELECT` statement and then the `FOREACH`
statement before we move on to describe the building blocks of expressions and
give some examples of how to use them.

## The SELECT Statement 

The basic format of a `SELECT` statement in the rule engine SQL is as follows:

```sql
SELECT <fields_expressions> FROM <topic> [WHERE <conditions>]
```
- The `FROM` clause attaches the rule to one or more [topic filter](https://www.emqx.com/en/blog/advanced-features-of-mqtt-topics)
- The `SELECT` clause is used to select which fields from the input that should be included in the output message, rename fields and transform data.
- The `WHERE` clause is used to filter messages based on conditions


Here is an example of a `SELECT` statement that will select the `a` and `b` fields (fields are separated by comma) from the input message's payload and include them in the output. All messages to topics matching the [pattern](https://www.emqx.com/en/blog/advanced-features-of-mqtt-topics) `t/#` will be affected by this rule. 

```sql
SELECT a, b FROM "t/#"
```

You can also select field's from the message's meta data. For example, the following rule will select the `clientid` field from the message's meta data and include it in the output message's payload. 

```sql
SELECT clientid FROM "t/#"
```

In case there is a conflict between the names of fields in the message's payload and the message's meta data, you can access the `payload` fields using the `payload.field_name` syntax. 

```sql
SELECT clientid, payload.clientid as myclientid FROM "t/#"
```

The `as` syntax exemplified above is used to give a field a name. A selected
field without an `as field_name` will be given the same name as the field in
the input message.

One can also attach rules to multiple topics by separating the topics with a comma:

```sql
SELECT clientid, payload.clientid as myclientid FROM "t/#", "my/other/topic"
```

One can not only attach rules to MQTT topics, but also to events. The following rule will be triggered when a client connects to the broker:

```sql
SELECT peername as ip_port FROM "$events/client_connected" WHERE clientid = 'c1'
```

You can find all available event topics in the EMQX dashboard UI for editing rules (under the Events tab to the right in the dashboard UI).

The `WHERE` clause is optional and can be used to filter out messages that match the topic filter specified with the `FROM` clause but do not match the conditions specified in the `WHERE` clause. For example, the following rule will only select messages that have a `username` field with a value of `eric`. The `SELECT *` clause specifies that all fields from the input message should be included in the output message.

```sql
SELECT * FROM "t/#" WHERE username = 'eric'
```

In the following example, only messages where the `clientid` is `'abc'` will be affected by the rule. The output message's payload has only one field named `cid`. Notice that the `cid` variable is defined in the `SELECT` statement (with the `as field_name` syntax), so it can be used in the `WHERE` clause. Although only one field is selected in the example below, all available fields in the message (such as clientid, username, etc.) can still be used in the `WHERE` clause. 
 
```sql
SELECT clientid as cid FROM "t/#" WHERE cid = 'abc'
```

The execution of the following SQL statement will result in an error when the field `xyz` is not a field in the message's payload and is not available in the message meta data:

```sql
SELECT clientid as cid FROM "#" WHERE xyz = 'abc'
```

The arguments for the `WHERE` clause is an expression that evaluates to a boolean value. The expression can be a combination of field names, constants, and operators. We will introduce expressions and the available operations in more details after we have introduced the `FOREACH` statement.

Expressions can also be used to transform data in the `SELECT` clause. For example, in the following example the value of the `clientid` field will be transformed to use capital letters and get a suffix. The result of the transformation is named `cid` in the output.

```sql
SELECT (upper(clientid) + '_UPPERCASE_LETTERS') as cid FROM "t/#"
```

The following showcase the use of a parenthesized arithmetic expression to transform data: 

```sql
SELECT (payload.integer_field + 2) * 2 as num FROM "t/#"
```

You can also use dot notation to access fields in a payload with a complex structure: 

```sql
SELECT payload.a.b.c.deep as my_field FROM "t/#"
```

## The FOREACH Statement

Now let us dive into the `FOREACH` statement. The `FOREACH` statement can be seen as a more general form of the `SELECT` statement. It can produce zero or more output messages for each input message. The basic format of the `FOREACH` statement is as follows:

```sql
FOREACH <expression_that_evaluates_to_array> [as <name>]
[DO <fields_expressions>]
[INCASE <condition>]
FROM <topic>
[WHERE <condition>]
```

- The `FOREACH` clause is used to specify an array. The `DO` and `INCASE` clauses will be executed for each element in the specified array. The array can either be specified as a field containing an array or as a function that returns an array. The built-in function `jq` can be combined with the `FOREACH` statement to transform data from the input message into an array that the `FOREACH` statement can process.
- The optional `DO` clause is used to transform each element in the array selected by `FOREACH` (that is, select fields of interest and apply built-in functions to transform data). The `DO` clause corresponds to the `SELECT` clause in the `SELECT` statement and accepts the same expressions. If the `DO` clause is omitted, then the array element as a whole will be included in the payload for the message corresponding to the array element.
- The optional `INCASE` clause can be used to filter out array elements that do not match the specified conditions. The `INCASE` clause accepts the same expressions as the `WHERE` clause. If the `INCASE` clause is omitted, then no array elements will be filtered out.
- The `FROM` and `WHERE` clause have the same meaning as in the `SELECT` statement.

As all but the `FOREACH` clause have corresponding clauses in the `SELECT` statement, the `FOREACH` statement can be seen as a generalization of the `SELECT` statement as mentioned earlier. The following two statements are equivalent:

```sql
FOREACH jq('.', payload) 
DO item.field_1, item.field_2 
FROM "t/#"
```

```sql
SELECT payload.field_1, payload.field_2
FROM "t/#"
```

The `FOREACH` example above uses the  build-in `jq` function to wrap the payload in an array. Please see the [documentation page for the build-in `jq` function](./rule-sql-jq) for more information about how to use it and for references to the JQ programming language. 

The following will give two output values. Both values contain only one field called `value`. The value of the field `value` is the value of the field `field_1` in one of the messages and the value of `field_2` in the other message:

```sql 
FOREACH jq('[.field_1, .field_2]', payload) 
DO item as value
FROM "t/#"
```

All actions specified for a rule with a `FOREACH` statement will be executed for each output value produced by the `FOREACH` statement. Thus, a rule with the above statement and a single action will trigger the action two times every time a message matches the rule.

Here is another example of a `FOREACH` statement that will split a message into multiple messages. The `FOREACH` clause specifies that the `sensors` field in the input message's payload should be used as the array to iterate over. The `DO` clause specifies that the `timestamp`, `clientid`, `name` and `idx` fields should be included in the output message's payload. The value for the `name` field is capitalized by the build-in function `upper`. The `INCASE` clause specifies that only array elements with an `idx` value greater than or equal to 1 should be included in the output message's payload. The `FROM` clause specifies that the rule should be applied to messages with a topic matching the [topic filter](https://www.emqx.com/en/blog/advanced-features-of-mqtt-topics) `t/#`.

```sql
FOREACH
    ## The data must be an array
    payload.sensors as sensor  
DO  ## The Do clause is used to select fields to the output message
    payload.timestamp,
    payload.client_id,
    upper(sensor.name) as name,
    sensor.idx as idx
INCASE
    sensor.idx >= 1
FROM "t/#"
```

The following example will also split a message into multiple messages. The `FOREACH` uses the JQ function to transform the input message's payload into an array. The `jq` function takes a JQ program as its first argument and the data to be processed by the JQ program as it's second argument. The JQ program in this example will transform the input message's payload into an array of objects with the following fields: `sensor_type`, `value`. The `DO` clause specifies that the `timestamp`, `clientid`, `sensor_type` and `value` fields should be included in the output message's payload. The `FROM` clause specifies that the rule should be applied to messages with a topic matching the [topic filter](https://www.emqx.com/en/blog/advanced-features-of-mqtt-topics) `car/measurements`.

```sql
FOREACH
    ## The data must be an array
    jq('
       [{
         sensor_type: "temprature",
         value: .temprature
        },
        {
         sensor_type: "humidity",
         value: .humidity
        },
        {
         sensor_type: "pressure",
         value: .pressure
        },
        {
         sensor_type: "light",
         value: .light
        },
        {
         sensor_type: "battery",
         value: .battery
        },
        {
         sensor_type: "speed",
         value: .speed
        }]',
        payload) as sensor  
DO
    payload.client_id,
    payload.timestamp,
    sensor.sensor_type,
    sensor.value
FROM "car/measurements"
```

The following example is equivalent to the previous example, but uses a different JQ program and no `DO` clause. This example is meant to illustrate that the JQ programs are very powerful and can be used to do any type of transformation. That being said, one should avoid doing computationally expensive transformations in EMQX rules as this can affect the performance of the EMQX broker. 

```sql
FOREACH
    jq('
       # Save the input
       . as $payload |
       
       # All sensor types
       [ 
         "temperature",
         "humidity",
         "pressure",
         "light",
         "battery",
         "speed" 
       ] as $sensor_types |
       
       # Output an object for each sensor type
       $sensor_types[] |
       {
         client_id: $payload.client_id,
         timestamp: $payload.timestamp,
         sensor_type: .,
         value: $payload[.] 
       }
       ',
       payload) as sensor  
FROM "car/measurements"
```

## Expressions and Operations 

The rule engine language allows the use of expressions to transform data and specify filtering conditions (as we have already given some examples of in the previous sections). These operations can be used in the `SELECT`, `FOREACH`, `DO`, `INCASE` and `WHERE` clauses. The "SQL Statement Examples" section below provides more information about how to use these expressions. We will now list the operations that can be used to form expressions. Also remember that the there is a rich set of build in functions that can also be included in the expressions.


### Arithmetic Operations

| Function | Purpose                                                                                               | Returned value              |      |
| -------- | ------------------------------------------------------------                                          | --------------------------- | ---- |
| `+`      | addition, or string concatenation                                                                     | Sum, or concatenated string |      |
| `-`      | Subtraction                                                                                           | Difference                  |      |
| `*`      | multiplication                                                                                        | product                     |      |
| `/`      | division                                                                                              | Quotient                    |      |
| `div`    | Integer division                                                                                      | Integer quotient            |      |
| `mod`    | modulus                                                                                               | module                      |      |


### Logical Operations

| Function | Purpose | Returned Value |
| ------ | ------------------- | ---------- |
| `>` | greater than | true/false |
| `<` | less than | true/false |
| `<=` | less than or equal | true/false |
| `>=` | greater than or equal | true/false |
| `<>` | not equal | true/false |
| `!=` | not equal | true/false |
| `=`      | Check if the two operands are completely equal. It can be used to compare values                      | true/false                  |
| `=~`     | Check if a topic can match the topic filter. It can only be used for topic matching                   | true/false                  |
| `and`    | logical and                                                                                           | true/false                  |
| `or`     | logical or                                                                                            | true/false                  |


### CASE Expressions 

The `CASE` expression can be used to perform conditional operations. A case expression corresponds to an if-then-else statement in other languages. How to use the `CASE` expression is illustrated by the following example.

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

Then the output will be:

```json
{"x": 7}
```
## More Examples

### Examples of SELECT Statements 

-  Extract all fields from the messages with the topic "t/a":
    ```sql
    SELECT * FROM "t/a"
    ```
-  Extract all fields from the messages with the topics "t/a" or "t/b":
    ```sql
    SELECT * FROM "t/a","t/b"
    ```
-  Extract all fields from the message with a topic that matches 't/#'.
    ```sql
    SELECT * FROM "t/#"
    ```
-  Extract the qos, username, and clientid fields from the input message with a topic that matches 't/#' (the output message will have a payload with the fields qos, username, and clientid):
    ```sql
    SELECT qos, username, clientid FROM "t/#"
    ```
-  Extract the username field from any message with a payload field named username with the value 'Steven' (it is not recommended to use the [topic filter](https://www.emqx.com/en/blog/advanced-features-of-mqtt-topics) '#' in the FROM clause as this means that the rule has to be checked for all messages that are sent to EMQX):
    ```sql
    SELECT username FROM "#" WHERE username='Steven'
    ```
- Extract the `x` field from the payload of the input message and rename the field to `y` in the output message. The new alias `y` for `payload.x` can also be used in the `WHERE` clause. A rule with this SQL statement matches messages with the payload `{"x": 1}` but not messages with the payload `{"x": 2}`:
    ```sql
    SELECT payload.x as x FROM "tests/test_topic_1" WHERE y = 1
    ```
- This SQL statement matches messages with the payload `{"x": {"y": 1}}` (and for example `{"x": {"y": 1}, "other": "field}`):
    ```sql
    SELECT * FROM "#" WHERE payload.x.y = 1
    ```
-  If an MQTT client with clientid = 'c1' connected, extract its source IP address and port number:
    ```sql
    SELECT peername as ip_port FROM "$events/client_connected" WHERE clientid = 'c1'
    ```
- Matches all subscriptions to topics that matches the pattern 't/topic' and have a quality of service (QoS) level of 1. It extracts the clientid to the output message.
    ```sql
    SELECT clientid FROM "$events/session_subscribed" WHERE topic = 'my/topic' and qos = 1
    ```
- Similar to the above example but here the topic match operator `=~` is used to match the [topic filter](https://www.emqx.com/en/blog/advanced-features-of-mqtt-topics) 't/#':
    ```sql
    SELECT clientid FROM "$events/session_subscribed" WHERE topic =~ 't/#' and qos = 1
    ```
- Extract the User Property with Key "foo" (User properties is new in the MQTT 5.0 protocol so this is not relevant for older MQTT versions):
    ```sql
    SELECT pub_props.'User-Property'.foo as foo FROM "t/#"
    ```

::: tip
- Topics in the `FROM` clause need to be enclosed in double quotes (`""`).
- The `WHERE` clause is followed by the filter condition. If a string is used in the condition, it needs to be enclosed in single quotes (`''`).
- If there are multiple topics in the FROM clause, they need to be separated by commas `","`. For example, `SELECT * FROM "topic1", "topic2" WHERE topic1.id = topic2.id`.

    ```sql
    SELECT * FROM "t/1", "t/2".
    ```
- You can use the period symbol (`.`) to access inner fields of the payload. For example, if the payload is a nested JSON structure, you can use `payload.outer_field.inner_field` to access the `inner_field` of the `outer_field`. 
:::

### Examples of FOREACH Statements

Suppose there are messages with client ID `c_steve` coming to the topic ` t/1`. The message body is in JSON format, and the sensors field is an array containing multiple objects as is shown in the following example:

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

#### Example 1

In this example it is required that each object in sensors array is re-published to the topic `sensors/${idx}` (where index is taken from the object) with the content of `${name}` (where name is taken from the object). That means that for for the example input given above the rule engine will issue the following three messages:

1. *Topic*: sensors/0
   *Content*: a
2. *Topic*: sensors/1
   *Content*: b
3. *Topic*: sensors/2
   *Content*: c

For the rule in this example, we need to configure the following action:

- *Action type*: message republish
- *Target topic*: `sensors/${idx}`
- *Target QoS*: 2 
- Message content template: `${name}`

And the following SQL statement:

```sql
FOREACH
    payload.sensors
FROM "t/#"
```

In the SQL statement above, the `FOREACH` clause specifies the array `sensors` that needs to be traversed. The `FOREACH` statement will perform a "message republish" action for each object in the result array, so the republish action will be performed three times.



#### Example 2

In this example, it is required that each object in the `sensors` array with a value for the `id` field which is greater than or equal to 1 is re-published to the topic `sensors/${idx}` with the content `clientid=${clientid},name=${name},date=${date}`. This means that the rule will issue two messages when given the example input message specified above (since the array element with the `id` field set to zero will be filtered out).

1. *Topic*: sensors/1
   *Content*: clientid=c_steve,name=b,date=2023-04-24
2. *Topic*: sensors/2
   *Content*: clientid=c_steve,name=c,date=2023-04-24

For the rule in this example, we need to configure the following action:

- *Action type*: message republish
- *Target topic*: `sensors/${idx}`
- *Target QoS*: 2 
- *Message content template*: `clientid=${clientid},name=${name},date=${date}`

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

In the above SQL statement, the `FOREACH` clause specifies that the array `sensors` needs to be traversed. The `DO` clause selects the fields required for each operation. The `clientid` field is selected from the message meta data and `name` and `idx` are selected from the current sensor object. The name `item` represents the current object in the sensors array. The `INCASE` clause specifies a filter condition for the array objects (objects that do not match the filter will be ignored).

In `DO` and `INCASE` clauses, you can use `item` to access the current object, or you can customize a variable name by using the `as` syntax in `FOREACH`. So the SQL statement in this example can also be written as follows:


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

#### Example 3

This extends Example 2 by also removing the `c_` prefix of `c_steve` in the clientid field.

The rule engine comes with a number of built in functions can be called in the `FOREACH`, `DO` and `INCASE` clauses. If you want to change `c_steve` into `steve`, you can change the SQL in Example 2 into:

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

Multiple expressions can be placed in the `FOREACH` clause as long as the last expression specifies the array to be traversed.
For example, if the input messages payload was formatted like this instead:

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

Then the `FOREACH` clause can give the payload data another name before selecting the array:

```sql
FOREACH
    payload.data as d
    d.sensors as s
...
```

This is equivalent to:

```sql
FOREACH
    payload.data.sensors as s
...
```

This feature can be useful when you are working with payloads that are structured in complex ways.



