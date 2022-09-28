# 3rd-Party Custom codec example - HTTP

## Rule requirements

The device publishes an arbitrary message to verify that the self-deployed codec service is working normally.

## Create Schema

In the [Dashboard](http://127.0.0.1:18083/#/schemas/0?oper=create) interface of EMQX, create a 3rd-Party Schema using the following parameters:

1. Name: my_parser
2. Codec Type: 3rd-party
3. Third Party Type: HTTP
4. URL: http://127.0.0.1:9003/parser
5. Codec Configuration: xor

All other configurations remain default.

Item 5 (i.e. codec configuration) above is optional and is a string, the content of which is related to the service of the codec service.

## Creating rules

**Use the Schema you have just created to write the rule SQL statement:**

```sql
SELECT
  schema_encode('my_parser', payload) as encoded_data,
  schema_decode('my_parser', encoded_data) as decoded_data
FROM
  "t/#"
```

This SQL statement first encodes and then decodes the data to verify that the encoding and decoding process is correct:

- The `schema_encode` function encodes the contents of the payload field according to the Schema 'my_parser' and stores the result in the variable `encoded_data`;
- The `schema_decode` function decodes the contents of the payload field according to the Schema 'my_parser' and stores the result in the variable `decoded_data`;

The final filtered result of this SQL statement is the variables `encoded_data` and `decoded_data`.

**Then add the action using the following parameters:**

- Action Type: Check (debug)

This check action prints the results filtered by the SQL statement to the emqx console (erlang shell).

If the service is started with emqx console, the print will be displayed directly in the console; if the service is started with emqx start, the print will be output to the erlang.log.N file in the log directory, where "N" is an integer, e.g. "erlang.log.1", " erlang.log.2".

## Codec server-side code

Once the rules have been created, it is time to simulate the data for testing. Therefore, the first thing you need to do is write your own codec service.

The following code implements an HTTP codec service using the Python language. For simplicity, this service provides two simple ways of coding and decoding (encryption and decryption). See [full code](https://github.com/terry-xiaoyu/schema-registry-examples/blob/master/3rd_party/http_parser_server.py) for details.

- xor
- Character substitution

```python
def xor(data):
  """
  >>> xor(xor(b'abc'))
  b'abc'
  >>> xor(xor(b'!}~*'))
  b'!}~*'
  """
  length = len(data)
  bdata = bytearray(data)
  bsecret = bytearray(secret * length)
  result = bytearray(length)
  for i in range(length):
    result[i] = bdata[i] ^ bsecret[i]
  return bytes(result)

def subst(dtype, data, n):
  """
  >>> subst('decode', b'abc', 3)
  b'def'
  >>> subst('decode', b'ab~', 1)
  b'bc!'
  >>> subst('encode', b'def', 3)
  b'abc'
  >>> subst('encode', b'bc!', 1)
  b'ab~'
  """
  adata = array.array('B', data)
  for i in range(len(adata)):
    if dtype == 'decode':
      adata[i] = shift(adata[i], n)
    elif dtype == 'encode':
      adata[i] = shift(adata[i], -n)
  return bytes(adata)
```

Run this service:

```shell
$ pip3 install flask
$ python3 http_parser_server.py
 * Serving Flask app "http_parser_server" (lazy loading)
 * Environment: production
   WARNING: This is a development server. Do not use it in a production deployment.
   Use a production WSGI server instead.
 * Debug mode: off
 * Running on http://127.0.0.1:9003/ (Press CTRL+C to quit)
```

## Checking rule execution results

Since this example is relatively simple, we'll use the MQTT Websocket client directly to simulate sending a message on the device side.

1) In the Dashboard's [Websocket](http://127.0.0.1:18083/#/websocket) tools, log in to an MQTT Client and publish a message to "t/1" with the text "hello".

2) Check what is printed in the emqx console (erlang shell):

```bash
(emqx@127.0.0.1)1> [inspect]
        Selected Data: #{decoded_data => <<"hello">>,
                         encoded_data => <<9,4,13,13,14>>}
        Envs: #{event => 'message.publish',
                flags => #{dup => false,retain => false},
                from => <<"mqttjs_76e5a35b">>,
                headers =>
                    #{allow_publish => true,
                      peername => {{127,0,0,1},54753},
                      username => <<>>},
                id => <<0,5,146,30,146,38,123,81,244,66,0,0,62,117,0,1>>,
                node => 'emqx@127.0.0.1',payload => <<"hello">>,qos => 0,
                timestamp => {1568,34882,222929},
                topic => <<"t/1">>}
        Action Init Params: #{}
```

Select Data is the data filtered by the SQL statement, Envs are available environment variables within the rule engine and Action Init Params is the initialization parameters for actions. All three data are in `Map` format.

The two fields `decoded_data` and `encoded_data` in Selected Data correspond to the two ASs in the SELECT statement. Because `decoded_data` is the result of encoding and then decoding, it is reverted to the content we sent, "hello", indicating that the codec plugin is working correctly.
