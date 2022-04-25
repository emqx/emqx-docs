# 3rd-Party Custom codec example - gRPC

## Rule requirements

The device publishes an arbitrary message to verify that the self-deployed codec service is working normally.

## Create a resource Parser gRPC

In the EMQX Dashboard 的 [Resource 创建](http://127.0.0.1:18083/#/resources) interface of EMQX, create a Parser gRPC resource using the following parameters:

- URL: http://127.0.0.1:50051
- Resource ID: my_grpc_parser_resource

## Create Schema

In the [Dashboard](http://127.0.0.1:18083/#/schemas/0?oper=create) interface of EMQX, create a 3rd-Party Schema using the following parameters:

1. Name: my_grpc_parser
2. Codec Type: 3rd-party
3. Third Party Type: Resources
4. Resource: my_grpc_parser_resource (select the Parser gRPC resource we created just now)

All other configurations remain default.

## Creating rules

**Use the Schema you have just created to write the rule SQL statement:**

```sql
SELECT

  schema_encode('my_grpc_parser', payload) as encode_resp,
  schema_decode('my_grpc_parser', encode_resp.result) as decode_resp

FROM

  "t/#"
```

This SQL statement first encodes and then decodes the data to verify that the encoding and decoding process is correct:

- The `schema_encode` function encodes the contents of the payload field according to the Schema 'my_grpc_parser' and stores the result in the variable `encode_resp`;
- The `schema_decode` function decodes the contents of the payload field according to the Schema 'my_grpc_parser' and stores the result in the variable `decode_resp`;

The final filtered result of this SQL statement is the variables `encode_resp` and `decode_resp`.

**Then add the action using the following parameters:**

- Action Type: Check (debug)

This check action prints the results filtered by the SQL statement to the emqx console (erlang shell).

If the service is started with emqx console, the print will be displayed directly in the console; if the service is started with emqx start, the print will be output to the erlang.log.N file in the log directory, where "N" is an integer, e.g. "erlang.log.1", " erlang.log.2".

## Codec server-side code

Once the rules have been created, it is time to simulate the data for testing. Therefore, the first thing you need to do is write your own codec service.

The following code implements an gRPC codec service using the Python language. For simplicity, this service just do base64_encode on received string when encoding, and do base64_decode
when decoding.
See [full code](https://github.com/terry-xiaoyu/emqx-schema-grpc-python-server) for details.

```python
class Parser(emqx_schema_registry_pb2_grpc.ParserServicer):
    def HealthCheck(self, request, context):
        return request
    def Parse(self, request, context):
        if request.type == 1:
            print("parser got encode request: ", request)
            encoded_d = base64.b64encode(request.data)
            return emqx_schema_registry_pb2.ParseResponse(code='SUCCESS', message="ok",
                result=encoded_d)
        elif request.type == 0:
            print("parser got decode request: ", request)
            decoded_d = base64.b64decode(request.data)
            return emqx_schema_registry_pb2.ParseResponse(code='SUCCESS', message="ok",
                result=decoded_d)

def serve():
    server = grpc.server(futures.ThreadPoolExecutor(max_workers=10))
    emqx_schema_registry_pb2_grpc.add_ParserServicer_to_server(
        Parser(), server)
    server.add_insecure_port('[::]:50051')
    server.start()
    server.wait_for_termination()

if __name__ == '__main__':
    logging.basicConfig()
    serve()
```

Run this service:

```shell
pip3 install grpcio
pip3 install grpcio-tools
python3 -m grpc_tools.protoc -I./protos --python_out=. --grpc_python_out=. ./protos/emqx_schema_registry.proto

python3 emqx_schema_registry_server.py
```

## Checking rule execution results

Since this example is relatively simple, we'll use the MQTT Websocket client directly to simulate sending a message on the device side.

1) In the Dashboard's [Websocket](http://127.0.0.1:18083/#/websocket) tools, log in to an MQTT Client and publish a message to "t/1" with the text "hello".

2) Check what is printed in the emqx console (erlang shell):

```bash
(emqx@127.0.0.1)1> [inspect]
        Selected Data: #{<<"decode_resp">> =>
                             #{code => 'SUCCESS',message => <<"ok">>,
                               result => <<"hello">>},
                         <<"encode_resp">> =>
                             #{code => 'SUCCESS',message => <<"ok">>,
                               result => <<"aGVsbG8=">>}}
        Envs: #{'__bindings__' =>
                    #{'Id' => <<"inspect_1649928007719256000">>,
                      'Params' => #{}},
                clientid => <<"mqttjs_4c8818ae">>,event => 'message.publish',
                flags => #{dup => false,retain => false},
                headers =>
                    #{peerhost => <<"127.0.0.1">>,properties => #{},
                      proto_ver => 4,protocol => mqtt,username => <<>>},
                id => <<"0005DC99CDA113B6F44200000CEB0001">>,
                metadata => #{rule_id => <<"rule:440083">>},
                node => 'emqx@127.0.0.1',payload => <<"hello">>,
                peerhost => <<"127.0.0.1">>,pub_props => #{},
                publish_received_at => 1649928021545,qos => 0,
                timestamp => 1649928021545,topic => <<"t/1">>,
                username => <<>>}
        Action Init Params: #{}
```

Select Data is the data filtered by the SQL statement, Envs are available environment variables within the rule engine and Action Init Params is the initialization parameters for actions. All three data are in `Map` format.

The two fields `decode_resp` and `encode_resp` in Selected Data correspond to the two ASs in the SELECT statement.

Because `decode_resp` is the result of encoding and then decoding, it is reverted to the content we sent, "hello", indicating that the codec plugin is working correctly.
