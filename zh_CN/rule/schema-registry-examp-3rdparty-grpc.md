# 编解码举例 - 自定义 gRPC 编解码

## 规则需求

设备发布一个任意的消息，验证自部署的编解码服务能正常工作。

## 创建 Parser gRPC 资源

在 EMQX Dashboard 的 [Resource 创建](http://127.0.0.1:18083/#/resources) 界面，使用下面的参数创建一个 Parser gRPC 资源：

- URL: http://127.0.0.1:50051
- Resource ID: my_grpc_parser_resource

## 创建 Schema

在 EMQX Dashboard 的 [Schema 创建](http://127.0.0.1:18083/#/schemas/0?oper=create) 界面，使用下面的参数创建一个 3rd-Party Schema:

1. 名称: my_grpc_parser
2. 编解码类型: 3rd-party
3. 第三方类型: Resources
4. Resource: my_grpc_parser_resource（这里选择我们刚才创建的 Parser gRPC 资源）

其他配置保持默认。

## 创建规则

**使用刚才创建好的 Schema 来编写规则 SQL 语句：**

```sql
SELECT

  schema_encode('my_grpc_parser', payload) as encode_resp,
  schema_decode('my_grpc_parser', encode_resp.result) as decode_resp

FROM

  "t/#"
```

这个 SQL 语句首先对数据做了 Encode，然后又做了 Decode，目的在于验证编解码过程是否正确:

- `schema_encode` 函数将 payload 字段的内容按照 'my_grpc_parser' 这个 Schema 来做编码，结果存储到 `encode_resp` 这个 Map 里;
- `schema_decode` 函数将编码结果内容按照 'my_grpc_parser' 这个 Schema 来做解码，结果存储到 `decode_resp` 这个变量里;

最终这个 SQL 语句的筛选结果是 `encode_resp` 和 `decode_resp` 这两个变量。

**然后使用以下参数添加动作：**

- 动作类型：检查(调试)

这个检查动作会把 SQL 语句筛选的结果打印到 emqx 控制台 (erlang shell) 里。

如果是使用 emqx console 启动的服务，打印会直接显示在控制台里；如果是使用 emqx start 启动的服务，打印会输出到日志目录下的 erlang.log.N 文件里，这里 "N" 为整数，比如 "erlang.log.1", "erlang.log.2"。

## 编解码服务端代码

规则创建好之后，就可以模拟数据进行测试了。所以首先需要编写一个自己的编解码服务。

下面的代码使用 Python 语言实现了一个 gRPC 编解码服务。
为简单起见，这个服务在加密时对原始字符做 base64_encode，解密时对字符串进行 base64_decode
详见 [完整代码](https://github.com/terry-xiaoyu/emqx-schema-grpc-python-server):

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

将这个服务运行起来:

```
pip3 install grpcio
pip3 install grpcio-tools
python3 -m grpc_tools.protoc -I./protos --python_out=. --grpc_python_out=. ./protos/emqx_schema_registry.proto

python3 emqx_schema_registry_server.py
```

## 检查规则执行结果

由于本示例比较简单，我们直接使用 MQTT Websocket 客户端来模拟设备端发一条消息。

1) 在 Dashboard 的 [Websocket](http://127.0.0.1:18083/#/websocket) 工具里，登录一个 MQTT Client 并发布一条消息到 "t/1"，内容为 "hello"。

2) 检查 emqx 控制台 (erlang shell) 里的打印:

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

Select Data 是经过 SQL 语句筛选之后的数据，Envs 是规则引擎内部可用的环境变量，Action Init Params 是动作的初始化参数。这三个数据均为 `Map` 格式。

Selected Data 里面的两个字段 `decode_resp` 和 `encode_resp` 对应 SELECT 语句里面的两个 AS。

因为 `decode_resp` 是编码然后再解码之后的结果，所以它又被还原为了我们发送的内容 "hello"，表明编解码插件工作正常。
