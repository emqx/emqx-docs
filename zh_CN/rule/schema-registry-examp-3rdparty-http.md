# 编解码举例 - 自定义 HTTP 编解码

## 规则需求

设备发布一个任意的消息，验证自部署的编解码服务能正常工作。

## 创建 Parser HTTP 资源

在 EMQX Dashboard 的 [Resource 创建](http://127.0.0.1:18083/#/resources) 界面，使用下面的参数创建一个 Parser HTTP 资源：

- URL: http://127.0.0.1:9003/parser
- Request Method: POST

其他保持默认，点击创建之后，得到 Resource ID，比如：resource:606631

## 创建 Schema

在 EMQX Dashboard 的 [Schema 创建](http://127.0.0.1:18083/#/schemas/0?oper=create) 界面，使用下面的参数创建一个 3rd-Party Schema:

1. 名称: my_http_parser
2. 编解码类型: 3rd-party
3. 第三方类型: Resources
4. Resource: resource:606631（这里选择我们刚才创建的 Parser HTTP 资源）
5. 编解码配置: xor

其他配置保持默认。

上面第 5 项编解码配置是个可选项，是个字符串，内容跟编解码服务的业务相关。

## 创建规则

**使用刚才创建好的 Schema 来编写规则 SQL 语句：**

```sql
SELECT
  schema_encode('my_http_parser', payload) as encoded_data,
  schema_decode('my_http_parser', encoded_data) as decoded_data
FROM
  "t/#"
```

这个 SQL 语句首先对数据做了 Encode，然后又做了 Decode，目的在于验证编解码过程是否正确:

- `schema_encode` 函数将 payload 字段的内容按照 'my_http_parser' 这个 Schema 来做编码，结果存储到 `encoded_data` 这个变量里;
- `schema_decode` 函数将 payload 字段的内容按照 'my_http_parser' 这个 Schema 来做解码，结果存储到 `decoded_data` 这个变量里;

最终这个 SQL 语句的筛选结果是 `encoded_data` 和 `decoded_data` 这两个变量。

**然后使用以下参数添加动作：**

- 动作类型：检查(调试)

这个检查动作会把 SQL 语句筛选的结果打印到 emqx 控制台 (erlang shell) 里。

如果是使用 emqx console 启动的服务，打印会直接显示在控制台里；如果是使用 emqx start 启动的服务，打印会输出到日志目录下的 erlang.log.N 文件里，这里 "N" 为整数，比如 "erlang.log.1", "erlang.log.2"。

## 编解码服务端代码

规则创建好之后，就可以模拟数据进行测试了。所以首先需要编写一个自己的编解码服务。

下面的代码使用 Python 语言实现了一个 HTTP 编解码服务，为简单起见，这个服务提供两种简单的方式来进行编解码(加解密)，详见 [完整代码](https://github.com/terry-xiaoyu/schema-registry-examples/blob/master/3rd_party/http_parser_server.py):

- 按位异或
- 字符替换

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

将这个服务运行起来:

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

## 检查规则执行结果

由于本示例比较简单，我们直接使用 MQTT Websocket 客户端来模拟设备端发一条消息。

1) 在 Dashboard 的 [Websocket](http://127.0.0.1:18083/#/websocket) 工具里，登录一个 MQTT Client 并发布一条消息到 "t/1"，内容为 "hello"。

2) 检查 emqx 控制台 (erlang shell) 里的打印:

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

Select Data 是经过 SQL 语句筛选之后的数据，Envs 是规则引擎内部可用的环境变量，Action Init Params 是动作的初始化参数。这三个数据均为 `Map` 格式。

Selected Data 里面的两个字段 `decoded_data` 和 `encoded_data` 对应 SELECT 语句里面的两个 AS。因为 `decoded_data` 是编码然后再解码之后的结果，所以它又被还原为了我们发送的内容 "hello"，表明编解码插件工作正常。
