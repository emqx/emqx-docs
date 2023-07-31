# HTTP 服务

EMQX 支持通过 HTTP 服务的方式将 MQTT 消息和客户端事件发送到外部 HTTP 服务。

:::tip 前置准备

- 了解[规则](./rules.md)。
- 了解[数据桥接](./data-bridges.md)。

:::

## 功能清单

- [连接池](./data-bridges.md#连接池)
- [缓存队列](./data-bridges.md#缓存队列)
- [异步请求模式](./data-bridges.md#异步请求模式)

## 快速开始

我们将通过示例来展示如何使用 Dashboard 创建一个简单的 HTTP 服务，并桥接到一个 HTTP 服务器。

### 搭建简易 HTTP 服务

首先我们使用 Python 搭建一个简单的 HTTP 服务，用来接收 `POST /` 请求，该服务打印请求内容后返回 200 OK：

```python
from flask import Flask, json, request

api = Flask(__name__)

@api.route('/', methods=['POST'])
def print_messages():
  reply= {"result": "ok", "message": "success"}
  print("got post request: ", request.get_data())
  return json.dumps(reply), 200

if __name__ == '__main__':
  api.run()
```

将上面的代码保存为 `http_server.py` 文件，文件所在目录运行如下命令：

```shell
# 安装 flask 依赖
pip install flask

# 启动服务
python3 http_server.py
```

### 创建 HTTP 服务数据桥接

1. 转到 Dashboard **数据集成** -> **数据桥接**页面。
2. 点击页面右上角的**创建**。
3. 在数据桥接类型中选择 **HTTP 服务**，点击**下一步**。
4. 输入数据桥接名称，要求是大小写英文字母和数字的组合，这里我们输入 `my_httpserver`。
5. 请求方法选择 POST，URL 为 `http://localhost:5000`，其他使用默认值即可。
6. 点击最下方**创建**按钮完成规则创建。

至此您已经完成数据桥接创建，接下来将继续创建一条规则来指定需要写入的数据。

### 创建数据转发规则

1. 转到 Dashboard **数据集成** -> **规则**页面。
2. 点击页面右上角的**创建**。
3. 输入规则 ID `my_rule`，在 SQL 编辑器中输入规则，此处选择将 `t/#` 主题的 MQTT 消息发送到 HTTP 服务，此处规则 SQL 如下：

  ```sql
  SELECT 
    *
  FROM
    "t/#"
  ```
4. 添加动作，在动作下拉框中选择**使用数据桥接转发**选项，选择先前创建好的 HTTP 服务。
5. 点击最下方**创建**按钮完成规则创建。

至此您已经完成整个创建过程，可以前往 **数据集成** -> **Flows** 页面查看拓扑图，此时应当看到 `t/#` 主题的消息经过名为 `my_rule` 的规则处理，处理结果转发到 HTTP 服务。

### 测试数据桥接与规则

使用 MQTTX 向 `t/1` 主题发布消息，此操作同时会触发上下线事件：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello HTTP Server" }'
```

查看 HTTP 服务 运行统计，命中、发送成功次数均 +1。

查看消息是否已经转发到 HTTP 服务：

```shell
python3 http_server.py
 * Serving Flask app 'http_server' (lazy loading)
 * Environment: production
   WARNING: This is a development server. Do not use it in a production deployment.
   Use a production WSGI server instead.
 * Debug mode: off
 * Running on http://127.0.0.1:5000 (Press CTRL+C to quit)

got post request:  b'hello HTTP Server'
```
