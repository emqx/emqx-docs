# 离线消息保存到 PostgreSQL

搭建 PostgreSQL 数据库，以 MacOS X 为例:
```bash
$ brew install postgresql
$ brew services start postgresql
```

创建 mqtt 数据库:

```
# 使用用户名 postgres 创建名为 'mqtt' 的数据库
$ createdb -U postgres mqtt

$ psql -U postgres mqtt

mqtt=> \dn;
List of schemas
Name  | Owner
--------+-------
public | postgres
(1 row)
```

创建 mqtt_msg 表:

```sql
$ psql -U postgres mqtt

CREATE TABLE mqtt_msg (
  id SERIAL8 primary key,
  msgid character varying(64),
  sender character varying(64),
  topic character varying(255),
  qos integer,
  retain integer,
  payload text,
  arrived timestamp without time zone
);
```

::: tip

消息表结构不能修改，请使用上面SQL语句创建

:::

创建规则:

打开 [EMQX Dashboard](http://127.0.0.1:18083/#/rules)，选择左侧的“规则”选项卡。

然后填写规则 SQL:

FROM说明

​	**t/#**: 发布者发布消息触发保存离线消息到PostgreSQL

​	**$events/session_subscribed**: 订阅者订阅主题触发获取离线消息

​	**$events/message_acked**: 订阅者回复消息ACK后触发删除已经被接收的离线消息

```bash
SELECT * FROM "t/#", "$events/session_subscribed", "$events/message_acked" WHERE topic =~ 't/#'
```

![](./assets/rule-engine/pg_offline_msg_01.png)

关联动作:

在“响应动作”界面选择“添加动作”，然后在“动作”下拉框里选择“离线消息保存到 PostgreSQL”。

![](./assets/rule-engine/pg_offline_msg_02.png)


现在资源下拉框为空，可以点击右上角的 “新建” 来创建一个 PostgreSQL 资源:

![](./assets/rule-engine/pg_offline_msg_03.png)

弹出一个“创建资源”对话框

![](./assets/rule-engine/pg_offline_msg_04.png)

填写资源配置:

填写真实的 PostgreSQL 服务器地址，其他配置填写相应的值，然后点击 “测试连接” 按钮，确保连接测试成功。

最后点击 “确定” 按钮。

![](./assets/rule-engine/pg_offline_msg_05.png)

返回响应动作界面，点击 “确认”。

![](./assets/rule-engine/pg_offline_msg_06.png)

返回规则创建界面，点击 “创建”。

![](./assets/rule-engine/pg_offline_msg_07.png)

规则已经创建完成，通过 Dashboard 的 WebSocket 客户端发一条数据**(发布消息的QoS必须大于0)**:

![](./assets/rule-engine/pg_offline_msg_08.png)

消息发送后，通过 psql 查看到消息被保存到 PostgreSQL 里面:

![](./assets/rule-engine/pg_offline_msg_09.png)

使用另外一个客户端，订阅主题 "t/1" (订阅主题的QoS必须大于0，否则消息会被重复接收):

![](./assets/rule-engine/pg_offline_msg_10.png)

订阅后马上接收到了保存到 PostgreSQL 里面的离线消息:

![](./assets/rule-engine/pg_offline_msg_11.png)

离线消息被接收后会在 PostgreSQL 中删除:

![](./assets/rule-engine/pg_offline_msg_12.png)
