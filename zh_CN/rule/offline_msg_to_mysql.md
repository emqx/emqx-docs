# 离线消息保存到 MySQL

搭建 MySQL 数据库，并设置用户名密码为 root/public，以 MacOS X 为例:
```bash
$ brew install mysql

$ brew services start mysql

$ mysql -u root -h localhost -p

ALTER USER 'root'@'localhost' IDENTIFIED BY 'public';
```

初始化 MySQL 数据库:
```bash
$ mysql -u root -h localhost -ppublic

create database mqtt;
```

创建 mqtt_msg 表:
```sql
DROP TABLE IF EXISTS `mqtt_msg`;
CREATE TABLE `mqtt_msg` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `msgid` varchar(64) DEFAULT NULL,
  `topic` varchar(180) NOT NULL,
  `sender` varchar(64) DEFAULT NULL,
  `qos` tinyint(1) NOT NULL DEFAULT '0',
  `retain` tinyint(1) DEFAULT NULL,
  `payload` blob,
  `arrived` datetime NOT NULL,
  PRIMARY KEY (`id`),
  INDEX topic_index(`id`, `topic`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8MB4;
CREATE TABLE `mqtt_acked` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `clientid` varchar(64) DEFAULT NULL,
  `topic` varchar(180) DEFAULT NULL,
  `mid` int(11) unsigned DEFAULT NULL,
  `created` timestamp NULL DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `mqtt_acked_key` (`clientid`,`topic`),
  INDEX topic_index(`id`, `topic`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8MB4;
```

::: danger

消息表结构不能修改，请使用上面SQL语句创建

:::

创建规则:

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，选择左侧的“规则”选项卡。

然后填写规则 SQL:

FROM说明

​	**t/#**: 发布者发布消息触发保存离线消息到MySQL

​	**$events/session_subscribed**: 订阅者订阅主题触发获取离线消息

​	**$events/message_acked**: 订阅者回复消息ACK后触发删除已经被接收的离线消息

```bash
SELECT * FROM "t/#", "$events/session_subscribed", "$events/message_acked" WHERE topic =~ 't/#'
```

![](./assets/rule-engine/mysql_offline_msg_01.png)

关联动作:

在“响应动作”界面选择“添加动作”，然后在“动作”下拉框里选择“离线消息保存到 MySQL”。

![](./assets/rule-engine/mysql_offline_msg_02.png)


现在资源下拉框为空，可以点击右上角的 “新建” 来创建一个 MySQL 资源:

![](./assets/rule-engine/mysql_offline_msg_03.png)

弹出一个“创建资源”对话框

![](./assets/rule-engine/mysql_offline_msg_04.png)

填写资源配置:

填写真实的 MySQL 服务器地址，其他配置填写相应的值，然后点击 “测试连接” 按钮，确保连接测试成功。

最后点击 “确定” 按钮。

![](./assets/rule-engine/mysql_offline_msg_05.png)

返回响应动作界面，点击 “确认”。

![](./assets/rule-engine/mysql_offline_msg_06.png)

返回规则创建界面，点击 “创建”。

![](./assets/rule-engine/mysql_offline_msg_07.png)

规则已经创建完成，通过 Dashboard 的 WebSocket 客户端发一条数据**(发布消息的QoS必须大于0)**:

![](./assets/rule-engine/mysql_offline_msg_08.png)

消息发送后，通过 mysql 查看到消息被保存到 MySQL 里面:

![](./assets/rule-engine/mysql_offline_msg_09.png)

使用另外一个客户端，订阅主题 "t/1" (订阅主题的QoS必须大于0，否则消息会被重复接收):

![](./assets/rule-engine/mysql_offline_msg_10.png)

订阅后马上接收到了保存到 MySQL 里面的离线消息:

![](./assets/rule-engine/mysql_offline_msg_11.png)

离线消息被接收后会在 MySQL 中删除:

![](./assets/rule-engine/mysql_offline_msg_12.png)