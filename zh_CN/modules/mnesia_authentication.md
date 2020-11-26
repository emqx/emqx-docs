# 内置数据库 认证/访问控制

内置数据库认证使用 EMQ X 内置 Mnesia 数据库存储客户端 Clientid/Username 与密码，支持通过 HTTP API 管理认证数据。

内置数据库认证不依赖外部数据源，使用上足够简单轻量。

## 创建模块

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/modules)，点击左侧的 “模块” 选项卡，选择添加：

![image-20200928161310952](./assets/modules.png)

点击"选择",然后选择内置数据库认证模块

![image-20200928141305205](./assets/auth_mnesia_1.png)

配置相关参数

![image-20200927213049265](./assets/auth_mnesia_2.png)

最后点击“添加”按钮模块即可添加成功。

![image-20200928141558866](./assets/auth_mnesia_3.png)

## 管理数据

内置数据库可以通过 dashboard 管理认证与访问控制数据

![image-20200928141558866](./assets/auth_mnesia_4.png)

#### 认证数据

可以通过 dashboard 对认证数据进行管理

![image-20200928141558866](./assets/auth_mnesia_5.png)

当客户端连接 EMQ X 时，内置数据库认证会获取 CONNENT 报文中的 Clientid 与 Username，然后数据库中记录的密码进行匹配，一旦匹配成功则认证成功。

内置数据库认证还支持 HTTP API 操作，详情请查看 [Mnesia 认证](../advanced/auth-mnesia.md)。

#### 访问控制数据

可以通过 dashboard 对访问控制数据进行管理

![image-20200928141558866](./assets/auth_mnesia_6.png)

当客户端向 EMQ X 进行发布、订阅、取消订阅时，访问控制可以根据内置数据库中提前设置的规则允许或拒绝该次操作。

内置数据库访问控制还支持 HTTP API 操作，详情请查看 [Mnesia ACL](../advanced/acl-mnesia.md)。
