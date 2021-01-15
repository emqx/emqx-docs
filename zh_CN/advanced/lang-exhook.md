---
# 编写日期
date: 2020-09-12 09:15:26
# 作者 Github 名称
author: hjianbo
# 关键字
keywords:
# 描述
description:
# 分类
category: 
# 引用
ref:
---

# 多语言 - 钩子扩展

多语言的 **钩子扩展** 由 [emqx-extension-hook](https://github.com/emqx/emqx-extension-hook) 插件进行支持。该插件在 4.1.0 中首次引入。它允许用户使用其它编程语言处理 EMQ X 的 [钩子(Hooks)](hooks.md)。例如：

- 校验某客户端的登录权限。
- 校验某客户端 PUB/SUB 的操作权限。
- 处理消息类事件，并消息桥接、转发或存储到其它的系统。

注：消息类钩子仅在企业版中支持。
注：4.1 到 4.2 版本中仅实现 Python, Java 的支持。

## 设计
EMQ X 发行包中不包含其它语言运行时环境的支持。它通过 EMQ X 提供的该语言的 驱动(Drivers) 进行通信和过程调用(Remote Process Call).

`emqx-extension-hook` 通过过程调用将 EMQ X 中的钩子事件，直接触发到某语言某个具体的函数，并得到其函数的返回值，作为事件的处理结果。

如下图所示：

```
 EMQ X                                      Third-party Runtimes
+========================+                 +====================+ 
|    Extension           |                 |                    |
|   +----------------+   |     Hooks       |  Python scripts /  |
|   |    Drivers     | ------------------> |  Java Classes   /  |
|   +----------------+   |     (pipe)      |  Others ...        |
|                        |                 |                    |
+========================+                 +====================+

```

因此，它要求：

- EMQ X 的宿主机，具备某语言的运行时环境，并已经配置到系统的环境变量中。
- 必须将脚本（或编译后的代码）、资源文件等，放到 emqx-extension-hook 指示的路径。
- 用户代码的实现，若包含三方依赖、库等，它应该包含在 emqx-extension-hook 对其的搜索路径中。

### 驱动

驱动(Drivers) 实现了 Erlang 和 其它语言（例如：Python, Java）间的过程调用和通信。它基于 [Erlang - Port](http://erlang.org/doc/tutorial/c_port.html) 进行实现。

例如：Java 语言驱动的实现包括两部分的内容：
- Erlang 侧的实现，它包含如何启动其他语言的运行时系统、和分发请求、处理结果等。
- Java 侧的实现，它包含如何和 Erlang 虚拟机通信，如何分发函数调用等。

```
 Erlang VM                       Third Runtimes (e.g: Java VM)
+===========+=========+         +=========+================+
| Extension | Driver  | <=====> |  Driver | User's Codes   |
+===========+=========+         +=========+================+
```

### SDK
为了方便用户的开发，我们对每类的语言都提供了对应的 SDK 支持。

对于用户开发自己的代码来说，SDK 并不是必须的，但它封装底层的比较晦涩的数据格式和方法，屏蔽底层细节。直接提供了更为优好的 API 和数据类型供用户使用。

从依赖的层级关系来说：

```
+---------------------+
|     User's Codes    |
+---------------------+
|         SDK         |  <====  The SDK Located
+---------------------+
|       Raw APIs      |
+---------------------+
|        Driver       |
+=====================+
           ||
+=====================+
|        Erlang       |
+---------------------+
```

对于 EMQ X 来说，`Raw APIs` 及往下的部分都属于 `emqx-extension-hook` 插件所包含的内容，并已包含在 EMQ X 的发行包中；往上的 `SDK` 和 `Users's Codes` 都属于用户使用的编程语言，需要额外部署到 EMQ X 的代码和资源。

`Raw APIs` 可参考 [emqx-extension-hook - examples](https://github.com/emqx/emqx-extension-hook/tree/master/test/scripts)

目前对于 `emqx-extension-hook` 提供的 SDK 有：

- Python: https://github.com/emqx/emqx-extension-python-sdk
- Java: https://github.com/emqx/emqx-extension-java-sdk

注：SDK 版本与 EMQ X 的第二位版本号进行兼容。例如，在 EMQ X v4.1.4 中，应该使用 v4.1.x 的 SDK

## 快速上手

### Python

参考：https://www.emqx.cn/blog/develop-emqx-plugin-using-python

### Java

参考：https://www.emqx.cn/blog/develop-emqx-plugin-using-java
