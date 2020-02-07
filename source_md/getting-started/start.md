---
# 标题
title: 启动 EMQ X
# 编写日期
date: 2020-02-07 17:15:26
# 作者 Github 名称
author: wivwiv
# 关键字
keywords:
# 描述
description:
# 分类
category: 
# 引用
ref: undefined
---

# 启动 EMQ X

控制台启动编译的 EMQ 程序包

``` sourceCode bash
cd _build/emqx/rel/emqx
emqx console
```

注册 windows 服务

``` sourceCode bash
cd _build/emqx/rel/emqx
emqx install
```


启动 EMQ X
    
      - 直接启动
        
        ``` sourceCode console
        $ emqx start
        emqx 4.0.0 is started successfully!
        
        $ emqx_ctl status
        Node 'emqx@127.0.0.1' is started
        emqx v4.0.0 is running
        ```
    
      - systemctl 启动
        
        ``` sourceCode console
        $ sudo systemctl start emqx
        ```
    
      - service 启动
        
        ``` sourceCode console
        $ sudo service emqx start
        ```