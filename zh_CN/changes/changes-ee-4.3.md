---
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
ref:
---

# 版本发布

## 4.3.0 版本

*发布日期: 2021-05-19*

### 增强

- 规则引擎消息桥接到 Kafka 分区支持动态扩容
- 规则引擎支持 ClickHouse 离线消息与代理订阅
- 规则引擎中支持批量操作动作默认启用批量异步模式
- 规则引擎数据保存到 InfluxDB 重构以增强性能
- 模块 消息下发 Kafka 消费组支持配置 Payload 格式

### 修复

- 规则引擎动作编辑数据不一致问题
- Dashboard 模块翻译问题
- 规则引擎 SQL语句支持 null 函数，undefined 转成 null
