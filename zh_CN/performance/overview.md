# 性能调优及测试

本章将介绍如何进行系统性能调优及测试。

- [系统调优](./tune.md)

  提供生产部署与测试所需的 Linux 内核参数，网络协议栈参数，Erlang 虚拟机参数， EMQX 参数设置。

- [使用 eMQTT-Bench 进行性能测试](./benchmark-emqtt-bench.md)

  将介绍如何安装与使用 [emqtt_bench](https://github.com/emqx/emqtt_bench)，一款简洁强大的 MQTT 协议性能测试工具。

- [使用 XMeter Cloud 进行性能测试](./benchmark-xmeter.md)

  将介绍如何使用全托管的 MQTT 负载测试云服务 [XMeter Cloud](https://www.emqx.com/zh/products/xmeter) 和其他工具对 EMQX 进行性能测试。

- [EMQX 性能参考](./performance-reference.md)

  列出了 EMQX 在几个典型场景下的性能变化曲线，并揭示了 QoS 等级、Payload 大小等因素对最终性能的影响。

:::tip

如需大规模场景、深度定制化的测试服务推荐使用 [XMeter](https://www.emqx.com/zh/products/xmeter)进行测试。

:::





