# Prerequisites

By default, all nodes assume the Core node role, so the cluster behaves like that in EMQX 4.x, which is recommended for a small cluster with 3 nodes or fewer. 

The Core + Replicant mode is  recommended if there are more than 3 nodes in your cluster. To enable the  Core + Replicant mode, the backend database (`db_backend`) should be set to `rlog`, some nodes should assume the replicant role (`node.db_role`), and the core node (`core_node`) should be specified, as shown below:

```bash
cluster {
		## Default setting, suitable for very large backend
		db_backend = rlog 
		##To set a node as a replicant node
		node.db_role = replicant 
		##List of core nodes that the replicant will connect to, different nodes can be seperated with a comma 
		core_node = [node1, node2, ...] 
}
```

<!--增加环境变量的配置方法-->

It is recommended to operate an EMQX cluster under the following network condition and hardware specifications:

## Network and Hardware

**Network**

Network latency is recommended to be less than 10 ms. The cluster will not be available if the latency is higher than 100 ms. 

The core nodes should be under the same private network. It is also recommended to deploy the replicant and core nodes under the same private network. 

**CPU and memory**

Configure as your connection and throughput needs. 

<!--Core nodes require a large amount of memory, and the CPU consumption is low when there are no connections; the hardware specification of Replicant nodes is the same as with EMQX 4.x, and you can configure it as your connection and throughput needs.-->

## Node configurations

1. All nodes are set with a unique node name in the format of `name@host`, where host must be an IP address or fully qualified domain name (FQDN).
2. If there is a firewall or security group between nodes, ensure the cluster communication port has been opened. For details, see [Intra-cluster communication port](./security.md).
3. All nodes use the same security cookie. For details about the magic cookie used, see [Distributed Erlang - Security](https://www.erlang.org/doc/reference_manual/distributed.html#security).  <!--这里的cookie 需要增加解释，以及如何配置cookie-->

:::tip

EMQX node names are immutable, as they are baked into the database schema and data files. It is strongly recommended to use static FQDNs for EMQX node names, even when the nodes have static IPs.
:::