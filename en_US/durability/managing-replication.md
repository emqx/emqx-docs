# Managing Replication

If you plan to run EMQX in a single-node configuration, you can skip this section. However, if you later decide to scale your deployment to a multi-node cluster, the information and procedures described here will be essential.

## Initial Cluster Setup

Suppose you are setting up a new EMQX cluster with session persistence enabled, or upgrading an existing cluster to enable session persistence. In that case, there are a few configuration parameters that influence how the cluster initially sets up durable storage and starts replicating data. Be sure to review these parameters as changing them will not take any effect once the durable storage is initialized.

### Replication Factor

The replication factor, controlled with `durable_storage.messages.replication_factor` configuration parameter, determines the number of replicas each shard should have across the cluster. The default value is 3. As a rule of thumb, the replication factor should be set to an odd number. This is because the replication factor is used to determine the quorum size needed to consider a write operation successful. The larger the replication factor, the more copies of the data are stored across the cluster, thus increasing durability and availability. This is at the expense of increased storage and network overhead, because more communication is needed to achieve consensus.

Note that the replication factor is not strictly enforced in smaller clusters. For example, in a two-node cluster, the default replication factor is effectively 2, as each shard is replicated on both nodes, and there is no point in replicating it further. In general, EMQX allocates the replicas of each shard to different sites in the cluster, so that no two replicas are stored on the same node.

### Number of Shards

This in turn determines the number of independent partitions, called _shards_, that the durable storage is divided into. It's controlled by the `durable_storage.messages.n_shards` parameter, which is 16 by default. If you plan to have a large number of nodes in the cluster, you may want to increase this value to ensure that the data is more evenly distributed and that the cluster has more opportunities to parallelize read and write operations. However, increasing the number of shards also inevitably increases the load on subscribers' side, as they need to query more shards to retrieve the data they are interested in and manage noticeably heavier subscription state.

Keep in mind that it's not possible to change the number of shards once the durable storage is initialized. Make a decision wisely.

### Number of Sites

The number of sites, as controlled by the `durable_storage.messages.n_sites` configuration parameter, determines the _minimum_ number of sites that must be online in the cluster for the durable storage to even initialize and start accepting writes. Once the minimum number of sites is online, the durable storage will start allocating shards to the available sites in a fair manner.

The default value is 1, which actually means that (depending on clustering strategy) each node may initially consider itself an only site responsible for storing the data, and initialize the durable storage accordingly. This is obviously a conflicting situation in a multi-node cluster. In this case once the cluster is formed, a single view of the cluster will "win", which will cause the other nodes to abandon the data they have stored so far. Despite this is unlikely to happen in practice, it's still a good idea to set the number of sites to an initial cluster size to avoid such situations.

Again, once the durable storage is initialized, changes to this parameter will not take any effect.

## Changing Existing Cluster

At some point an existing cluster may need to be reconfigured. Changes to required capacity, durability, or client traffic, decommissioning old nodes and replacing them with new ones are most common reasons for reconfiguration. This can be achieved by joining new sites to the set of sites the durable storage is replicated to, or by removing the sites that are no longer needed.

You can always look up the current allocation of shards through `emqx ctl` CLI. The `ds` subcommand stands for "durable storage".
```shell
$ emqx ctl ds info
SITES:
...
SHARDS:
...
```

### Adding Sites

As soon as the new node is part of the cluster, it's assigned _Site ID_ and can be made a part of the durable storage.
```shell
$ emqx ctl ds join emqx_durable_storage <Site ID>
ok
```

Responsibility over some of the shard replicas will be transferred to this new site, and it will start replicating the data.

Depending on the amount of data stored in the cluster, the process of joining a new site may take some time. This does not compromise availability of the durable storage, but may affect the performance of the cluster, because a lot of data may be shuffled around in the background.

Any changes to the set of durable storage sites are durably stored as well, so that node restarts or network partitions do not affect the outcome. Cluster will eventually reach the desired state in a consistent manner.

### Removing Sites

Removing a site implies transferring the responsibility over the shard replicas away from the site being removed.
```shell
$ emqx ctl ds leave emqx_durable_storage <Site ID>
ok
```

This process is similar to adding a site, and may take a lot of time as well.

However, removing a site may cause the effective replication factor to drop below the configured value. For example, if the replication factor is 3 and one of the sites in a 3-node cluster is removed, the replication factor will effectively drop to 2. This is a bit risky, so if you plan to replace a node, it's a good idea to add a new site first, and only then remove the old one. Or alternatively, perform both operations simultaneously.

### Assiging Sites

If you plan to conduct a series of changes to the set of sites holding the durable storage replicas (as in the aforementioned example), it may be a good idea to assign the sites in a single operation.
```shell
$ emqx ctl ds set_replicas emqx_durable_storage <Site ID 1> <Site ID 2> ...
```

This will help to minimize amount of data that needs to be transferred between the sites, while at the same time ensuring that the replication factor is maintained if possible.
