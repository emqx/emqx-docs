# Known Issues in EMQX 6.1

## 6.1.1

| Since version | Issue                                                        | Workaround                                                   | Status |
| ------------- | ------------------------------------------------------------ | ------------------------------------------------------------ | ------ |
| 5.8.0         | **Core-replicant cluster startup may not complete when Durable Storage is enabled**<br />During initialization of a cluster comprised of core and replicant nodes, Durable Storage (DS) metadata may become inconsistent across nodes, which leads to partial unavailability of related features: Durable Sessions, Queues or Streams. This may affect both new clusters and clusters upgraded from EMQX 5.x, particularly when `n_sites` is configured to a value greater than 1. It is typically manifested as a constant stream of the following warnings:<br />`msg: dsrepl_optimistic_leader_fail, reason: {init_failed,{error,recoverable,{fsm_needs_upgrade,0}}}` | For every replicant node: stop the node, delete its data directory, and restart. | - |
