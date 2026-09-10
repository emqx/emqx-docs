# Known Issues in EMQX 6.1

## 6.1.1

| Since version | Issue                                                        | Workaround                                                   | Status |
| ------------- | ------------------------------------------------------------ | ------------------------------------------------------------ | ------ |
| 5.8.0         | **Core-replicant cluster startup may not complete when Durable Storage is enabled**<br />When initializing a cluster with core and replicant nodes, Durable Storage (DS) metadata may become inconsistent across nodes, causing partial unavailability of related features: Durable Sessions, Queues, or Streams. This can affect both new clusters and clusters upgraded from EMQX 5.x, especially when `n_sites > 1`. It is typically indicated by continuous warnings such as:<br />`msg: dsrepl_optimistic_leader_fail, reason: {init_failed,{error,recoverable,{fsm_needs_upgrade,0}}}` | Stop and restart each replicant node one at a time. |  |

