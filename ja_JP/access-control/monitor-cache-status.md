To view cache metrics and monitor usage in real-time:

1. Click the arrow next to **External Resource Cache Settings** and select **External Resource Cache Status**. A side panel will appear showing cache metrics.
2. Use the drop-down to view metrics per node or across the cluster.

The metrics include the following:

- **Memory Usage**: Total memory currently used by the cache.
- **Cache Entries**: Total number of stored cache results.
- **Cache Hits**: Number of times EMQX found a valid result in the cache, avoiding a call to the external backend.
  - Metrics shown: Current rate, 5-minute average, Maximum rate
- **Cache Misses**: Number of times EMQX looked for a result in the cache but didn’t find one, resulting in a backend query.
  - Metrics shown: Current rate, 5-minute average, Maximum rate
- **Cache Inserts**: Number of new results added to the cache after a miss.
  - Metrics shown: Current rate, 5-minute average, Maximum rate

At the bottom of the panel, a node list provides an overview of **Memory Usage**, **Cache Entries**, and **Cache Hits** for each node in the cluster.

You can refresh or reset the statistics using the buttons in the top-right corner of the panel.