You can enable and configure the external resource cache through the EMQX Dashboard:

1. Navigate to **Access Control** -> **Authorization**. 

2. Click the **External Resource Cache Settings** button in the upper-right corner. A side panel will appear from the right.

3. In the panel, use the **Enable External Resource Cache** button to turn the caching feature on or off. Once enabled, configure the following cache settings:

   | Field Name                        | Description                                                  |
   | --------------------------------- | ------------------------------------------------------------ |
   | **Maximum Number of Cache Items** | Maximum number of cached entries per node. Default: `1,000,000`. |
   | **Maximum Memory**                | Limit on cache memory usage. Default: `100 MB`.              |
   | **Cache TTL**                     | Duration a cached entry remains valid. Default: `1 minute`.  |

4. Click **Update** to apply the settings.

These settings are applied cluster-wide to ensure consistent behavior across all nodes.
