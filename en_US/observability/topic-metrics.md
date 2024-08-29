# Topic Metrics

::: tip Note

The Topic Metrics feature is only available in EMQX Enterprise.

:::

The Topic Metrics feature in EMQX provides detailed statistics for a given topic, including the number of messages sent and received, message rate, and other relevant metrics. To access this feature, navigate to **Diagnose** -> **Topic Metrics** on the EMQX Dashboard. Alternatively, you can retrieve topic metrics through the REST API.

## View Topic Metrics on Dashboard

After enabling the Topic Metrics feature, you can add new topic monitoring rules by clicking the **Add Topic** button in the top right corner of the page. Note that topic filters with wildcards, such as `+` or `#`, are not supported at this time. You must use specific topic names.

<img src="./assets/topic-metrics-ee.png" alt="topic-metrics-ee" style="zoom: 40%;" />

Click the **View** button in the **Actions** column to see detailed information about the number of messages received, sent, and dropped per second for the specified topic. You can also filter this information by different QoS levels.

The topic metrics list includes the following fields:

- **Topic**: The name of the topic that you want to monitor.
- **Incoming messages**:  The total number of incoming messages on the current topic and the number of incoming messages per second.
- **Outgoing Messages**: The total number of outgoing messages on the current topic and the number of outgoing messages per second.
- **Dropped Messages**: The total number of messages dropped on the current topic and the number of dropped messages per second.
- **Start at**: The time you created this topic monitoring record.
- **Actions**: Operations you can do on this topic monitoring record.
  - **View**: View detailed metrics of the topic by different QoS levels.
  - **Reset**: Restart the monitoring by clicking this button.
  - **Delete**: Remove the record.

## Get Topic Metrics via REST API

You can also retrieve the topic metrics through the API. On how to work with EMQX APIs, see [REST API](../admin/api.md).

<img src="./assets/topic-metrics-api-ee.png" alt="topic-metrics-api-ee" style="zoom:45%;" />
