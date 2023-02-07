# Topic Metrics

EMQX provides a topic monitoring feature(called Topic Metrics) that allows you to count the number of messages sent and received, the rate and other metrics for a given topic. You can view and use this feature through the Dashboard's **Diagnose** -> **Topic Metrics** page, or you can configure through the HTTP API.

After the Topic Metrics feature enabled, you can add new topic monitoring rules by clicking the **Add Topic** button in the top right corner of the page.

At the moment, topic filters with widecards are currently not supported, for example, `+` or `#`. You have to use the specific topic names.
