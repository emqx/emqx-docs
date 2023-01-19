# Topic Metrics

EMQX provides a topic monitoring feature(called Topic Metrics) that allows you to count the number of messages sent and received, the rate and other metrics for a given topic. You can view and use this feature through the Dashboard's **Diagnose** -> **Topic Metrics** page, or you can do the corresponding operation through the HTTP API.

Once the theme monitoring feature is enabled, you can create a new theme monitoring by clicking the **Add Topic** button in the top right corner of the page.

> For overall performance reasons, the theme monitoring feature currently only supports theme names, i.e. it does not support theme filters with `+` or `#` wildcards, such as `a/+`, etc. Support will be provided someday in the future if we resolve performance issues.
