# Log Trace

Though enabling the `debug` level log can significantly increase the efficiency for debugging, the number of generated logs may affect the system performance, especially in the production environments with intensive connecting and messaging services, so it is not feasible. 

Therefore, EMQX 5.0 has added the Log Trace feature, allowing users only to enable debug level logs output for specific client IDs, topics or IPs in real-time. 

## Introduction

Log Trace is based on the built-in Erlang Logger Filter function, which only has a negligible impact on overall message throughput:

- EMQX uses independent File Handlers to persist Trace disk logs.
- EMQX will create an independent process for each client connection to process its messages.
- When receiving a client message, this independent process will check whether it complies with the rules according to the customized Trace Filter, for example, whether it is from the specified client ID; if not, EMQX will execute the original transfer logic; if yes, this process will convert the message into a binary data, and then asynchronously sends the message to the File Handler.
- Then the File Handlers will persist the binary data into Trace files.

Therefore, all filtering actions are completed by the independent process of each client. As most logs will be filtered out, the File Handler will not be overloaded by the incoming messages, and it is safe for the production environments.

Log Trace can be used for various scenarios, such as debugging abnormal messages or data losses, client disconnection, subscription failure, etc. For system malfunctions that occur at a specific time, Trace allows users to set the task start/stop time for automatic log collection, which is very convenient for users.

On EMQX Dashboard, you can click **Diagnose** -> **Log Trace**, and then click **Create** to set your rules. Below will introduce the operating steps.  

<!-- TODO 下面的内容先凑合使用，后续更新 -->

## Trace by Client IDs

1. Select the **Type** as `ClientID`.
2. Fill in the Client IDs to be traced.
3. Select the start and end time. If the start time is earlier than or the same as the current time, it will start from the current time.
![image-202112140002](./assets/trace_create_clientid.png)
![image-202112140003](./assets/trace_clientid.png)

You can see the Trace record after successful creation in the list, where you can view it or choose to download the log. The log contains the current ClientID interaction with the EMQX connection.

## Trace by Topics

1. Select the **Type** as `Topic`;
2. Fill in the topics to be traced and wildcard characters are supported;
3. Select the start and end time. If the start time is earlier than or the same as the current time, it will start from the current time.

![image-202112140004](./assets/trace_create_topic.png)

You can see the Trace record after successful creation in the list, where you can view it or choose to download the log. The log contains Publish/Subscribe/UnSubscribe information for the current topic on EMQX.

## Trace by IPs

1. Select the **Type** as `IP Address`;
2. Fill in the IP addresses to be traced;
3. Select the start and end time. If the start time is earlier than or the same as the current time, it will start from the current time.

![image-202112140005](./assets/trace_create_ip.png)

You can see the Trace record after successful creation in the list, where you can view it or choose to download the log. The log contains the current IP interaction with the EMQX connection.

![image-202112140006](./assets/trace_list.png)

### Notes

1. You can create up to 30 trace logs. 
2. Trace logs can generate a maximum of 512MB logs per node. If the generated log file reaches the maximum, it will stop appending logs and give an alert in the main log file.
3. You can stop logging manually or wait until the end time to stop automatically.
4. The log file size viewed in the list is the sum of the uncompressed file sizes.
5. The EMQX cluster will continue an outstanding trace after the restart.
6. If the dashboard download timeout, you can find the log file directly on the server's data/trace.

