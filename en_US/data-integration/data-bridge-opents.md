# Ingest Data into OpenTSDB

EMQX supports integration with OpenTSDB. You can save MQTT messages to OpenTSDB for subsequent analysis and retrieval.

{% emqxce %}
:::tip
EMQX Enterprise Edition features. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. Experience the benefits of this [enterprise-ready MQTT messaging platform](https://www.emqx.com/en/try?product=enterprise) today.
:::
{% endemqxce %}


## Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [data bridge](./data-bridges.md)

## Features List

- [Connection pool](./data-bridges.md#connection-pool)
- [Async mode](./data-bridges.md#async-mode)
- [Batch mode](./data-bridges.md#batch-mode)
- [Buffer queue](./data-bridges.md#buffer-queue)
- [SQL preprocessing](./data-bridges.md#prepared-statement)

## Quick Start Tutorial

This section introduces how to configure the OpenTSDB data bridge, covering topics like how to set up the OpenTSDB server, create data bridges and rules for forwarding data to OpenTSDB and test the data bridges and rules.

This tutorial assumes that you run both EMQX and OpenTSDB on the local machine. If you have OpenTSDB and EMQX running remotely, adjust the settings accordingly.

The data reported by the client in this tutorial is as follows:
- Topic: `t/opents`
- Payload:
```json
{
  "metric": "cpu",
  "tags": {
    "host": "serverA"
  },
  "value":12
}
```

### Install OpenTSDB

Install OpenTSDB via Docker, and then run the docker image. 

```bash
docker pull petergrace/opentsdb-docker

docker run -d --name opentsdb -p 4242:4242 petergrace/opentsdb-docker

```

### Create OpenTSDB Data Bridge

1. Go to EMQX Dashboard, and click **Integration** -> **Data Bridge**.

2. Click **Create** on the top right corner of the page.

3. In the **Create Data Bridge** page, click to select **OpenTSDB**, and then click **Next**.

4. Input a name for the data bridge. The name should be a combination of upper/lower case letters and numbers.

5. Input the connection information:

   - **URL**: Input `http://127.0.0.1:4242`, or the actual URL if the OpenTSDB server runs remotely.
   - Leave other options as default.

6. Advanced settings (optional):  Choose whether to use **sync** or **async** query mode as needed. For details, see the relevant configuration information in [Data Integration](./data-bridges.md).

7. Before clicking **Create**, you can click **Test Connectivity** to test that the bridge can connect to the OpenTSDB server.

8. Then click **Create** to finish the creation of the data bridge.

   A confirmation dialog will appear and ask if you like to create a rule using this data bridge, you can click **Create Rule** to continue creating rules to specify the data to be saved into OpenTSDB. You can also create rules by following the steps in [Create Rules for OpenTSDB Data Bridge](#create-rules-for-opentsdb-data-bridge).

Now the OpenTSDB data bridge should appear in the data bridge list (**Integration** -> **Data Bridge**) with **Resource Status** as **Connected**. 

### Create Rules for OpenTSDB Data Bridge

Now that you have successfully created the data bridge to OpenTSDB, you can continue to create rules to specify the data to be saved into OpenTSDB.

1. Go to EMQX Dashboard, and click **Integration** -> **Rules**.

2. Click **Create** at the upper right corner of the page.

3. Input `my_rule` as the rule ID, and set the rules in the **SQL Editor** using the following statement, which means the MQTT messages under topic `t/#`  will be saved to OpenTSDB.

   Note: If you want to specify your own SQL syntax, make sure that you have included all fields required by the data bridge in the `SELECT` part.

   ```sql
   	SELECT
     		payload.metric as metric, payload.tags as tags, payload.value as value
   	FROM
     		"t/#"
   ```

4. Click the **Add Action** button, select **Forwarding with Data Bridge** from the dropdown list, and then select the data bridge you just created under **Data Bridge**.  Click the **Add** button. 
5. Click the **Create** button to finish the setup. 

Now you have successfully created the data bridge to OpenTSDB. You can click **Integration** -> **Flows** to view the topology. It can be seen that the messages under topic `t/#`  are sent and saved to OpenTSDB after parsing by rule `my_rule`. 

### Test the Data Bridges and Rules

Use MQTTX to publish a message on topic `t/opents`. 

```bash
mqttx pub -i emqx_c -t t/opents -m '{"metric":"cpu","tags":{"host":"serverA"},"value":12}'
```

Check the running status of the data bridge, there should be one new incoming and one new outgoing message. 

Check whether the data is written into the OpenTSDB: 

```bash
curl -X POST -H "Accept: Application/json" -H "Content-Type: application/json" http://localhost:4242/api/query -d '{
    "start": "1h-ago",
    "queries": [
        {
            "aggregator": "last",
            "metric": "cpu",
            "tags": {
                "host": "*"
            }
        }
    ],
    "showTSUIDs": "true",
    "showQuery": "true",
    "delete": "false"
}'
```

The formatted output of the query result is as follows:
```json
[
  {
    "metric": "cpu",
    "tags": {
      "host": "serverA"
    },
    "aggregateTags": [],
    "query": {
      "aggregator": "last",
      "metric": "cpu",
      "tsuids": null,
      "downsample": null,
      "rate": false,
      "filters": [
        {
          "tagk": "host",
          "filter": "*",
          "group_by": true,
          "type": "wildcard"
        }
      ],
      "percentiles": null,
      "index": 0,
      "rateOptions": null,
      "filterTagKs": [
        "AAAB"
      ],
      "explicitTags": false,
      "useFuzzyFilter": true,
      "preAggregate": false,
      "rollupUsage": null,
      "rollupTable": "raw",
      "showHistogramBuckets": false,
      "useMultiGets": true,
      "tags": {
        "host": "wildcard(*)"
      },
      "histogramQuery": false
    },
    "tsuids": [
      "000001000001000001"
    ],
    "dps": {
      "1683532519": 12
    }
  }
]% 
```



