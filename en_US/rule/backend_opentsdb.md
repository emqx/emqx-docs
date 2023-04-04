# Integrate with OpenTSDB

Setup a OpenTSDB database, taking Mac OSX for instance:

```bash
$ docker pull petergrace/opentsdb-docker

$ docker run -d --name opentsdb -p 4242:4242 petergrace/opentsdb-docker
```

Create a rule:

Go to [EMQX Dashboard](http://127.0.0.1:18083/#/rules), select the
"rule" tab on the menu to the left.

Select "message.publish", then type in the following SQL:

```sql
SELECT
    payload.metric as metric, payload.tags as tags, payload.value as value
FROM
    "message.publish"
```

![image](./assets/rule-engine/opentsdb_sql_1.png)

Bind an action:

Click on the "+ Add" button under "Action Handler", and then select
"Data to OpenTSDB" in the pop-up dialog window.

![image](./assets/rule-engine/opentsdb_action_0.png)

Fill in the parameters required by the action:

Six parameters is required by action "Data to OpenTSDB":

1). Details. Whether let OpenTSDB Server return the failed data point
and their error reason, defaults to false.

2). Summary. Whether let OpenTSDB Server return data point
success/failure count, defaults to true.

3). Max Batch Size. In case of heavy traffic, how many data points are
allowed to be included in a single request. Default to 20.

4). Sync Call. Defaults to false.

5). Sync Timeout. Defaults to 0.

![image](./assets/rule-engine/opentsdb_action_1.png)

6). Bind a resource to the action. Since the dropdown list "Resource"
is empty for now, we create a new resource by clicking on the "New
Resource" to the top right, and then select "OpenTSDB":

![image](./assets/rule-engine/opentsdb_action_2.png)

Configure the resource:

Keep all the default configs as default, and click on the "Testing
Connection" button to make sure the connection can be created
successfully, and then click on the "Create" button.

![image](./assets/rule-engine/opentsdb_resource_1.png)

Back to the "Actions" dialog, and then click on the "Confirm"
    button.

![image](./assets/rule-engine/opentsdb_action_3.png)

Back to the creating rule page, then click on "Create" button. The
    rule we created will be show in the rule list:

![image](./assets/rule-engine/opentsdb_rule_overview_0.png)

We have finished, testing the rule by sending an MQTT message to
    emqx:

```bash
> Topic: "t/1"
>
> QoS: 0
>
> Retained: false
>
> Payload: {"metric":"cpu","tags":{"host":"serverA"},"value":12}
```

Then inspect the OpenTSDB table, verify a new record has been
inserted:

```bash
## Use postman to send an HTTP request to the opentsdb server:
POST /api/query HTTP/1.1
Host: 127.0.0.1:4242
Content-Type: application/json
cache-control: no-cache
Postman-Token: 69af0565-27f8-41e5-b0cd-d7c7f5b7a037
{
    "start": 1560409825000,
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
}
------WebKitFormBoundary7MA4YWxkTrZu0gW--
```
The response should look like following:

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
          "index": 0,
          "tags": {
              "host": "wildcard(*)"
          },
          "rateOptions": null,
          "filterTagKs": [
              "AAAC"
          ],
          "explicitTags": false
      },
      "tsuids": [
          "000002000002000007"
      ],
      "dps": {
          "1561532453": 12
      }
  }
]
```

And from the rule list, verify that the "Matched" column has increased
to 1:

![image](./assets/rule-engine/opentsdb_rule_overview_1.png)
