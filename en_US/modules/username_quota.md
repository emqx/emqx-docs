# Username Quota

Starting from version 4.4.25, a new username quota module has been added to limit the maximum number of sessions for a single MQTT username.

When a username exceeds its quota, new session connections will be rejected, and the corresponding CONNACK return code will be sent: "0x97 Quota Exceeded" (MQTT 5.0) or "0x03 Service Unavailable" (MQTT 3.1.1).

## Add Username Quota Module

1. Click **Modules** from the left navigation menu on the Dashboard.

2. Click the **Add Module** button on the page.

3. Navigate to **Username Quota** under the **Authentication** tab on the module select page, and click **Select**.

4. Configure the following options for the username quota module.

   - **Max Sessions Per Username**: Defines the maximum number of MQTT sessions allowed for each username. Note that if the MQTT client logs in using a persistent session, the session will remain on the server even after the client disconnects, until the session expires and is cleared.
   - **Username White List**: You can add username entries by clicking the **Add** button on the right. Usernames in the whitelist are not subject to session limits. For example, clients connecting to a cluster using the MQTT bridge should bypass the quota limitation, you can add the usernames used by the MQTT bridge to the whitelist.

5. Click **Add** to complete the settings.

   ![user-quota-create](./assets/user-quota-create-en.png)

## Manage Username Quota

On the Modules page, navigate to the **Username Quota** module you have added. Click **Manage** on the right.

On the **Usage** tab, you can view the current usernames in the EMQX cluster and the number of sessions used for each username. By clicking the **View** button next to the session count, you can see the session list for the selected username.

On the **Configuration** tab, you can edit the configuration of the username quota limits. To delete the module, click **Delete** in the upper right corner.

![user-quota-usage](./assets/user-quota-usage-en.png)

## HTTP API

In addition to viewing the username quota module on the Dashboard, you can also retrieve username usage details using the HTTP API.

### GET /api/v4/quota/usernames

Get the list of usernames in the cluster, sorted in descending order by the number of sessions for each username.

**Success Response Body (JSON):**

| Name | Type    | Description |
| ---- | ------- | ----------- |
| code | Integer | 0 indicates success   |
| data | Array   | List of username details   |
| data[0].username | String   | Username |
| data[0].used | Integer   | Number of sessions for the username |
| data[0].clientids | Array | List of client IDs |

**Examples**

```shell
curl -u admin:public 'http://localhost:18083/api/v4/quota/usernames' | jq .

{
  "meta": {
    "page": 1,
    "limit": 10000,
    "count": 1
  },
  "data": [
    {
      "username": "a",
      "used": 1,
      "clientids": [
        "mqttjs_6916e2ae"
      ]
    }
  ],
  "code": 0
}
```

### GET /api/v4/quota/usernames/:username

Get the sessions for a specific username.

**Success Response Body (JSON):**

| Name | Type    | Description |
| ---- | ------- | ----------- |
| code | Integer | 0 indicates success   |
| data | Object  | User details   |
| data.username | String   | Username |
| data.used | Integer   | Number of sessions for the username |
| data.clientids | Array | List of client IDs |

**Examples**

```shell
curl -u admin:public 'http://localhost:18083/api/v4/quota/usernames/a' | jq .

{
  "data": {
    "username": "a",
    "used": 1,
    "clientids": [
      "mqttjs_6916e2ae"
    ]
  },
  "code": 0
}
```
