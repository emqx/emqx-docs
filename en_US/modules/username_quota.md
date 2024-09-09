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

## Usage Example

1. On the Modules page, navigate to the **Username Quota** module you have added. Click **Manage** on the right.

2. On the Details page, you can see the current usernames in the EMQX cluster and the number of sessions used for each username. Click the **View** button next to the session count to view the session list for the current username.

   ![user-quota-usage](./assets/user-quota-usage-en.png)
