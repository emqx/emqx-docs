# Retained Messages

You can view all retained messages in the EMQX on the **Retained Messages** page. When a user publishes a retained message, EMQX will save this message in the system. The user can view this message on the Retained Messages page. When the user subscribes to the topic of this retained message, EMQX will publish this message to the topic, and users can receive this message immediately. The retained message is never expired by default unless the user manually deletes this message.

## Retained Messages List

The Retained Messages List displays all retained messages currently stored in the system. The list includes the topic, Quality of Service (QoS) level, Client ID of the publisher, and the time when the retained message was published. Within the list, you can click the **Show Payload** and **Delete** buttons to inspect the payload of a retained message and delete a retained message respectively. Clicking the **Refresh** button in the top-right corner refreshes the current retained messages list while clicking **Settings** redirects to the retained messages settings page.

EMQX by default retains messages for three system topics. In a clustered environment, retained messages under different system topics are stored based on the node name:

- $SYS/brokers/+/sysdescr: System description of the current EMQX node.
- $SYS/brokers/+/version: The version number of the current EMQX node.
- $SYS/brokers: Number and names of all nodes in the current EMQX cluster.

![image](./assets/retained-messages.png)

### Delete Retained Message

Usually, you can delete retained messages from the client by publishing an empty message to the topic of the retained message. In addition to this method, you can also delete a specified retained message by clicking the **Delete** button in the retained messages list. Furthermore, you have the option to delete all retained messages across the cluster by using the **Clear All** button. You can also set the expiration time for retained messages on the retained messages configuration page. When a retained message reaches its expiration time, EMQX will automatically delete the message.

### View Payload

If you want to view the Payload of the retained message, you can click the **Show Payload** in **Actions** column of the retained message item.

On the pop-up window, you can click the **Copy** button at the lower right corner to copy the payload. You can also select the payload display format from the drop-down list at the lower left corner have more intuitive display for some special payload formats, such as JSON or Hex format.

## Retainer Settings

By clicking the **Settings** button in the upper right corner of the **Retained Messages** page, you will be redirected to the **Retainer** tab on the **Management** -> **MQTT Settings** page where you can enable or disable the retained messages feature and also configure the settings for retained messages.

::: tip

If you disable the retained messages feature by clicking the toggle switch, an **Enable** button will be displayed on the Retained Messages page. By clicking the button, you will be redirected to the **Retainer** tab.

:::

<img src="./assets/mqtt-settings-retainer.png" alt="mqtt-settings-retainer" style="zoom:50%;" />

Below are detailed descriptions of each field.

| Configuration item      | Type     | Optional value    | Default value | Description                                                  |
| ----------------------- | -------- | ----------------- | ------------- | ------------------------------------------------------------ |
| Storage Type            | -        | Built-in Database | -             | -                                                            |
| Storage Method          | Enum     | `ram`, `disc`     | `ram`         | `ram`: Only stored in memory; <br />`disc`: Stored in memory and hard disk. |
| Max Retained Messages   | Integer  | ≥ 0               | 0 (Unlimited) | 0: Unlimit. <br />When you set a limit on the maximum number of retained messages, EMQX replaces existing messages once the limit is reached. However, you cannot store retained messages for new topics beyond the limit. |
| Max Payload Size        | Bytesize |                   | 1MB           | Retain the maximum payload value of the message. If the payload value exceeds the maximum value, the EMQX will treat the retained reserved message as a normal message. |
| Message Expire Interval | Duration |                   | Never Expire  | The expiration time of the retained message, and 0 means never expire. If the message expiration interval is set in the PUBLISH packet, the message expiration interval in the PUBLISH packet shall prevail. |
| Message Clear Interval  | Duration |                   | Disabled      | Interval to clean up expired messages.                       |
| Deliver Rate            | Integer  | ≥ 0               | 1000          | The maximum rate of delivering retain messages.              |
