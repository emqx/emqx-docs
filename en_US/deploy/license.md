---
title: Work with EMQX Enterprise License
description: EMQX Enterprise already includes a single-node Community License with limited commercial use permission. However,  if you use EMQX Enterprise for full commercial usage and cluster deployment, you must obtain a commercial license. 
---


# Work with EMQX Enterprise License

Starting from EMQX 5.9, EMQX is released under the Business Source License (BSL) 1.1, a source-available license that allows open development while protecting EMQX’s commercial use.

::: tip

For detailed information about the licensing changes, see the [EMQX Licensing FAQ](https://www.emqx.com/en/content/license-faq).

:::

As part of the installation package, EMQX Enterprise already includes a single-node Community License with limited commercial use permission. However,  if you use EMQX Enterprise for full commercial usage and cluster deployment, you must obtain a Commercial License. 

This page guides you through the process of obtaining a Commercial License and importing it into EMQX.

## Apply for a License

To apply for a Commercial License with a valid License Key, contact your EMQ sales representative or fill out the contact information on our [Contact Us](https://www.emqx.com/en/contact?product=emqx&channel=apply-Licenses) page to apply for a commercial license. Our sales representative will contact you as soon as possible. 

Suppose you prefer to try EMQX Enterprise before purchasing. In that case, you can apply for a Trial License on our [Trial License application page,](https://www.emqx.com/en/apply-licenses/emqx) and the license file will be sent to your email immediately:

- The Trial License is valid for 15 days.
- The Trial License supports 10,000 concurrent sessions.

::: tip Note

All the EMQX Enterprise features are available during the trial period. However, the Clustering feature will be disabled after the trial period expires. You will need to purchase a Commercial License to continue using the Clustering feature.

EMQX Enterprise under a Trial License is not permitted for use in production environments.

:::

If you want to extend the trial period, contact our sales department.

## Update and Configure License Settings

You can update your license file and configure the settings for the license connection quota usage through the EMQX Dashboard, command line interface (CLI), or the configuration file.

### Dashboard

1. On the EMQX Dashboard, click **System** -> **License** from the left navigation menu. In the **Basic Info** section on the License page, you can check information such as License connection quota usage, EMQX version, and issue information. 

2. Click the **Update License** button. Paste your License Key in the pop-up dialog box, and click **Save**. The license information on the page automatically refreshes following your submission.

   Verify the information to confirm that the new license file has taken effect.

3. In the **License Settings** section, you can configure the watermark thresholds for the license session quota usage limits. For details about session limits, see [Session Limits](#session-limits).

   - **Usage High Watermark**: Specify the percentage value to set the threshold above which alarms for license session quota usage will be triggered.
   - **Usage Low Watermark**: Specify the percentage value to set the threshold below which alarms for license session quota usage will be deactivated.

4. Click **Save Changes** to save your License settings.

   <img src="./assets/license.png" alt="license" style="zoom: 50%;" />

#### Revert to Community License

The EMQX Dashboard allows users to revert to the default single-node Community License. You can click the **Remove License** button on the **License** page and confirm in the pop-up dialog to remove the current License.

::: tip Note

In cluster mode, the License cannot be removed. If you are using EMQX in cluster mode, you must first dissolve the cluster.

:::

After reverting to the Community License:

- The current License will be cleared and replaced with the Community License.
- Existing client connections will remain active.

::: tip Note

The Community License does not allow full commercial use and supports only single-node deployments. Removing the License will disable clustered deployment.

:::

### CLI

You can also use the following command to update your EMQX Enterprise License:

```bash
./bin/emqx ctl 

    license info             # Show license info 
    license update <License> # Update license given as a string
    license update default   # Revert to default Community License
```

### Configuration File

You can also configure the license file with the configuration file. After the configuration, you can run `emqx ctl license reload` in [EMQX command line tool](../admin/cli.md) to reload the license. 

```bash
license {
    ## License Key
    key = "MjIwMTExCjAKMTAKRXZhbHVhdGlvbgpjb250YWN0QGVtcXguaW8KZGVmYXVsdAoyMDIzMDEwOQoxODI1CjEwMAo=.MEUCIG62t8W15g05f1cKx3tA3YgJoR0dmyHOPCdbUxBGxgKKAiEAhHKh8dUwhU+OxNEaOn8mgRDtiT3R8RZooqy6dEsOmDI="
    ## Low watermark limit below which license connection quota usage alarms are deactivated
    connection_low_watermark = "75%"

    ## High watermark limit above which license connection quota usage alarms are activated
    connection_high_watermark = "80%"
}
```

After execution, you can run `emqx ctl license info` to confirm that the new license file has taken effect.

<!-- 您也可以通过环境变量 `EMQX_LICENSE__KEY` 变量名设置您的 License。TODO 确认是否可以 reload -->

## License Limits

EMQX Enterprise Licenses may include usage limits to enforce compliance with licensed terms in production environments. The license limits include:

- Session Limits
- TPS Limits (from EMQX 6.0)

### Session Limits

The session limit defines the maximum number of concurrent MQTT client connections (sessions) that EMQX Enterprise can support under the current License.

- When the limit is reached, any new connection attempts will be rejected.
- Clients attempting to connect beyond the licensed quota will receive a "Quota Exceeded" response with CONNACK reason code `151 (0x97)`.
- An alarm will be raised if the session usage crosses the configured high watermark threshold.
- The alarm will be cleared automatically once the usage drops below the low watermark threshold.

You can configure the alarm watermarks via the EMQX Dashboard or configuration file.

### Session High-Watermark History

EMQX Enterprise automatically records the daily peak session count across the cluster and retains at least 24 months of history. This data is stored in a replicated, tamper-proof internal table that persists across node restarts and cluster topology changes, providing an audit-ready basis for billing settlements.

#### CLI

Use `emqx ctl license history` to query the recorded history:

```bash
# Monthly peaks (default)
emqx ctl license history

# Daily peaks, last 7 days
emqx ctl license history 7 --period daily

# JSON output
emqx ctl license history --json
```

For full command reference, see [license history](../admin/cli.md#license-history).

#### REST API

```bash
GET /api/v5/license/session_hwm_history
```

**Query Parameters**

| Parameter | Type | Default | Description |
| --------- | ---- | ------- | ----------- |
| `period` | `daily` \| `monthly` | `daily` | Aggregation granularity. `daily` returns one row per calendar day; `monthly` folds daily peaks into per-month maximums. |
| `limit` | Integer | `30` | Maximum number of rows to return. Applies to `daily` period only; for `monthly`, all 24 months are always returned. |

**Response Example**

```json
{
  "period": "monthly",
  "count": 2,
  "data": [
    { "period": "2026-04", "high_watermark": 25000, "observed_at": "2026-04-18T13:53:05.000Z" },
    { "period": "2026-03", "high_watermark": 23500, "observed_at": "2026-03-31T22:10:42.000Z" }
  ]
}
```

Each record contains:
- `period`: Calendar day (`YYYY-MM-DD`) or month (`YYYY-MM`), depending on the requested period
- `high_watermark`: Peak session count observed during the period
- `observed_at`: RFC 3339 timestamp of the peak observation

#### Timezone Configuration

Day boundaries are determined by the `license.high_watermark_timezone` configuration field. By default, it uses the node host's local timezone (`"system"`). You can set an explicit UTC offset (e.g., `"+08:00"`) to ensure consistent day boundaries across nodes in different regions. See [License Configuration](../configuration/license.md) for details.

### TPS Limits

Starting from EMQX 6.0, Licenses can also include a Transactions Per Second (TPS) limit. This limit applies to the total MQTT messages processed across the cluster, including both incoming and outgoing MQTT messages.

- When the TPS usage exceeds the licensed limit, EMQX raises an alarm.
- The alarm will record the peak TPS observed, but will not restrict message traffic.
- The alarm remains active until:
  - A new license with a higher TPS limit is applied, or
  - The alarm is manually deactivated from the EMQX Dashboard or CLI.

This TPS limit is designed for observability and compliance rather than strict enforcement.

::: tip Note

The TPS limit is defined in the license and cannot be configured or adjusted by the user. To raise the limit, apply a new license with a higher TPS value.

:::
