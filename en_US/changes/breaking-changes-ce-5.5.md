# Incompatible Changes in EMQX 5.5

## v5.5.0

Refactored and divided the MQTT data bridge management in EMQX to offer a more flexible and efficient management approach. Operations previously managed through the `/bridges` API have now been split across three separate APIs: `/connectors`, `/actions`, and `/sources`. This division allows for more granular control and management of each component of the MQTT data bridge, enhancing system flexibility and usability.

For configurations from older versions, users must manually migrate by following the configuration instructions in [Bridge with Other MQTT Services](../data-integration/data-bridge-mqtt.md). This migration process may require some time and effort, but users can benefit from the enhancements brought by the new version once completed.