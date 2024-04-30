# MQTT Client Attributes

Apart from using predefined names like `clientid` and `username` as MQTT client identifiers, the client attribute extraction feature allows setting custom attributes upon connection for use in functions such as authentication and authorization. It is designed to support flexible templating for MQTT client identification by extracting client attribute values from various client metadata sources. This feature is particularly useful in contexts such as personalized client configurations, and streamlined authentication processes.

## Introduction

When a client connects to EMQX, its client attributes are initialized. During initialization, EMQX extracts attributes based on predefined rules set in the `mqtt.client_attrs_init` configuration. For example, it can extract a substring from the clientID, username, or client certificate common name. This extraction happens before the authentication process, ensuring that the attributes are ready to be used in subsequent steps, such as used in the HTTP request body template or SQL template for composing authentication and authorization requests.

Client attributes are stored in a field called `client_attrs` within the client's session or connection context. This `client_attrs` info field acts like a dictionary or a map, holding the attributes as key-value pairs. This field is maintained in memory associated with the client's session for quick access during the client's connection lifecycle.

After initialization `client_attrs` object can be extended with more data fields from authentication results, such as JWT's `client_attrs` claim, and `client_attrs` field in HTTP authentication response body.

### Extract Client Attributes

This section describes from which client attributes EMQX extracts and how attributes are extracted.

#### Sources of Client Metadata and Attributes

In EMQX, client metadata and attributes originate from various sources and are stored within specific properties for use throughout the client's connection lifecycle.
Here's a breakdown of where these existing client information come from and where they are stored:

- **MQTT CONNECT Packet**: When a client connects to EMQX, it sends a CONNECT packet that includes several pieces of information, such as `clientid`, `username`, `password`, and `user properties`.
- **TLS Certificates**: If the client connects using TLS, the client's TLS certificate can provide additional metadata, such as:
  - `cn` (Common Name): Part of the certificate that can identify the device or user.
  - `dn` (Distinguished Name): The full subject field within the certificate that includes several descriptive fields about the certificate owner.
- **IP Connection Data**: This includes the client’s IP address and port number, which are automatically captured by EMQX when a client connects.
- **Properties from Listener Config**: A client may also inherit properties like `zone` from MQTT listener.

The extraction process uses an function-call style template render expression which we call Variform.
For example, this expression extracts the prefix of a dot-separated client ID.

```js
nth(1, tokens(clientid, '.'))
```

And the configuration may look like below.

```js
mqtt {
    client_attrs_init = [
        {
            expression = "nth(1, tokens(clientid, '-'))"
            # And set as client_attrs.group
            set_as_attr = group
        }
    ]
}
```

Pre-bound variables can be directly used in the extraction expressions.
For client attributes extraction, below variables are pre-bound:

- `cn`: Client certificate common name.
- `dn`: Client certificate distinguish name (Subject).
- `clientid`
- `username`
- `user_property`: The user properties provided in the MQTT v5 CONNECT packet sent by the client.
- `ip_address`: The source IP of the client.
- `port`: The source port number of the client
- `zone`: The zone name

::: tip
Read more about [Variform](../advanced/variform.md).
:::

### Extend Attributes from Authentication Response

EMQX merges attributes from authentication results. Below are the authentication mechanisms supported so far.

- **JWT claims**: If JWTs are used for authentication, they can include `client_attrs` claims that carry additional metadata about the client, such as roles, permissions, or other identifiers.

- **HTTP Authentication Responses**: If EMQX is configured to use an external HTTP service for authentication, the response from this service might include additional attributes or metadata about the client, which can be configured to be captured and stored within EMQX. For example, if an HTTP response includes `"client_attrs": {"group": "g1"}`, EMQX will incorporate this data into the client's existing attributes, which can then be utilized in authorization requests.

## Application of Client Attributes

The extracted and merged attributes can be used in constructing authentication and authorization requests. The `client_attrs.NAME` can be used in authentication and authorization template rendering. If an attribute named `client_attrs.alias` is defined, it can be incorporated into an HTTP request body or SQL query, enhancing the flexibility and specificity of these requests.

For example, for an attribute named `client_attrs.alias`, you can use `${client_attrs.alais}` to build an HTTP request body as an HTTP authentication request.  For more details, see [Authentication Placeholders](../access-control/authn/authn.md#authentication-placeholders).

## Configure Attribute Extraction

You can configure the attribute extraction feature through the configuration file or Dashboard.

### Configure Attribute Extraction via Configuration File

Configuration example:

```
<!-- code example -->
```

Explanations of the configuration items:



{% emqxce %}

For detailed information about the configuration, see [Configuration Manual](https://www.emqx.io/docs/en/v@CE_VERSION@/hocon/).

{% endemqxce %}

{% emqxee %}

For detailed information about the configuration, see [Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/).

{% endemqxee %}

### Configure Attribute Extraction via Dashboard

<!-- Add description after Frontend dev. Complete -->

