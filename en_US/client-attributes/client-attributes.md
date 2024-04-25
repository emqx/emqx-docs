# MQTT Client Attribute Extraction

Apart from using predefined names like `clientid` and `username` as MQTT client identifiers, the client attribute extraction feature allows setting custom attributes upon connection for use in functions such as authentication and authorization. It is designed to support flexible templating for MQTT client identification by extracting client attribute values from various client metadata sources. This feature is particularly useful in contexts such as multi-tenancy, personalized client configurations, and streamlined authentication processes.

## Introduction

When a client connects to EMQX, its client attributes are initialized. During initialization, EMQX extracts attributes based on predefined rules set in the `mqtt.client_attrs_init` configuration. For example, it can extract a substring from the clientID, username, or client certificate common name. This extraction happens before the authentication process, ensuring that the attributes are ready to be used in subsequent steps, such as used in the HTTP request body template or SQL template for composing authentication and authorization requests.

Once initialized, client attributes are stored in a field called `client_attrs` within the client's session or connection context. This `client_attrs` info field acts like a dictionary or a map, holding the attributes as key-value pairs. This field is maintained in memory associated with the client's session for quick access during the client's connection lifecycle.

### Extract Client Attributes

This section describes from which client attributes EMQX extracts and how attributes are extracted.

#### Sources of Client Metadata and Attributes

In EMQX, client metadata and attributes originate from various sources and are stored within specific system properties for use throughout the client's connection lifecycle. Here's a breakdown of where these existing client information come from and where they are stored:

- **MQTT CONNECT Packet**: When a client connects to EMQX, it sends a CONNECT packet that includes several pieces of information, such as `clientid`, `username`, `password`, and `user properties`.
- **TLS Certificates**: If the client connects using TLS, the client's TLS certificate can provide additional metadata, such as:
  - `cn` (Common Name): Part of the certificate that can identify the device or user.
  - `dn` (Distinguished Name): The full subject field within the certificate that includes several descriptive fields about the certificate owner.
  - Server Name Indication (SNI): Currently used as multi-tenancy tenant id.
- **IP Connection Data**: This includes the client’s IP address and port number, which are automatically captured by EMQX when a client connects.

#### Extraction Expressions

The extraction process uses variform expressions that allow function calls and variable references to define how attributes should be extracted and dynamically process the data. However, the expressions are not fully programmable and support only predefined functions and variables.

##### Syntax

`function_call(clientid, another_function_call(username))` can be used to combine or manipulate client data. The configuration example is as follows:

```bash
mqtt {
    client_attrs_init = [{expression = "conat([clientid, username])"}]
}
```

##### Pre-bound Variables

Pre-bound variables can be directly used in the extraction expressions. A set of variables are pre-bound, including:

- `cn`: Client certificate common name.
- `dn`: Client certificate distinguish name (Subject).
- `clientid`
- `username`
- `user_property`: The user properties provided in the MQTT v5 CONNECT packet sent by the client.
- `ip_address`: The source IP of the client.
- `port`: The source port number of the client
- `zone`: The zone name

##### Pre-defined Functions

EMQX includes a rich set of string, array, random, and hashing functions similar to those available in rule engine string functions. These functions can be used to manipulate and format the extracted data. For instance, `lower()`, `upper()`, and `concat()` help in adjusting the format of extracted strings, while `hash()` and `hash_to_range()` allow for creating hashed or ranged outputs based on the data.

Below are the functions that can be used in the expressions:

- **String functions**: 
  - [String Operation Functions](../data-integration/rule-sql-builtin-functions.md#string-operation-functions)
  - A new function any_to_string/1 is also added to convert any intermediate non-string value to a string.
- **Array functions**: [nth/2](../data-integration/rule-sql-builtin-functions.md#nth-n-integer-array-array-any)
- **Random functions**: rand_str, rand_int
- **Schema-less encode/decode functions**:
  - [bin2hexstr/1](../data-integration/rule-sql-builtin-functions.md#bin2hexstr-data-binary-string)
  - [hexstr2bin/1](../data-integration/rule-sql-builtin-functions.md#hexstr2bin-data-string-binary)
  - [base64_decode/1](../data-integration/rule-sql-builtin-functions.md#base64-decode-data-string-bytes-string)
  - [base64_encode/1](../data-integration/rule-sql-builtin-functions.md#base64-encode-data-string-bytes-string)
  - int2hexstr/1
- **Hash functions**:
  - hash(Algorihtm, Data), where  algorithm can be one of: md4 | md5, sha (or sha1) | sha224 | sha256 | sha384 | sha512 | sha3_224 | sha3_256 | sha3_384 | sha3_512 | shake128 | shake256 | blake2b | blake2s
  - hash_to_range(Input, Min, Max): Use sha256 to hash the Input data and map the hash to an integer between Min and Max inclusive ( Min =< X =< Max)
  - map_to_rage(Input, Min, Max): Map the input to an integer between Min and Max inclusive (Min =< X =< Max)

##### Example Expressions

 `nth(1, tokens(clientid, '.'))`:  Extract the prefix of a dot-separated client ID.

`strlen(username, 0, 5)`: Extract a partial username.

### Merge Authentication Data

EMQX can also merge attributes from different sources such as JSON Web Token (JWT) claims or HTTP authentication responses into the client's attributes. 

- **JWT claims**: If JWTs are used for authentication, they can include `client_attrs` claims that carry additional metadata about the client, such as roles, permissions, or other identifiers.
- **HTTP Authentication Responses**: If EMQX is configured to use an external HTTP service for authentication, the response from this service might include additional attributes or metadata about the client, which can be configured to be captured and stored within EMQX. For example, if an HTTP response includes `"client_attrs": {"group": "g1"}`, EMQX will incorporate this data into the client's existing attributes, which can then be utilized in authorization requests.

## Application of Client Attributes

The extracted and merged attributes can be used in constructing authentication and authorization requests. The `client_attrs.{NAME}` can be used in authentication and authorization template rendering. If an attribute named `client_attrs.alias` is defined, it can be incorporated into an HTTP request body or SQL query, enhancing the flexibility and specificity of these requests.

For example, for an attribute named `client_attrs.alias`, you can use `${client_attrs.alais}` to build an HTTP request body as an HTTP authentication request.  For more details, see [Authentication Placeholders](../access-control/authn/authn.md#authentication-placeholders).

Other applications include: <!-- Need some descriptions about how it is used -->

- Multi-tenancy tenant ID (more flexible tenant ID assignment)
- Per client mountpoint
- Simple match for Authentication (e.g. GOCSP wanted to compare certificate CN with clientid prefix)
- Data field in ACL rules or requests

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

