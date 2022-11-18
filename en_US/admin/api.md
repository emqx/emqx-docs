# HTTP APIs

EMQX exposes a HTTP management API which by default listens on port 18083.
The APIs are described by OpenAPI (Swagger) 3.0 specification.

When EMQX is started in localhost, you can visit [http://localhost:18083/api-docs/index.html](http://localhost:18083/api-docs/index.html)
to view the API document, and also experiment with the management APIs from the Swagger UI.

The reset of this document is to guide you to get started quickly with the EMQX REST API.

## Basic Path

EMQX has version control on the REST API, all API paths from EMQX 5.0.0 start with `/api/v5`.

## Authenticating

EMQX's REST API using [Basic Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication).

You can create an API key in the Dashboard "System" -> "API Keys" page, or you can calling the `api_key` API like below:

```bash
curl -u 'admin:public' \
     -X 'POST' 'http://localhost:18083/api/v5/api_key' \
     -H 'accept: application/json' \
     -H 'Content-Type: application/json' \
     -d '{
            "name": "EMQX-API-KEY-3",
            "expired_at": "2022-12-05T02:01:34.186Z",
            "desc": "for testing",
            "enable": true
        }'
```

An example response:

```bash
{
  "api_key": "a87465f14ca0d420",
  "api_secret": "LECuyY4VAnndsYRkjtWO2vFTi80FvohmhVgOeNeorMN",
  "created_at": "2022-06-21T22:28:23+02:00",
  "desc": "for testing",
  "enable": true,
  "expired": false,
  "expired_at": "2022-12-05T03:01:34+01:00",
  "name": "EMQX-API-KEY-3"
}
```

Then the `api_key` and `api_secret` can be used to access management APIs
using HTTP basic auth. For instance:

```bash
curl -u a87465f14ca0d420:LECuyY4VAnndsYRkjtWO2vFTi80FvohmhVgOeNeorMN \
     -X 'GET' 'http://localhost:18083/api/v5/nodes'
```

::: tip
The `api_secret` is only returned once when it is created, please save it in time.
:::

## HTTP Headers

Unless otherwise specified, most operations require the `Accept` header to be set to `application/json`, and the response will be returned in JSON format.

## HTTP Response Status Code

EMQX follows the [HTTP Response Status Code](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status) standard, and the possible status codes are as follows:

| Codes | Description                                                                              |
| ----- | ---------------------------------------------------------------------------------------- |
| 200   | Request successfull, and the returned JSON data will provide more details                |
| 201   | Created successfull, and the new object will be returned in the Body                     |
| 204   | Request successfull. Usually used for delete and update operations, and th Body is empty |
| 400   | Bad Request. Usually a request body or parameter error                                   |
| 401   | Unauthorized. API key is expired or does not exist                                       |
| 403   | Forbidden. Check if the object is in use or has dependency constraints                   |
| 404   | Not Found. You can refer to the `message` field in the Body to check the reason          |
| 409   | Conflict. The object already exists or the number limit has been exceeded                |
| 500   | Internal Server Error. You need to use the Body and logs to find out the reason          |

## Error Codes

The HTTP response status codes make it simple to judge the possible problems. EMQX also defines a list of error codes to identify the specific errors.

When an error happens, the error code is returned in JSON format by the Body:

```bash
# GET /clients/foo

{
  "code": "RESOURCE_NOT_FOUND",
  "reason": "Client id not found"
}
```

| Error Codes                                    | Description                                                               |
| ---------------------------------------------- | ------------------------------------------------------------------------- |
| WRONG_USERNAME_OR_PWD                          | Wrong username or pwd <img width=200/>                                    |
| WRONG_USERNAME_OR_PWD_OR_API_KEY_OR_API_SECRET | Wrong username & pwd or key & secret                                      |
| BAD_REQUEST                                    | Request parameters are not legal                                          |
| NOT_MATCH                                      | Conditions are not matched                                                |
| ALREADY_EXISTS                                 | Resource already existed                                                  |
| BAD_CONFIG_SCHEMA                              | Configuration data is not legal                                           |
| BAD_LISTENER_ID                                | Bad listener ID                                                           |
| BAD_NODE_NAME                                  | Bad Node Name                                                             |
| BAD_RPC                                        | RPC Failed. Check the cluster status and the requested node status        |
| BAD_TOPIC                                      | Topic syntax error, Topic needs to comply with the MQTT protocol standard |
| EXCEED_LIMIT                                   | Create resources that exceed the maximum limit or minimum limit           |
| INVALID_PARAMETER                              | Request parameters is not legal and exceeds the boundary value            |
| CONFLICT                                       | Conflicting request resources                                             |
| NO_DEFAULT_VALUE                               | Request parameters do not use default values                              |
| DEPENDENCY_EXISTS                              | Resource is dependent by another resource                                 |
| MESSAGE_ID_SCHEMA_ERROR                        | Message ID parsing error                                                  |
| INVALID_ID                                     | Bad ID schema                                                             |
| MESSAGE_ID_NOT_FOUND                           | Message ID does not exist                                                 |
| NOT_FOUND                                      | Resource was not found or does not exist                                  |
| CLIENTID_NOT_FOUND                             | Client ID was not found or does not exist                                 |
| CLIENT_NOT_FOUND                               | Client was not found or does not exist(usually not a MQTT client)         |
| RESOURCE_NOT_FOUND                             | Resource not found                                                        |
| TOPIC_NOT_FOUND                                | Topic not found                                                           |
| USER_NOT_FOUND                                 | User not found                                                            |
| INTERNAL_ERROR                                 | Server inter error                                                        |
| SERVICE_UNAVAILABLE                            | Service unavailable                                                       |
| SOURCE_ERROR                                   | Source error                                                              |
| UPDATE_FAILED                                  | Update failed                                                             |
| REST_FAILED                                    | Reset source or config failed                                             |
| CLIENT_NOT_RESPONSE                            | Client not responding                                                     |


<ClientOnly>
  <OpenApi path="swagger.json" />
</ClientOnly>
