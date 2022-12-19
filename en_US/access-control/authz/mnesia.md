# Built-in Database

This authorizer implements authorization checks through matching pub/sub requests against lists of rules stored in the
built-id database (Mnesia).

The advantage of this method is that it does not require any external dependencies and allows manipulating
client's authorization rules through a REST API.

## Configuration

The built-in database authorizer is identified by type `built_in_database`.

Sample configuration:

```
{
    type = built_in_database
    enable = true
}
```

## Data Management

Rules can be managed from the dashboard UI "Access Contorl" -> "Authorization" -> "Built-in Database" -> "Users"
and click on the "+Add" button to add new clients and its rules.

Rules are also managed through `/api/v5/authorization/sources/built_in_database` APIs.

Each rule is applied to
* a particular client identified by username, `/api/v5/authorization/sources/built_in_database/username` endpoint;
* a particular client identified by clientid, `/api/v5/authorization/sources/built_in_database/clientid` endpoint;
* all clients, `/api/v5/authorization/sources/built_in_database/all` endpoint.

You can find more details and examples in document API (Swagger UI) available at http://dashboard-host:18083/api-docs
below is a quick example for how to create rules for a client (`client1`):

```
curl -X 'POST' \
  'http://localhost:18083/api/v5/authorization/sources/built_in_database/clientid' \
  -H 'accept: */*' \
  -H 'Content-Type: application/json' \
  -d '[
  {
    "clientid": "client1",
    "rules": [
      {
        "action": "publish",
        "permission": "allow",
        "topic": "test/toopic/1"
      },
      {
        "action": "subscribe",
        "permission": "allow",
        "topic": "test/toopic/2"
      },
      {
        "action": "all",
        "permission": "deny",
        "topic": "eq test/#"
      }
    ]
  }
]'
```

Rules contain:
* permission, `allow` or `deny`;
* action, i.e., the relevant operation: `publish`, `subscribe`, or `all`;
* topic filter, possibly with wildcards or [topic placeholders](authz.md#topic-placeholders).
