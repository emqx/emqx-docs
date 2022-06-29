# Built-in Database

This authorizer implements ACL checks through matching pub/sub requests against lists of rules stored in the
built-id database (Mnesia).

The advantage of this method is that it does not require any external dependencies and allows manipulating
client's ACL rules through a REST API.

## Configuration

The built-in database authorizer is identified by type `built_in_database`.

Sample configuration:

```
{
    type = built_in_database
    enable = true
}
```

## ACL rule structure

Rules for the authorizer are managed through `/api/v5/authorization/sources/built_in_database` API endpoint.

Each rule is applied to
* a particular client identified by username, `/api/v5/authorization/sources/built_in_database/username` endpoint;
* a particular client identified by clientid, `/api/v5/authorization/sources/built_in_database/clientid` endpoint;
* all clients, `/api/v5/authorization/sources/built_in_database/all` endpoint.

Rules contain:
* permission, `allow` or `deny`;
* action, i.e., the relevant operation: `publish`, `subscribe`, or `all`;
* topic filter, possibly with wildcards or [placeholders](authz.md#topic-placeholders).
