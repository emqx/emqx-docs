# Management APIs

EMQX exposes a HTTP management API which by default listens on port 18083.
The APIs are described by OpenAPI (Swagger) 3.0 specification.

When EMQX is started in localhost, you can visit
http://localhost:18083/api-docs/index.html
to view the API document, and also experiment with the management APIs from the Swagger UI.

The `/api-docs` endpoint does not require login, however to go further from there,
you'll need to go through a few simple steps to setup the management API authentication.

The reset of this document is to guide you through the steps.

## Change default user password

EMQX dashboard and management API comes with a default user `admin`, and the default password
is `public`. The default password should be changed as soon as possible.

You may add or change `dashboard.default_password` in `emqx.conf` to bootstrap the default user.
You also set environment variable `EMQX_DASHBOARD__DEFAULT_PASSWORD` to bootstrap the default user.

::: warning
EMQX administrative users are stored in its builtin database, once the database is bootstrapped
changing the config or environment variable will not take effect.
After bootstrapped, the only way to change a user's password is to use CLI command:
`emqx ctl admins passwd admin new-password`
:::

## Create administrative users

### Users

Using the default user (by default `admin`), more users can be created from
dashboard, management API or CLI.

::: warning
Administrative users all have the privilege.
That is, there is no role based permission management at the moment.
For tooling and scripting purposes, you may create API Keys which
is prohibited to login dashboard, manage users or other API Keys.
:::

### API Keys

You may create 'app' `key:secret` pairs from the dashboard or by calling the
`api_key` API like below:

```
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

```
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

```
curl -u a87465f14ca0d420:LECuyY4VAnndsYRkjtWO2vFTi80FvohmhVgOeNeorMN \
     -X 'GET' 'http://localhost:18083/api/v5/nodes' -H 'accept: text/plain'
```
