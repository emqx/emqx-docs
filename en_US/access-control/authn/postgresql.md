# Integrate with PostgreSQL

EMQX supports integrating with PostgreSQL for password authentication. 

::: tip

Knowledge about [basic EMQX authentication concepts](../authn/authn.md)

:::

## Data Schema and Query Statement

EMQX PostgreSQL authenticator supports almost any storage schema. You can determine how to store credentials and access them as your business needs, for example, using one or multiple tables, views, etc.

Users need to provide a query statement template and ensure the following fields are included:

- `password_hash`: required; password (in plain text or hashed) stored in the database; 
- `salt`: optional; `salt = ""` or just remove this field to indicate no salt value will be added; 
- `is_superuser`: optional; flag if the current client is a superuser; default: `false`.

Example table structure for storing credentials:

```sql
CREATE TABLE mqtt_user (
    id serial PRIMARY KEY,
    username text NOT NULL UNIQUE,
    password_hash  text NOT NULL,
    salt text NOT NULL,
    is_superuser boolean DEFAULT false,
    created timestamp with time zone DEFAULT NOW()
);
```

::: tip
The above example has created an implicit `UNIQUE` index field (username) that is helpful for the queries.
When there is a significant number of users in the system, please optimize and index the tables to be queried beforehand to shorten the query response time and reduce the load for EMQX.
:::

In this table, MQTT users are identified by `username`.

For example, if you want to add a document for a superuser (`is_superuser`: `true`) with username `user123`, password `secret`, and suffixed salt `salt`, the query statement should be:

```bash
INSERT INTO mqtt_user(username, password_hash, salt, is_superuser) VALUES ('user123', 'bede90386d450cea8b77b822f8887065e4e5abf132c2f9dccfcc7fbd4cba5e35', 'salt', true);
INSERT 0 1
```

The corresponding configuration parameters are:

- password_hash_algorithm: `sha256`
- salt_position: `suffix`

SQL: 

```sql
query = "SELECT password_hash, salt, is_superuser FROM mqtt_user WHERE username = ${username} LIMIT 1"
```

## Configure with Dashboard

You can use EMQX Dashboard to configure how to use PostgreSQL for password authentication. 

In EMQX Dashboard, click **Access Control** -> **Authentication** from the left navigation menu. On the **Authentication** page, click **Create** at the top right corner. Click to select **Password-Based** as **Mechanism**, and **PostgreSQL** as **Backend** to go to the **Configuration** tab, as shown below. 

<img src="./assets/authn-postgresql.png" alt="Authentication with postgresql" style="zoom:67%;" />

Follow the instructions below on how to configure the authentication:

**Connect**: Enter the information for connecting to PostgreSQL.

- **Server**: Specify the server address that EMQX is to connect (`host:port`).
- **Database**: PostgreSQL database name.
- **Username** (optional): Specify user name. 
- **Password** (optional): Specify user password. 
- **Disable Prepared Statements** (optional): If you are using a PostgreSQL service that does not support prepared statements, such as PGBouncer in transaction mode or Supabase, enable this option. This option was introduced in EMQX v5.7.1.
- **Enable TLS**: Turn on the toggle switch if you want to enable TLS. For more information on enabling TLS, see [Network and TLS](../../network/overview.md).

- **Pool size** (optional): Specify the number of concurrent connections from an EMQX node to a PostgreSQL server. Default: `8`. 

**Authentication configuration**: Configure settings related to authentication:

- **Password Hash**: Select the hash function for storing passwords in the database. Options include `plain`, `md5`, `sha`, `bcrypt`, or `pbkdf2`. Additional configuration depends on your selected function:
  - For `plain`, `md5`, `sha`, `sha256`, or `sha512`:
    - **Salt Position**: Specify how salt (random data) should be added to the password. Options are `suffix`, `prefix`, or `disable`. Keep the default value unless you migrate user credentials from external storage into the EMQX built-in database. Note: For `plain`, the **Salt Position** should be set to `disable`.
  - For `bcrypt`:
    - **Salt Rounds**: Define the number of times the hash function is applied, represented as 2^Salt Rounds, also known as the "cost factor". The default is `10`, with a range of `5` to `10`. A higher setting is recommended for increased security. Note: Increasing the cost factor by 1 doubles the necessary time for authentication.
  - For `pkbdf2`:
    - **Pseudorandom Function**: Select the hash function used for key generation, such as `sha256`.
    - **Iteration Count**: Indicate the number of hash iterations. Default:  `4096`.
    - **Derived Key Length**: Set the desired length of the generated password. This field can be left blank, in which case the key length will default to the output of the selected pseudorandom function.
- **SQL**: Fill in the query statement according to the data schema. For more information, see [SQL data schema and query statement](#sql-table-structure-and-query-statement). 

After you finish the settings, click **Create**.

## Configure with Configuration Items

You can configure the EMQX PostgreSQL authenticator with EMQX configuration items. <!--For detailed operation steps, see [authn-postgresql:authentication](../../configuration/configuration-manual.html#authn-postgresql:authentication). -->

PostgreSQL authentication is identified with `mechanism = password_based` and `backend = postgresql`.

Sample configuration:

```bash
{
  mechanism = password_based
  backend = postgresql
  enable = true

  password_hash_algorithm {
    name = sha256
    salt_position = suffix
  }

  database = mqtt
  username = postgres
  password = public
  server = "127.0.0.1:5432"
  query = "SELECT password_hash, salt, is_superuser FROM users where username = ${username} LIMIT 1"
}
```
