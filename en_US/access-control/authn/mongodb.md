# Use MongoDB for password authentication

EMQX supports integrating with MongoDB for password authentication. EMQX MongoDB authenticator currently supports connecting to MongoDB running in three different modes, which are [Single](<!--链接待补充-->), [Replica Set](https://www.mongodb.com/docs/manual/reference/replica-configuration/) and [Sharding](https://www.mongodb.com/docs/manual/sharding/). Below we will give detailed instruction on the data schema we supported and how to configure with EMQX Dashboard and configuration file. 

::: tip

- Knowledge about [basic EMQX authentication concepts](../authn/authn.md)

:::

## Data schema and query statement

EMQX MongoDB authenticator supports storing authentication data as MongoDB documents. Users need to provide a query statement template and ensure the following fields are included:

- `password_hash`: required; password (in plain text or hashed) stored in the database; this field supports renaming;
- `salt`: optional; `salt = ""` or just remove this field to indicate no salt value will be added; this field supports renaming;
- `is_superuser`: optional; flag if the current client is a superuser; default: `false`; this field supports renaming.

For example, if we want to add a document for a superuser (`is_superuser`: `true`) with username `user123`, password `secret`, prefixed salt `slat_foo123`, and password hash `sha256`, the query statement should be:

```
> db.mqtt_user.insertOne(
  {
      "username": "emqx_u",
      "salt": "slat_foo123",
      "is_superuser": true,
      "password_hash": "44edc2d57cde8d79c98145003e105b90a14f1460b79186ea9cfe83942fc5abb5"
  }
);
{
  "acknowledged" : true,
  "insertedId" : ObjectId("631989e20a33e26b05b15abe")
}
```

:::tip 

When there is a significant number of users in the system, please optimize and index the collection to be queried beforehand to shorten the query response time and reduce the load for EMQX.

 :::

For this MongoDB data schema, the corresponding Dashboard configuration parameters are: 

- **Password Hash**: `sha256`
- **Salt Position**: `suffix`
- **Collection**: `mqtt_user`
- **Filter**: `{ username = "${username}" }`
- **Password Hash field**: `password_hash`
- **Salt Field**: `salt`
- **is_superuser Field**： `is_superuser`

## Configure with Dashboard

You can use EMQX Dashboard to configure how to use MongoDB for password authentication. 

On [EMQX Dashboard](http://127.0.0.1:18083/#/authentication), click **Access Control** -> **Authentication** on the left navigation tree to enter the **Authentication** page. Click **Create** at the top right corner, then click to select **Password-Based** as **Mechanism**, and **MongoDB** as **Backend**, this will lead us to the **Configuration** tab, as shown below. 

![Authenticate with MondoDB](/Users/lena/Library/Application Support/typora-user-images/image-20230112171929784.png)

Follow the instruction below on how to configure:

**Connect**: In this section, we will fill in the information needed to connect MongoDB.

- **MongoDB Mode**: Select how MongoDB is deployed, including **Single**, **Replica Set** and **Sharding**. 
- **Server**:  Specify the MongoDB server address that EMQX is to connect, if **MongoDB Mode** is set to **Replica Set** or **Sharding**, you will need to input all MondoDB servers (separated with a `,`) that EMQX is to connect.
- **Replica Set Name**: Specify the Replica Set name to use; type: strings; only needed if you set **MongoDB Mode** to **Replica Set**.
- **Database**: MongoDB database name; Data type: strings.
- **Collection**: Name of MongoDB collection where authentication rules are stored; Data type: strings.
- **Username** (optional): Specify MongoDB user name. 
- **Password** (optional): Specify MongoDB user password. 
- **Read Mode** (optional): only needed if you set **MongoDB Mode** to **Replica Set**; Default: **master**; Options: **master**, **slave_ok**. 
  - **master**: Indicate each query in a sequence must only read fresh data (from a master/primary server). If the connected server is not a master then the first read will fail, and the remaining operations will be aborted. 
  - **slave_ok**: Indicate every query is allowed to read stale data from a slave/secondary (or fresh data from a master).

- **Write Mode **(optional):  only needed if you set **MongoDB Mode** to **Replica Set**; Default: **unsafe**; Options: **unsafe**, **safe**.

**TLS Configuration**: Turn on the toggle switch if you want to enable TLS. 

**Connection Configuration**: In this section, we will set the concurrent connections and waiting time before a connection is timed out.

- **Pool size** (optional): Input an integer value to define the number of concurrent connections from an EMQX node to a MongoDB server. Default: **8**. 
- **Connect Timeout** (optional): Specify the waiting period before EMQX assumes the connection is timed out. Units supported include milliseconds, second, minute, and hour. 

**Authentication configuration**: In this section, we will fill in the authentication-related settings:

- **Password Hash Field**: Specify the field name of the password.
- **Password Hash**: Select the Hash function for storing the password in the database, for example, plain, md5, sha, bcrypt, pbkdf2. 
  - If **plain**, **md5**, **sha**, **sha256** or **sha512** are selected, we also need to configure:
    - **Salt Position**: Specify the way (**suffix**, **prefix**, or **disable**) to add salt (random data) to the password. Note: If **plain** is selected, the **Salt Position** should be **disable**. 

  - If **bcrypt** is selected, no extra configurations are needed. 
  - If **pkbdf2** is selected, we also need to configure:
    - **Pseudorandom Function**: Specify the Hush functions to generate the key, such as sha256. 
    - **Iteration Count**: Specify the iteration times; Default: 4096
    - **Derived Key Length**: Specify the length of the generated password, if left blank, the password length will be determined by the pseudorandom function you selected. 

- **Salt Field**: Salt field in MongoDB. 
- **is_superuser Field**: Specify if the user is a super user. 
- **Filter**: A map interpreted as MongoDB selector for credential lookup. [Placeholders](./authn.md#authentication-placeholders) are supported. 

Now we can click **Create** to finish the settings. 

## Configure with configuration items

You can configure the EMQX MongoDB authenticator with EMQX  EMQX configuration items, for detailed operating steps, see 

- [authn-mongodb:standalone](../../admin/cfg.md#authn-mongodb:standalone)
- [authn-mongodb:sharded-cluster](../../admin/cfg.md#authn-mongodb:sharded-cluster) 
- [authn-mongodb:replica-set](../../admin/cfg.md#authn-mongodb:replica-set)

Below are code examples you may refer to:

:::: tabs type:card

::: tab Single mode

```
{
  mechanism = password_based
  backend = mongodb
  enable = true

  password_hash_algorithm {
    name = sha256
    salt_position = suffix
  }

  collection = "mqtt_user"
  filter { username = "${username}" }

  mongo_type = single
  server = "127.0.0.1:27017"

  database = "mqtt"
  username = "emqx"
  password = "secret"
}
```

:::

::: tab Replica set

```
{
  mechanism = password_based
  backend = mongodb
  enable = true

  password_hash_algorithm {
    name = sha256
    salt_position = suffix
  }

  collection = "mqtt_user"
  filter { username = "${username}" }

  mongo_type = rs
  servers = "10.123.12.10:27017,10.123.12.11:27017,10.123.12.12:27017"
  replica_set_name = "rs0"

  database = "mqtt"
  username = "emqx"
  password = "secret"
}
```

:::

::: tab Sharding:

```
{
  mechanism = password_based
  backend = mongodb
  enable = true

  password_hash_algorithm {
    name = sha256
    salt_position = suffix
  }

  collection = "mqtt_user"
  filter { username = "${username}" }

  mongo_type = sharded
  servers = "10.123.12.10:27017,10.123.12.11:27017,10.123.12.12:27017"

  database = "mqtt"
  username = "emqx"
  password = "secret"
}
```

:::

::::
