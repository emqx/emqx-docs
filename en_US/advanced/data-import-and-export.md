# Data import and export

EMQ X provides users with data import and export function to meet the needs of server upgrade, migration and data backup. The data import and export function supports exporting the data (such as the blacklist, rule engine configuration) stored in Mnesia , the default database of EMQ x broker, to the local file in JSON format. Of course, the user does not need to care about the data content in the exported file. The exported file can be imported into other running instances of EMQ x broker. The EMQ x broker can be the same version or different versions, but it currently only supports `4.1.0` and later versions..

EMQ X Broker provides [Command Line Interface](./cli.md#endpoint-data-import-and-export), [HTTP API](./http-api.md#endpoint-data- import-and-export) and Dashboard's visual interface (Enterprise Edition) for data import and export functions. The data currently supported for import and export are as follows:

- Rule engine configuration data (resources, rules)
- Blacklist data
- Application data
- Dashboard user data
- MQTT user data and ACL data added through the emqx-auth-mnesia plugin
- MQTT user data and ACL data added through the emqx-auth-clientid plugin
- MQTT user data and ACL data added through the emqx-auth-username plugin


## Example

### Command line interface

1. Export data. The file name format of the exported file is `emqx-export-YYYY-MM-DD-HH-mm-SS.json`, and the default export path is the data directory (Refer to  [Directory structure](../getting-started/directory.md))

    ```
    $ ./emqx_ctl data export
    The emqx data has been successfully exported to /var/lib/emqx/data/emqx-export-2020-5-15-17-39-0.json.
    ```
    
2. Save the exported file, and we save the exported file to the tmp directory here.

   ```
   $ cp /var/lib/emqx/data/emqx-export-2020-5-15-17-39-0.json /tmp
   ```

3. Reinstall EMQ X Broker and start it

   ```
   $ ./emqx start
   EMQ X Broker v4.1-rc.1 is started successfully!
   ```

4. Import data. The name of the imported file must be specified as an absolute path

    ```
    $ ./emqx_ctl data import /tmp/emqx-export-2020-5-15-17-39-0.json
    The emqx data has been imported successfully.
    ```

### HTTP API

1. Import data.

   ```
   $ curl -i --basic -u admin:public -X POST "http://localhost:8081/api/v4/data/export"

   {"data":{"size":388,"filename":"emqx-export-2020-9-4-10-24-16.json","created_at":"2020-9-4 10:24:16"},"code":0}
   ```

   > The exported data file is located at the directory of `.../emqx/data` or `/var/lib/emqx/data` 

2. Download data file

   ```
   $ curl --basic -u admin:public -X GET http://localhost:8081/api/v4/data/file/emqx-export-2020-9-4-10-24-16.json -o /tmp/emqx-export-2020-9-4-10-24-16.json   
   ```

3. Upload data file

   ```
   $ curl -i --basic -u admin:public -X POST "http://localhost:8081/api/v4/data/import" -d @/tmp/emqx-export-2020-9-4-10-24-16.json

   {"code":0}
   ```

   > Steps 2 and 3 are suitable for migrating emqx on different devices

4. Import Data

    ```
    $ curl -i --basic -u admin:public -X POST "http://localhost:8081/api/v4/data/import" -d '{"filename":"emqx-export-2020-9-4-10-24-16.json"}'

    {"code",0}
    ```
    
    