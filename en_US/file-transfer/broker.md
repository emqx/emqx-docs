# Broker Side of File Transfer

Here we describe the main concepts of file transfer from the perspective of the EMQX broker, i.e., the party that receives file uploads from the clients.

EMQX allows not to configure most of the file transfer settings and use the default values.

## File Transfer MQTT Settings

Since file transfer/export operations may take a significant amount of time, we may want to specify timeouts for particular operations so that the MQTT clients do not wait too long for the result.

We can do this by setting the following options:
```
file_transfer {
    enable = true
    init_timeout = "10s"
    store_segment_timeout = "10s"
    assemble_timeout = "60s"
}
```

`init_timeout`, `store_segment_timeout` and `assemble_timeout` are the timeouts for the corresponding operations. The client will receive a PUBACK packet with `RC_UNSPECIFIED_ERROR` code if the operation is not completed within the specified timeout.

## File Transfer Storage

File transfer _storage_ manages how the broker stores and handles metadata and file segments and assembles the file from the segments. Currently, a single storage backend is supported, the local file storage.

```
{
    file_transfer {
        enable = true
        storage {
            local {
                enable = true
            }
        }
    }
}
```

Local file storage stores the file metadata and segments in the local file system of the nodes which receive the corresponding file transfer commands.

Also, local file storage exports the uploaded files using a configured _exporter_. Currently, two exporters are supported: _local_ and _s3_, exporting files correspondingly to the local file system and S3-compatible object storage.

Several exporters may have settings simultaneously, but only one may be enabled.

### Segement Settings

To configure how the segments are managed, one may specify the following:
* the root directory where the segments are stored;
* segment garbage collection settings


```
{
    file_transfer {
        enable = true
        storage {
            local {
                enable = true
                segments {
                    root = "/var/lib/emqx/file_transfer/segments"
                    gc {
                        interval = "1h"
                        maximum_segments_ttl = "1d"
                        minimum_segments_ttl = "1h"
                    }
                }
            }
        }
    }
}
```

One should choose these configuration parameters carefully, depending on the expected file transfer load, expected file sizes, number of concurrent transfers, and available disk space.

### Local Exporter Settings

Local exporter has few settings: only the root folder for the exported files.

```
{
    file_transfer {
        enable = true
        storage {
            local {
                enable = true
                exporter {
                    local {
                        enable = true
                        root = "/var/lib/emqx/file_transfer/exported"
                    }
                }
            }
        }
    }
}
```

Exported files may be collected for a long time, so thousands of files may be in the export folder. To avoid having overpopulated folders, EMQX uses a bucketing scheme to store the exported files:
* it calculates the sha256 hash of the file id together client id (`ABCDEFG012345...`)
* stores the file into a 6-level folder hierarchy, using as levels:
    * the first 2 bytes of the hash as the first-level folder name;
    * the next 2 bytes as the second-level folder name;
    * the rest of the hash as the 3-rd level folder name;
    * the client id;
    * the file id;
    * the escaped file name as the last level.
  So the exported file will be stored in a folder like this: `AB/CD/EFGH.../clientid/file_id/escaped_file_name_form_the_metadata`.

EMQX provides an API to list and download the exported files.

### S3 Exporter Settings

S3 exporter settings are straightforward (see the corresponding configuration section for details).

The notable settings are:
* `min_part_size` and `max_part_size`. S3 exporter uses multipart upload to upload the files to S3. It gathers the file segments into chunks larger than `min_part_size` before uploading them. Also, segments larger than `max_part_size` will cause an error.
* `transport_settings` - settings for the underlying HTTP(S) connection to S3. Use this to make file upload secure or to manage the connection pool.

Also, S3 exporter does not use the bucketing scheme; they are stored using a 3-level hierarchy:
* the client id
* the file id
* the escaped file name as the last level.
