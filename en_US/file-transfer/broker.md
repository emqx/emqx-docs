# File Transfer Configurations

This page provides instructions on configuring the file transfer settings in EMQX. Topics will cover the storage options available for managing file metadata, segments, and exporting files, as well as the MQTT settings for file transfer operations. Specifically, instructions will focus on the local storage backend and the S3 exporter for file export.

EMQX allows not to configure most of the file transfer settings and use the default values.

## Configure File Transfer Storage

EMQX provides storage options for managing file metadata, segments, and exporting files. Currently, the EMQX supports a single storage backend: local file storage.

To enable local file storage, use the following configuration:

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

With the local file storage, EMQX stores file metadata and segments in the local file system of the receiving nodes. Additionally, the local file storage exports uploaded files using a configured exporter. EMQX supports two exporters: the local exporter and the S3 exporter, exporting files correspondingly to the local file system and S3-compatible object storage. Several exporters may have settings simultaneously, but only one may be enabled.

### Segment Settings

Segment settings allow you to configure how the segments are managed. The following parameters can be specified:

- `root`: The root directory where segments are stored.
- Segment garbage collection settings.


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

It is important to choose appropriate values for these parameters based on factors such as expected file transfer load, file sizes, concurrent transfers, and available disk space.

### Local Exporter Settings

The local exporter allows exporting files to a local file system. It only has the setting of the root directory for the exported files.

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

Exported files may accumulate over time, potentially resulting in a large number of files in the export folder. To address this, EMQX utilizes a bucketing scheme to store exported files. The scheme involves the following 6-level directory hierarchy:

- Calculate the hash of the file id together with client id (`ABCDEFG012345...`).
- Store the file in a 6-level directory hierarchy, with each level defined as follows:
  * The first 2 bytes of the hash as the first-level directory name;
  * The next 2 bytes as the second-level directory name;
  * The rest of the hash as the 3-rd level directory name;
  * The client id;
  * The file id;
  * The escaped file name as the last directory.

For example, an exported file might be stored in a directory structure like this: `AB/CD/EFGH.../clientid/file_id/escaped_file_name_from_the_metadata`.

EMQX provides an API for listing and downloading the exported files.

### S3 Exporter Settings

The S3 exporter allows exporting files to an S3-compatible object storage system. It has the following configuration settings:

- `min_part_size` and `max_part_size`: The S3 exporter utilizes multipart upload for file uploads to S3. Segments are gathered into chunks larger than `min_part_size` before uploading, while segments larger than `max_part_size` will result in an error.
- `transport_settings`: Settings for the underlying HTTP(S) connection to S3, allowing secure file uploads and connection pool management.

Unlike the bucketing scheme used in the local exporter, files exported using the S3 exporter are stored using a simpler 3-level hierarchy:

- The client ID.
- The file ID.
- The escaped file name.

## Configure File Transfer MQTT Settings

To optimize file transfer operations and prevent clients from waiting excessively, we can set specific timeouts for different file transfer operations. The following MQTT settings can be configured:

```
file_transfer {
    enable = true
    init_timeout = "10s"
    store_segment_timeout = "10s"
    assemble_timeout = "60s"
}
```

- `init_timeout`: Timeout for initialization operation.
- `store_segment_timeout`: Timeout for storing file segments.
- `assemble_timeout`: Timeout for file assembly.

If any of these operations exceed the specified timeout, the MQTT client will receive a PUBACK packet with the `RC_UNSPECIFIED_ERROR` code.
