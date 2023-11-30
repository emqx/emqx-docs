# Configure File Transfer Server-Side Settings

EMQX does not enable the MQTT file transfer feature by default. If you wish to use this feature, you need to enable it in the configuration file.

This page mainly introduces how to enable the file transfer feature in EMQX and configure various file transfer functionalities on the EMQX server side, including segment storage, exporting merged files to local disk and S3 buckets, and managing exported files. It also explains how to configure MQTT transfer settings to optimize server-side file transfer operations.

## Enable File Transfer

In EMQX, the file transfer feature is disabled by default, and you need to enable it in the configuration file.

```bash
file_transfer {
  enable = true
}
```

With this configuration, EMQX will default to storing segment files in the `data/file_transfer/segments` directory and enable local disk export, exporting merged files to the `data/transfers/exports` directory.

If you need to further configure the file transfer feature, you can refer to the following configuration instructions.

## Configure Segment Storage

EMQX supports clients uploading file segments, which are merged into a complete file after receiving all segments. To support this, EMQX needs to temporarily store the segment files and manage them.

Currently, EMQX only supports storing segment files on disk, and you can configure the storage location of the segments.

Once the file upload is complete, the segments will be automatically cleaned up. For files that have not been uploaded successfully within the timeout period, you can configure the validity period of segments and schedule cleaning time to avoid occupying disk space.

```bash
file_transfer {
  # Enable file transfer feature
  enable = true

  # Segment storage configuration
  storage.local.segments = {
    # Segment storage directory, preferably set on high I/O performance disks.
    root = "./data/file_transfer/segments"
    
    # Scheduled cleaning of expired segment files
    gc {
      # Cleaning interval
      interval = "1h"

      # Maximum valid period for segment storage, segments will be cleared after this period, even if they have not been merged.
      # The validity period specified by the client must not exceed this value.
      maximum_segments_ttl = "24h"
    }
  }
}
```

You need to set a reasonable configuration based on the expected file size, concurrent transmission number, and available disk space.

## Configure File Export

After all segments are transmitted, EMQX supports merging segments into a complete file and exporting the complete file to a local disk or S3 bucket for application integration.

:::tip

If file export is not configured, it defaults to local disk export. EMQX does not support configuring both export methods simultaneously; only one can be used.

:::

### File Export to Local Disk

Use the following configuration example to save the merged complete file on the local disk. You can configure the storage location and validity period of the exported file.

```bash
file_transfer {
  # Enable file transfer feature
  enable = true

  # Segment storage configuration
  # ...

  # Enable local disk file export
  storage.local.exporter.local {
    enable = true
    # Export file storage directory, preferably set on high I/O performance disks.
    root = "./data/transfers/exports"
  }
}
```

### File Export to S3 Bucket

Use the following configuration example to save the merged complete file in an S3 bucket.

```bash
file_transfer {
  # Enable file transfer feature
  enable = true

  # Segment storage configuration
  # ...

  # Enable S3 bucket file export
  storage.local.exporter.s3 {
    enable = true

    host = "s3.us-east-1.amazonaws.com"
    port = 443

    # Credentials for accessing S3
    access_key_id = "AKIA27EZDDM9XLINWXFE"
    secret_access_key = "******"

    # Export file storage bucket
    bucket = "my-bucket"

    # Shared URL expiration time
    # EMQX generates a temporary shared URL for clients to download files directly from S3. This parameter specifies the expiration time of the file download URL returned by the EMQX API. After expiration, the URL becomes unavailable, although the actual file remains in S3.
    #url_expire_time = "1h"

    # Settings for the underlying HTTP(S) connection with S3, allowing secure file upload and connection pool management.
    transport_options {
      ssl.enable = true
      connect_timeout = 15s
    }
  }
}
```

## Manage Exported Files

You can manage exported files, such as listing files to browse detailed information, moving, deleting, or downloading files. You can perform these management operations via the REST API or manually. Future versions will add a Dashboard interface for managing exported files.

### Manage Exported Files via REST API

EMQX provides REST APIs for managing exported files, and you can use the [MQTT File Transfer Management API](https://docs.emqx.com/en/enterprise/v5.3/admin/api-docs.html#tag/File-Transfer) for management, enabling file browsing and downloading.

### Manually Manage Disk Exported Files

If you need to manage exported files directly on the disk, such as moving files, or using your FTP or HTTP service for download, you can refer to the following instructions for file storage locations.

To address file name conflicts and excessive numbers of files in a single directory, EMQX uses a bucket storage scheme for saving exported files. The working principle of the scheme is as follows:

- First, calculate the sha256 hash value of the file ID and client ID, e.g., `ABCDEFG012345...`.
- Store the file in a 6-level directory structure, each defined as follows:
  1. The first two bytes of the hash as the first-level directory name;
  2. The next two bytes as the second-level directory name;
  3. The remaining hash as the third-level directory name;
  4. Escaped client ID;
  5. Escaped file ID;
  6. The file name from the metadata as the last layer.

For example, an exported file might be stored in a directory structure like this:  `AB/CD/EFGH.../{clientid}/{file_id}/{filename}`.

### Manually Manage S3 Bucket Exported Files

For S3 buckets, you can use S3 client tools or S3's REST API for management, enabling file deletion, download, etc. You can refer to the following instructions for file storage locations.

Unlike the bucket storage scheme used by the local exporter, files exported using the S3 exporter are stored in a simpler 3-level hierarchical structure:

1. Escaped client ID;
2. Escaped file ID;
3. File name.

For example, an exported file might be stored in a directory structure like this:  `{clientid}/{file_id}/{filename}`.

::: tip

For more about using S3 client tools or REST API, refer to the following resources:

- [Amazon S3](https://aws.amazon.com/s3/?nc1=h_ls) and its [User Guide](https://docs.aws.amazon.com/AmazonS3/latest/userguide/Welcome.html)
- [MinIO Object Storage System](https://min.io/)

:::

## Configure MQTT Transfer Settings

To optimize file transfer operations and prevent clients from waiting excessively, we can set specific timeouts for different file transfer operations. The following MQTT settings can be configured:

```bash
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
