# Hot Upgrade

## Release Upgrade

Since version 4.2.0, EMQX enterprise supports hot upgrades.

By using the hot upgrade feature, users can quickly and safely upgrade the EMQX Broker in the production environment and avoid the decrease in system availability caused by restarting the service.

Currently, EMQX Broker only supports hot upgrades of Patch version (Patch version is the third digit of the version number).
That is, it currently supports hot upgrades from 4.2.0 -> 4.2.1, 4.2.0 -> 4.2.2, ..., etc., but 4.2.x cannot be hot upgraded to 4.3.0 or 5.0.

Currently, Windows and MacOS do not support the hot upgrade feature.

## Hot Upgrade Steps

1. View the currently installed version list of EMQX Broker.

```bash

$ emqx versions

Installed versions:
* 5.0.0 permanent
```

2. Download the software package to be upgraded from the EMQX website.

Visit https://www.emqx.com/en/downloads?product=broker, select the corresponding version and operating system type, and then select the **"zip"** package type.

3. Find the installation directory of EMQX:

```bash

$ EMQX_ROOT_DIR=$(emqx root_dir)

$ echo ${EMQX_ROOT_DIR}
"/usr/lib/emqx"

```

4. Put the downloaded zip package in the `releases` directory under the EMQX installation directory:

```bash

$ cp emqx-5.0.1.zip ${EMQX_ROOT_DIR}/releases/

```

5. Upgrade to the specified version:

```bash

$ emqx upgrade 5.0.1

Release 5.0.1 not found, attempting to unpack releases/emqx-5.0.1.tar.gz
Unpacked successfully: "5.0.1"
Installed Release: 5.0.1
Made release permanent: "5.0.1"
```

6. Check the version list again, and the status of the previous version will become `old`:

```bash

$ emqx versions

Installed versions:
* 5.0.1 permanent
* 5.0.0 old
```

## Manually Permanent After Upgrade

The above `emqx upgrade 5.0.1` command actually performs three actions:

- `unpack`
- `install`
- `permanent`

After permanent, this version upgrade will be fixed, which means that after the hot upgrade, if emqx restarts, the new version after the upgrade will be used.

If you don't want to persist while upgrading, you can use the `--no-permanent` parameter:

```bash

$ emqx upgrade --no-permanent 5.0.1

Release 5.0.1 not found, attempting to unpack releases/emqx-5.0.1.tar.gz
Unpacked successfully: "5.0.1"
Installed Release: 5.0.1

```

At this time, the version has been successfully upgraded to 5.0.1. However, if you restart emqx, it will revert to the old version 5.0.0.
Now, if you check the version list, you will find that the state of 5.0.1 is `current`, not the permanent version:

```bash

$ emqx versions

Installed versions:
* 5.0.1 current
* 5.0.0 permanent

```

After the system has been running stably for a period of time, if you decide to make the new version permanent, you can execute the `install` command again:

```bash

$ emqx install 5.0.1

Release 5.0.1 is already installed and current, making permanent.
Made release permanent: "5.0.1"

```

## Downgrade to Previous Versions

If you find a problem and want to roll back after the upgrade, you can execute the version downgrade command.
For example, the following example will roll back emqx to version 5.0.0:

```bash

$ emqx downgrade 5.0.0

Release 5.0.1 is marked old, switching to it.
Installed Release: 5.0.0
Made release permanent: "5.0.0"

```

## Delete Versions

After the system has been running stably for a period of time, if you decide to delete an old version, you can execute the version uninstall command.
For example, the following example will uninstall the old version 5.0.0:

```bash

$ emqx uninstall 5.0.0

Release 5.0.0 is marked old, uninstalling it.
Uninstalled Release: 5.0.0

```
