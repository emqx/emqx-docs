
## Hot upgrade

Since version 4.2.0, EMQ X Broker supports hot upgrade.

By using the hot upgrade feature, users can quickly and safely upgrade the EMQ X Broker in the production environment, and avoid the decrease in system availability caused by restarting the service.

Currently EMQ X Broker only supports hot upgrade of Patch version (Patch version is the third digit of the version number).
That is, it currently supports hot upgrades from 4.2.0 -> 4.2.1, 4.2.0 -> 4.2.2, ..., etc., but 4.2.x cannot be hot upgraded to 4.3.0 or 5.0.

Currently, Windows, MacOSX does not support hot upgrade feature.

## Hot upgrade steps

1. View the currently installed version list of EMQ X Broker.

```bash

$ emqx versions

Installed versions:
* 4.2.0	permanent
```

2. Download the software package to be upgraded from the EMQ X website.

Visit https://www.emqx.com/en/downloads?product=broker, Select the corresponding version and operating system type, and then select the **"zip"** package type.

3. Find the installation directory of EMQ X:

```bash

$ EMQX_ROOT_DIR=$(emqx root_dir)

$ echo ${EMQX_ROOT_DIR}
"/usr/lib/emqx"

```

4. Put the downloaded zip package in the `releases` directory under the EMQ X installation directory:

```bash

$ cp emqx-4.2.1.zip ${EMQX_ROOT_DIR}/releases/

```

5. Upgrade to the specified version:

```bash

$ emqx upgrade 4.2.1

Release 4.2.1 not found, attempting to unpack releases/emqx-4.2.1.tar.gz
Unpacked successfully: "4.2.1"
Installed Release: 4.2.1
Made release permanent: "4.2.1"
```

6. Check the version list again, and the status of the previous version will become `old`:

```bash

$ emqx versions

Installed versions:
* 4.2.1	permanent
* 4.2.0	old
```

## Manually permanent after upgrade

The above `emqx upgrade 4.2.1` command actually performs three actions:

- `unpack`
- `install`
- `permanent`

After permanent, this version upgrade will be fixed, which means that after the hot upgrade, if emqx restarts, the new version after the upgrade will be used.

If you don't want to persist while upgrading, you can use the `--no-permanent` parameter:

```bash

$ emqx upgrade --no-permanent 4.2.1

Release 4.2.1 not found, attempting to unpack releases/emqx-4.2.1.tar.gz
Unpacked successfully: "4.2.1"
Installed Release: 4.2.1

```

At this time, the version has been successfully upgraded to 4.2.1. However, if you restart emqx, it will revert to the old version 4.2.0.
Now, if you check the version list, you will find that the state of 4.2.1 is `current`, not the permanent version:

```bash

$ emqx versions

Installed versions:
* 4.2.1	current
* 4.2.0	permanent

```

After the system has been running stably for a period of time, if you decide to make the new version permant, you can execute the `install` command again:

```bash

$ emqx install 4.2.1

Release 4.2.1 is already installed and current, making permanent.
Made release permanent: "4.2.1"

```

## Downgrade to pervious versions

If you find a problem and want to roll back after the upgrade, you can execute the version downgrade command.
For example, the following example will roll back emqx to version 4.2.0:

```bash

$ emqx downgrade 4.2.0

Release 4.2.0 is marked old, switching to it.
Installed Release: 4.2.0
Made release permanent: "4.2.0"

```

## Delete versions

After the system has been running stably for a period of time, if you decide to delete an old version, you can execute the version uninstall command.
For example, the following example will uninstall the old version 4.2.0:

```bash

$ emqx uninstall 4.2.0

Release 4.2.0 is marked old, uninstalling it.
Uninstalled Release: 4.2.0

```
