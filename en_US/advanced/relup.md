
## Hot Upgrade

EMQX support hot upgrade since version 4.2.0.

With the hot upgrade feature, users can quickly and safely upgrade the running EMQX in production environment while keeping all the clients connected, ensure high availability because there is no need to restart the service.

::: tip
Currently EMQX for Windows and MacOS does not support the hot upgrade feature.
:::

::: warning
EMQX only allows hot upgrade between patch versions (the third digit of the version number). Please operate with caution.
e.g. You can upgrade to 4.4.16 from 4.4.1, 4.4.2, ..., but not from 4.3.1, 4.3.2, ...
:::

## Download the Hot Upgrade Zip Package

{% emqxce %}
Go to [Download EMQX](https://www.emqx.com/en/downloads?product=broker) for the version and the OS you want, and then select the **"zip"** in the "Install Method".

The Zip package name format of EMQX version 4.4 is:

```
emqx-[EMQX Version]-[OTP Version]-[OS Type]-[Arch].zip
```

Take `emqx-4.4.16-otp24.3.4.2-1-ubuntu20.04-amd64.zip` for example:

- EMQX Version(`4.4.16`): EMQX version number. Please ensure that the running EMQX is on version 4.4.*, but not 4.3 or 5.0 etc.
- OTP Version(`otp24.3.4.2-1`): Erlang OTP version number. Please ensure that the first digit (here is 24) must be the same as the version of the running EMQX.
- OS Type(`ubuntu20.04`): OS type. Please ensure that it is same as the OS Type of the running EMQX.
- Arch(`amd64`): Type of the architecture. Please ensure that it is the same as the arch type of the running EMQX.
{% endemqxce %}

{% emqxee %}
Go to [EMQX Enterprise](https://www.emqx.com/en/try?product=enterprise) for the version and the OS you want, and then select the **"zip"** in the "Install Method".

The Zip package name format of EMQX Enterprise version 4.4 is:

```
emqx-[EMQX Type]-[EMQX Version]-[OTP Version]-[OS Type]-[Arch].zip
```

Take `emqx-ee-4.4.16-otp24.3.4.2-1-ubuntu20.04-amd64.zip` for example:

- EMQX Type(`ee`): EMQX Type. `ee` stands for EMQX Enterprise, the package of the open source version does not have this field. You cannot upgrade the open source EMQX using a EMQX enterprise package and vice versa.
- EMQX Version(`4.4.16`): EMQX version number. Please ensure that the running EMQX is on version 4.4.*, but not 4.3 or 5.0 etc.
- OTP Version(`otp24.3.4.2-1`): Erlang OTP version number. Please ensure that the first digit (here is 24) must be the same as the version of the running EMQX.
- OS Type(`ubuntu20.04`): OS type. Please ensure that it is same as the OS Type of the running EMQX.
- Arch(`amd64`): Type of the architecture. Please ensure that it is the same as the arch type of the running EMQX.
{% endemqxee %}

## Setups for Hot Upgrade

1. View the list of currently installed EMQX versions

```bash
$ emqx versions

Installed versions:
* 4.4.0	permanent
```

2. Check whether the downloaded package type is the same as the currently installed one.
   Please see the "Download the Hot Upgrade Zip Package" section above.

3. Locate the EMQX installation directory:

```bash
$ EMQX_ROOT_DIR=$(emqx root_dir)

$ echo ${EMQX_ROOT_DIR}
"/usr/lib/emqx"
```

4. Place the downloaded zip package in the `releases` directory under the EMQX installation directory:

```bash
$ cp emqx-*4.4.16-*.zip ${EMQX_ROOT_DIR}/releases/
```

5. Upgrade to the specified version:

```bash
$ emqx upgrade 4.4.16

Release 4.4.16 not found, attempting to unpack releases/emqx-4.4.16.tar.gz
Unpacked successfully: "4.4.16"
Installed Release: 4.4.16
Made release permanent: "4.4.16"
```

6. View the version list again, and the status of the previous version will change to `old`:

```bash
$ emqx versions

Installed versions:
* 4.4.16 permanent
* 4.4.0	old
```

::: warning
Don't remove any files or directories under the `${EMQX_ROOT_DIR}/releases/`.
If you want to remove the specified version, use the `emqx uninstall` command.
:::

## Permanent the Version Manually after Upgrading

The `emqx upgrade 4.4.16` command above actually performs three actions:

- Uncompress the Zip package (`unpack`)
- Install it (`install`)
- Permanent it (`permanent`)

After persistence, if EMQX restarts, the new version will be used.
If you do not want to persist while upgrading, you can use the `--no-permanent` parameter:

```bash
$ emqx upgrade --no-permanent 4.4.16

Release 4.4.16 not found, attempting to unpack releases/emqx-4.4.16.tar.gz
Unpacked successfully: "4.4.16"
Installed Release: 4.4.16
```

Now the version has been successfully upgraded to 4.4.16, but it will revert to 4.4.0 after the restart.
Now if you lookup the versions, you will find that the status of 4.4.16 is "current", not the persistent version:

```bash
$ emqx versions

Installed versions:
* 4.4.16	current
* 4.4.0	permanent
```

After the system has been running stably for a period of time, if you decide to persist the new version, you can run the `install` command again:

```bash
$ emqx install 4.4.16

Release 4.4.16 is already installed and current, making permanent.
Made release permanent: "4.4.16"
```

## Downgrade to a Version

If you find a problem after upgrading and want to go back, you can run the version downgrade command.
For example, the following example will roll back EMQX to version 4.4.0:

```bash
$ emqx downgrade 4.4.0

Release 4.4.0 is marked old, switching to it.
Installed Release: 4.4.0
Made release permanent: "4.4.0"
```

## Uninstall a Version

If you decide to delete an old version after the system has been running stably for a period of time, you can do the version uninstallation.
For example, the following example will uninstall the old version of 4.4.0:

```bash
$ emqx uninstall 4.4.0

Release 4.4.0 is marked old, uninstalling it.
Uninstalled Release: 4.4.0
```
