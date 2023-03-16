
## Release Upgrade

## Release Hot Upgrade

EMQX support release hot upgrade since version 4.2.0.

With the release hot upgrade feature, users can quickly and safely upgrade the EMQX of the production environment while keeping all the clients connected, avoid reducing the system availability caused by restarting the service.

::: tip
Currently EMQX for Windows and MacOS does not support the release hot upgrade feature.
:::

::: warning
EMQX only allows release hot upgrade between patch versions (the third digit of the version number). Please operate with caution.
That is, upgrading from x.y.z to x.y.(z+N) is allowed, but upgrading from x.y to (x+N).(y+M) is not allowed.
e.g. It is OK to upgrade to 4.4.16 from 4.4.1, 4.4.2, ..., but not from 4.3.1, 4.3.2, ...
:::

## Download the EMQX Zip Package for Release Hot Upgrade

{% emqxce %}
Go to [Download EMQX](https://www.emqx.com/en/downloads?product=broker) for the version and the OS you want, and then select the **"zip"** in the "Install Method".

The Zip package name format of EMQX version 4.4 is:

```
emqx-[EMQX Version]-[OTP Version]-[OS Type]-[Arch].zip
```

Take `emqx-4.4.16-otp24.3.4.2-1-ubuntu20.04-amd64.zip` for example:

- EMQX Version(`4.4.16`): EMQX version number. Please ensure that all the digits except the last one (here is 16) in the version number must be the same as that of the installed EMQX.
- OTP Version(`otp24.3.4.2-1`): Erlang OTP version number. Please ensure that the first digit (here is 24) must be the same as that of the installed EMQX.
- OS Type(`ubuntu20.04`): OS type. Please ensure that it is same as the OS Type of the installed EMQX.
- Arch(`amd64`): Type of the architecture. Please ensure that it is the same as the arch type of the installed EMQX.
{% endemqxce %}

{% emqxee %}
Go to [EMQX Enterprise](https://www.emqx.com/en/try?product=enterprise) for the version and the OS you want, and then select the **"zip"** in the "Install Method".

The Zip package name format of EMQX Enterprise version 4.4 is:

```
emqx-[EMQX Type]-[EMQX Version]-[OTP Version]-[OS Type]-[Arch].zip
```

Take `emqx-ee-4.4.16-otp24.3.4.2-1-ubuntu20.04-amd64.zip` for example:

- EMQX Type(`ee`): EMQX Type. `ee` stands for EMQX Enterprise, Open source version does not have this field. Please make it is the same as the installed EMQX Type.
- EMQX Version(`4.4.16`): EMQX version number. Please ensure that all the digits except the last one (here is 16) in the version number must be the same as that of the installed EMQX.
- OTP Version(`otp24.3.4.2-1`): Erlang OTP version number. Please ensure that the first digit (here is 24) must be the same as that of the installed EMQX.
- OS Type(`ubuntu20.04`): OS type. Please ensure that it is same as the OS Type of the installed EMQX.
- Arch(`amd64`): Type of the architecture. Please ensure that it is the same as the arch type of the installed EMQX.
{% endemqxee %}

## Setups for Release Hot Upgrade

1. View the list of currently installed EMQX versions

```bash
$ emqx versions

Installed versions:
* 4.4.0	permanent
```

2. Check whether the downloaded package type is the same as the currently installed one.

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

After persistence, this version upgrade will be fixed, which means that after a hot upgrade, if EMQX restarts, the new version after the upgrade will be used.
If you do not want to persist while upgrading, you can use the `--no-permanent` parameter:

```bash
$ emqx upgrade --no-permanent 4.4.16

Release 4.4.16 not found, attempting to unpack releases/emqx-4.4.16.tar.gz
Unpacked successfully: "4.4.16"
Installed Release: 4.4.16
```

At this time, the version has been successfully upgraded to 4.4.16, but if you restart EMQX, it will revert to the old version 4.4.0.
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

## Downgrade to a version

If you find a problem after upgrading and want to go back, you can run the version downgrade command.
For example, the following example will roll back EMQX to version 4.4.0:

```bash
$ emqx downgrade 4.4.0

Release 4.4.0 is marked old, switching to it.
Installed Release: 4.4.0
Made release permanent: "4.4.0"
```

## Uninstall a version

If you decide to delete an old version after the system has been running stably for a period of time, you can do the version uninstallation.
For example, the following example will uninstall the old version of 4.4.0:

```bash
$ emqx uninstall 4.4.0

Release 4.4.0 is marked old, uninstalling it.
Uninstalled Release: 4.4.0
```
