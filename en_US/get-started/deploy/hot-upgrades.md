# Hot Upgrade

Hot upgrade lets you apply a patch-version update to a running EMQX Enterprise node without stopping it. Connections stay alive throughout the upgrade; the node switches to the new release tree on its next restart.

Hot upgrade is supported for patch-version hops only (the third digit of the version number). For example, upgrading from 5.10.4 to 5.10.5 is supported, but upgrading from 5.10.x to 5.11.0 is not.

Hot upgrade is driven by the `emqx_relup` plugin. The plugin is installed from the Dashboard. All upgrade operations are performed through the `emqx ctl relup` CLI.

::: warning Important Notice
Back up `data/`, `etc/`, and `log/` before running a hot upgrade. There is no in-place rollback once a hop is applied.
:::

## Prerequisites

- EMQX Enterprise is running on each node.
- You have shell access and can run `emqx ctl` commands on each node.
- The `emqx_relup` plugin version matches your current EMQX Enterprise version.
- The target version is listed as a supported hop by the plugin (verify in step 2).

## Step 1: Install the emqx_relup Plugin

Install the plugin from the Dashboard following the [Install Packages via Dashboard](../../guides/extensions/plugin-management.md#install-packages-via-dashboard) instructions. Plugin packages are published at `https://packages.emqx.io/emqx-plugins/e<EMQX-VSN>/`.

The plugin should appear with status `running` after installation.

## Step 2: Confirm the Upgrade Path Is Supported

List the version hops the installed plugin knows about:

```bash
emqx ctl relup list-supported-paths
```

Confirm that your `{from, target}` version pair appears in the output before continuing.

## Step 3: Prepare the Tarball and SHA256 Sidecar

Copy two files to each node. They can be placed anywhere readable by the EMQX process:

- `emqx-enterprise-<TargetVsn>-<os>-<arch>.tar.gz`: The EMQX Enterprise target release tarball.
- `emqx-enterprise-<TargetVsn>-<os>-<arch>.tar.gz.sha256`: The SHA256 digest of the tarball. Both the bare digest format and the `sha256sum` output format (`<digest>  <filename>`) are accepted.

```bash
scp emqx-enterprise-<TargetVsn>-<os>-<arch>.tar.gz      node1:/opt/upgrade/
scp emqx-enterprise-<TargetVsn>-<os>-<arch>.tar.gz.sha256 node1:/opt/upgrade/
```

The `.sha256` sidecar must sit next to the tarball and share the same base name with a `.sha256` extension.

## Step 4: Run the Upgrade

On each node, trigger the upgrade by pointing `relup upgrade` at the tarball:

```bash
emqx ctl relup upgrade /opt/upgrade/emqx-enterprise-<TargetVsn>-<os>-<arch>.tar.gz
```

The upgrade handler performs the following steps:

1. Verifies the SHA256 digest and refuses to proceed if there is a mismatch.
2. Extracts the tarball and reads the target version from `releases/emqx_vars`.
3. Deploys only the runtime subdirectories (`bin/`, `erts-*/`, `lib/`, `releases/`) to `<RootDir>/relup/<TargetVsn>/`. Config, data, logs, and plugins remain at the original install root.
4. Applies the matching code-change instructions from the plugin's `.relup` catalog.
5. Runs post-upgrade callbacks.
6. Writes `<RootDir>/relup/current` with the target version string.

Rolling out across a cluster is the operator's responsibility. Upgrade nodes one at a time and verify each before proceeding.

## Step 5: Verify the Node

After the upgrade command returns, confirm the node is healthy:

```bash
# Check the node is running
emqx ctl status

# Confirm the version marker was written
cat <RootDir>/relup/current

# Check upgrade status and history
emqx ctl relup status
emqx ctl relup logs
```

`relup/current` should contain the target version string, and `emqx ctl status` should report the node running.

## Step 6: Restart Into the New Release

The code-change upgrade takes effect in the running VM immediately. To fully switch to the new ERTS runtime and binaries, restart the node:

```bash
emqx restart
```

On restart, the `bin/emqx` wrapper detects `<RootDir>/relup/current` and starts from `<RootDir>/relup/<TargetVsn>/` instead. The original `<RootDir>` remains the authority for `data/`, `etc/`, `log/`, and `plugins/`.

## Step 7: Clean Up

Once the entire cluster is running the target version, remove the staging files:

```bash
rm /opt/upgrade/emqx-enterprise-<TargetVsn>-<os>-<arch>.tar.gz
rm /opt/upgrade/emqx-enterprise-<TargetVsn>-<os>-<arch>.tar.gz.sha256
```

The plugin does not track the source path, so no cleanup is needed inside the plugin itself.

## Rollback

There is no in-place rollback for an applied hop. Code changes run against the live VM, and post-upgrade callbacks may have mutated data on disk; reversing those is not supported.

Two limited recovery options are available:

- **Before the next restart:** If the upgraded code is misbehaving but disk state is still compatible with the old release, delete the version marker and restart into the old tree:

  ```bash
  rm <RootDir>/relup/current
  # optionally: rm -rf <RootDir>/relup/<TargetVsn>/
  emqx restart
  ```

  This recovers only the boot path. Live state changes made by the upgrade are already in effect.

- **Full restore:** Restore `data/` from the pre-upgrade backup and reinstall the old EMQX release.

Plan your upgrade window with these limitations in mind.

## CLI Reference

| Command | Description |
|---|---|
| `emqx ctl relup upgrade <TarballPath>` | Apply the upgrade hop from the given tarball. |
| `emqx ctl relup list-supported-paths` | List supported `{from, target}` version hops in the plugin catalog. |
| `emqx ctl relup status` | Show the current upgrade state: `idle`, `in-progress`, or `hot-upgraded to <vsn>; pending on restart to boot from the new version`. |
| `emqx ctl relup logs` | Print this node's upgrade history from the persistent log table. |
| `emqx ctl relup logs-clear` | Wipe this node's upgrade log table. |
