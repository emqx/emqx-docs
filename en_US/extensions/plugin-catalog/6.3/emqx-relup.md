# Hot Upgrade (Relup)

This plugin applies a `.relup` set of code-change instructions to a running EMQX node, so operators can roll out a patched release without restarting the VM.

Operators drive it through the `emqx ctl relup ...` CLI on each node; cluster-wide rollout is the operator's responsibility (no orchestration is built in).

## When to Use

A hot upgrade is appropriate when:

- The hop you want to apply is listed by `emqx ctl relup list-supported-paths` (only declared `{from, target}` hops are supported).
- You can verify the target node before moving on to the next.
- You have a backup of `data/`. There is no in-place rollback for an applied hop (see [Rollback](#rollback)).

If you cannot satisfy these, do a normal rolling restart instead.

## Operator Workflow

### 1. Install the plugin

Download the tarball matching your EMQX version from the [Download](#download) section below and install it from the Dashboard (or via the REST API / CLI like any other plugin).

### 2. Confirm the upgrade path is supported

```bash
emqx ctl relup list-supported-paths
```

The output lists the `{from, target}` hops bundled in `priv/relup/` of this plugin version. If your hop is missing, hot upgrade for that path is not available; fall back to a normal restart-based upgrade.

### 3. Stage the target release on every node

For each node, copy two files to a path the EMQX process can read:

- `emqx-enterprise-<TargetVsn>-<os>-<arch>.tar.gz`: The EMQX target release tarball.
- `<tarball>.sha256`: The sha256 digest. The standard `sha256sum` format (`<digest>  <filename>`) is accepted.

### 4. Trigger the upgrade

On each node:

```bash
emqx ctl relup upgrade <TarballPath> [--force]
```

The handler:

- Verifies `<TarballPath>.sha256` against the actual digest and refuses to extract on mismatch.
- Refuses to continue if `data/patches/` contains any `*.beam` files. This directory is prepended to the code path through `vm.args -pa`, so those files would take precedence over modules from the upgrade target. If the target release already includes the hot-patched fix, a stale beam file could still be loaded. Delete the patch files first, or pass `--force` only if you intend to keep them applied on top of the target release.
- Extracts the tarball and reads `REL_VSN` from `releases/emqx_vars`.
- Looks up the matching `{from, target}` hop in `priv/relup/*.relup` and runs the declared code-change instructions and post-upgrade callbacks.

### 5. Verify the node

Check these signals before moving on:

- `emqx ctl status` reports the node running.
- `cat <RootDir>/relup/current` matches the target version, and `<RootDir>/relup/<TargetVsn>/` contains `bin/`, `erts-*/`, `lib/`, `releases/`.

On the next `emqx start` / `restart`, the `bin/emqx` wrapper detects `relup/current` and execs into the deployed tree (new ERTS, new bin scripts, new lib). The original `<RootDir>` remains the authority for `data/`, `etc/`, `log/`, `plugins/`.

### 6. Clean up after success

Once the cluster is fully on the target version, manually remove the staged tarball and its `.sha256` sidecar. The plugin does not track the source path, so there is no plugin-side state to clean.

## Upgrade History

Each node keeps its own audit trail in the `emqx_relup_log` table (disc-backed, local content). The history survives plugin uninstall; reinstalling re-attaches and the rows are still there.

Inspect or clear via CLI:

```bash
emqx ctl relup logs           # show recent upgrade attempts
emqx ctl relup logs-clear     # delete all log rows on this node
```

## Rollback

There is no in-place rollback for an applied hop. The hot-upgrade runs `code_changes` against the live VM and any `post_upgrade_callbacks` may have mutated data on disk; reversing that is not supported by this plugin.

Practical fallback paths:

- **Before the next restart**, if the upgrade succeeded but the new code is misbehaving and the data on disk is still compatible with the old release:

  ```bash
  rm <RootDir>/relup/current
  # optional: rm -rf <RootDir>/relup/<TargetVsn>/
  emqx restart
  ```

  The wrapper falls back to the original `<RootDir>/bin/emqx` tree. This recovers only the boot path; live state inside the VM at the time of the bad upgrade is already gone.

- **Otherwise**, restore from the pre-upgrade backup of `data/` (mnesia, configs, etc.) and reinstall the old EMQX release. Plan the upgrade window with this in mind.

## Authoring a Hop (Developer Notes)

To add a new hop, for each release that needs one:

1. Add `priv/relup/<from>-to-<to>.relup` declaring the `code_changes` and `post_upgrade_callbacks` for the hop. See `priv/relup/README.md` in the plugin source for the schema, the supported instructions, and the post-upgrade callback contract. In particular, when adding a `pr_NNNNN_*` callback to the new EMQX's `emqx_post_upgrade`, the relup hop must reload that module before invoking the callback, or ship the callback in this plugin as `emqx_post_upgrade_<TargetVsn>.erl`.
2. Bump this plugin's `VERSION` and re-publish.

The plugin validates every `priv/relup/*.relup` at app start and logs a warning for malformed entries; bad files are skipped, not fatal.

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## Download

Tarballs for each EMQX release:

| EMQX Version | Plugin Version | Package |
|---|---|---|
| 6.3.0 | 1.0.2 | [emqx_relup-1.0.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.3.0/emqx_relup-1.0.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.3.0/emqx_relup-1.0.2.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
