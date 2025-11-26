# Hot Patch

Starting from 4.3, EMQX supports an easier (compared to 4.2) hot-patch mechanism.
Hot-patch is an ad-hoc patch mechanism compared to [Hot Upgrade](./relup.md#hot-upgrade-steps).
For certain urgent issues, a hot patch can be applied before the fix is included in a (future) official release.

::: tip NOTE
It's recommended to upgrade to the official release which included the patched fixes.
The patched modules should be deleted before upgrading.
:::

## Steps to Patch

1. Get the list of modules to be updated from the EMQX support team. For example:

```
emqx.beam
emqx_rule_engine.beam
```

2. Copy the files to `data/patches` directory

```bash
cp /path/to/patch/emqx.beam data/patches
cp /path/to/patch/emqx_rule_engine.beam data/patches
```

The exact path to the `data/patches` directory depends on configuration and installation.
Typically, it is:

* Where the environment variable `EMQX_NODE__DATA_DIR` points to
* Where the `node.data_dir` config key points to in `emqx.conf`
* `/opt/emqx/data` when running in docker (typically a mounted volume)
* `<install-path>/data` when installed from zip package extraction
* `/var/lib/emqx/` when installed from RPM or DEB packages

3. Load new files at runtime:

```bash
$ emqx eval 'c:l(emqx).'
{module,emqx}

$ emqx eval 'c:l(emqx_rule_engine).'
{module,emqx_rule_engine}
```

## Rollback the Patched Modules

In case the patch does not work as expected, or you want to go back to the state before patching, below are the steps to roll back.

1. Delete the patched modules from the `data/patches` directory

```bash
$ mv data/patches/emqx.beam /tmp/
$ mv data/patches/emqx_rule_engine.beam /tmp/
```

2. reload the beam files:

```bash
$ emqx eval 'c:l(emqx).'
{module,emqx}

$ emqx eval 'c:l(emqx_rule_engine).'
{module,emqx_rule_engine}
```
