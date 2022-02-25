# Patch EMQX at Runtime

If a bug fix only updates a few modules, you can use patch to upgrade emqx if you know what modules to be updated.

Note: If you can use release upgrade, then release upgrade is preferred. You should apply patches only when release upgrade is not available, and you're sure you know what you are doing.

## Steps of Patching

1. Get the list of modules to be updated from emqx developers. For example:

    ```
    emqx.beam
    emqx_rule_engine.beam
    ```

2. Get the corresponding software package from emqx official website or emqx developer.

Visit [open source download address](https://www.emqx.com/en/try?product=broker) or [enterprise download address](https://www.emqx.com/en/try?product=enterprise) to download the corresponding version of the zip package.

Pay attention to selecting the correct software version number, OTP version number and operating system type, and select **zip** package type.

3. Unzip the downloaded zip package and find the modules to be updated:

    ```bash
    $ unzip -q emqx-ee-4.4.1-otp24.1.5-3-ubuntu20.04-amd64.zip
    ```

    Suppose we want to update the `emqx.beam` and `emqx_rule_engine.beam`, then find them in the extracted directory:

    ```bash
    $ find ./emqx -name "emqx.beam"
    ./emqx/lib/emqx-4.4.1/ebin/emqx.beam

    $ find ./emqx -name "emqx_rule_engine.beam"
    ./emqx/lib/emqx_rule_engine-4.4.1/ebin/emqx_rule_engine.beam
    ```

4. Ensure that emqx is started:

    ```bash
    $ emqx_ctl status
    Node 'emqx@127.0.0.1' 4.4.1 is started
    ```

5. Find the corresponding location of beam file, backup and replace the old beams:

    Locate the emqx installation directory:

    ```bash
    $ emqx root_dir
    "/usr/lib/emqx"
    ```

    Find the path of the old beam file in the `lib` directory under the installation directory:

    ```bash
    $ find /usr/lib/emqx/lib -name "emqx.beam"
    /usr/lib/emqx/lib/emqx-4.4.0/ebin/emqx.beam

    $ find /usr/lib/emqx/lib -name "emqx_rule_engine.beam"
    /usr/lib/emqx/lib/emqx_rule_engine-4.4.0/ebin/emqx_rule_engine.beam
    ```

    Back up the old beam file to the `/tmp` Directory:

    ```bash
    $ cp /usr/lib/emqx/lib/emqx-4.4.0/ebin/emqx.beam \
        /usr/lib/emqx/lib/emqx_rule_engine-4.4.0/ebin/emqx_rule_engine.beam /tmp
    ```

    Overwrite the corresponding files with the new beam file:

    ```bash
    $ cp -f ./emqx/lib/emqx-4.4.1/ebin/emqx.beam /usr/lib/emqx/lib/emqx-4.4.0/ebin/
    $ cp -f ./emqx/lib/emqx_rule_engine-4.4.1/ebin/emqx_rule_engine.beam /usr/lib/emqx/lib/emqx_rule_engine-4.4.0/ebin/
    ```

6. Load new files at runtime:

    ```bash
    $ emqx eval 'c:lm().'
    [{module, emqx},
     {module, emqx_rule_engine}]
    ```

## Rollback the Patches

1. Copy the backup files back to the original directory:

    ```bash
    $ cp -f /tmp/emqx.beam /usr/lib/emqx/lib/emqx-4.4.0/ebin/
    $ cp -f /tmp/emqx_rule_engine.beam /usr/lib/emqx/lib/emqx_rule_engine-4.4.0/ebin/
    ```
2. reload the beam files:

    ```bash
    $ emqx eval 'c:lm().'
    [{module, emqx},
     {module, emqx_rule_engine}]
    ```