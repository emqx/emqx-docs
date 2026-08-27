# Change EMQX Configuration

## Objective

Change EMQX configuration through `.spec.config.roots` in the EMQX Custom Resource.

## Configure EMQX Cluster

The `apps.emqx.io/v3beta1` EMQX CRD accepts top-level EMQX configuration roots as JSON-compatible values in `.spec.config.roots`. In a YAML manifest, express each root as a structured YAML object, array, or scalar that corresponds to the [EMQX configuration schema](https://docs.emqx.com/en/enterprise/v6.2.0/hocon/).

The field does not accept HOCON-only constructs such as includes or substitutions.

Removing a root from `.spec.config.roots` means that EMQX Operator stops managing that root. It does not remove values that EMQX has persisted or restore the root to its schema defaults. To reset a root to known values, declare those values explicitly.

1. Save the following as a YAML file and deploy it using `kubectl apply`:

   ```yaml
   apiVersion: apps.emqx.io/v3beta1
   kind: EMQX
   metadata:
     name: emqx
   spec:
     image: emqx/emqx:@EE_VERSION@
     imagePullPolicy: IfNotPresent
     config:
       roots:
         # Configure a TCP listener named `test` on port 1884:
         listeners:
           tcp:
             test:
               bind: "0.0.0.0:1884"
               max_connections: 1024000
         license:
           key: "..."
     listenersServiceTemplate:
       spec:
         type: LoadBalancer
     dashboardServiceTemplate:
       spec:
         type: LoadBalancer
   ```

   ::: tip
   Do not configure `node.cookie`, because EMQX Operator manages this setting.
   :::

   ::: tip
   EMQX Operator writes most settings, such as listener settings, to [`base.hocon`](../../../../configuration/configuration.md#base-configuration-file) and applies changes at runtime through the EMQX Configs API without restarting Pods. It writes settings that take effect only when EMQX starts, such as Dashboard listeners and node settings, to [`emqx.conf`](../../../../configuration/configuration.md#immutable-configuration-file). Changing such settings triggers a controlled rolling update.
   :::

2. Wait for the EMQX cluster to become ready. Check the status of the EMQX cluster using `kubectl get`, and make sure that `STATUS` is `Ready`. This may take some time.

   ```bash
   $ kubectl get emqx emqx
   NAME   STATUS   AGE
   emqx   Ready    10m
   ```

3. Check the `ConfigApplied` condition to confirm that the desired configuration is active:

   ```bash
   $ kubectl get emqx emqx -o jsonpath='{range .status.conditions[?(@.type=="ConfigApplied")]}{.status}{"\t"}{.reason}{"\t"}{.message}{"\n"}{end}'
   True    Applied    Desired configuration is active
   ```

## Verify Configuration

View the EMQX listeners' status.

```bash
$ kubectl exec -it emqx-core-0 -c emqx -- emqx ctl listeners
tcp:default
   listen_on: 0.0.0.0:1883
   acceptors: 16
   proxy_protocol : false
   running: true
   current_conn: 0
   max_conns : 1024000
tcp:test
   listen_on: 0.0.0.0:1884
   acceptors: 16
   proxy_protocol : false
   running: true
   current_conn: 0
   max_conns : 1024000
```

Here we can see that the new listener on port 1884 is running.

## Change Configuration That Requires a Restart

Some configuration changes update the Pod template and trigger a rolling update. The following example changes the Dashboard HTTP listener, which takes effect when EMQX starts.

1. Patch the EMQX resource:

   ```bash
   kubectl patch emqx emqx --type=merge -p '{"spec":{"config":{"roots":{"dashboard":{"listeners":{"http":{"bind":"0.0.0.0:18084"}}}}}}}'
   ```

2. Check the `ConfigApplied` condition after EMQX Operator detects the change:

   ```bash
   $ kubectl get emqx emqx -o jsonpath='{range .status.conditions[?(@.type=="ConfigApplied")]}{.status}{"\t"}{.reason}{"\t"}{.message}{"\n"}{end}'
   False    StartupConfigPending    Configuration roots require rolling restart: [dashboard]
   ```

   `False` with reason `StartupConfigPending` means that at least one ready Pod still uses the previous configuration and the rolling update is in progress.

3. Wait until the rolling update completes:

   ```bash
   kubectl wait --for=condition=ConfigApplied emqx/emqx --timeout=10m
   ```

4. Check the `ConfigApplied` condition to confirm that the new configuration is active:

   ```bash
   $ kubectl get emqx emqx -o jsonpath='{range .status.conditions[?(@.type=="ConfigApplied")]}{.status}{"\t"}{.reason}{"\t"}{.message}{"\n"}{end}'
   True    Applied    Desired configuration is active
   ```
