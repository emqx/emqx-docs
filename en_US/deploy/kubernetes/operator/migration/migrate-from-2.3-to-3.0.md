# Migrate from EMQX Operator 2.3 to 3.0

Operator 3.0 uses the `apps.emqx.io/v3beta1` API. It cannot convert an earlier EMQX custom resource or adopt the workloads created by Operator 2.3. This migration therefore deploys a new EMQX cluster instead of updating the existing cluster in place. It does not join the old and new clusters.

::: warning

This walkthrough does not migrate MQTT sessions. Plan a maintenance window and make sure clients can reconnect to the new cluster. Live connections, session state, offline queues, in-flight messages, and Durable Storage data are not included in an EMQX data backup.

:::

The walkthrough keeps the old workloads available for rollback while you install Operator 3.0 and verify a separately named EMQX cluster. After verification, switch client traffic to the new cluster and retire the old workloads.

The examples use the following names. Replace them with the names and namespace of your deployment:

```bash
export EMQX_NAMESPACE=default
export OLD_EMQX=my-emqx
export NEW_EMQX=my-emqx-v3
```

## 1. Prepare for the migration

Before starting:

- Rehearse the complete procedure in an environment that matches production.
- Keep the EMQX image and version unchanged. Upgrade EMQX only after the new cluster is running under Operator 3.0.
- Make sure the Kubernetes cluster has enough capacity to run the old and new EMQX workloads at the same time.
- Plan how to direct clients from the old listener Service to the new one, for example through a load balancer, ingress, or DNS change.
- Stop configuration and application data changes before taking the final backup. Changes made afterward are not copied to the new cluster.
- Confirm that Durable Storage is not in use. If it is, proceed only if losing its data is acceptable because this walkthrough does not migrate it.

The EMQX CRD is cluster-scoped. List every EMQX resource before removing the Operator 2.3 CRD:

```bash
kubectl get emqx.apps.emqx.io --all-namespaces
```

All listed resources must be included in the same maintenance event. Do not remove the CRD while another Operator 2.3 cluster still depends on it.

## 2. Back up the existing cluster

Save the existing custom resource and a list of its workloads. Keep these files outside the Kubernetes cluster so that you can use them for rollback:

```bash
kubectl get emqx.apps.emqx.io "$OLD_EMQX" \
  -n "$EMQX_NAMESPACE" -o yaml > emqx-v2.yaml

kubectl get statefulset,replicaset,pod,service,pvc \
  -n "$EMQX_NAMESPACE" \
  -l "apps.emqx.io/instance=$OLD_EMQX" -o yaml \
  > emqx-v2-workloads.yaml
```

Select a running Core Pod and create a global EMQX data backup in its `/tmp` directory:

```bash
OLD_EMQX_CORE_POD="$(kubectl get pod \
  -n "$EMQX_NAMESPACE" \
  -l "apps.emqx.io/instance=$OLD_EMQX,apps.emqx.io/db-role=core" \
  --field-selector=status.phase=Running \
  -o jsonpath='{.items[0].metadata.name}')"

kubectl exec -n "$EMQX_NAMESPACE" "$OLD_EMQX_CORE_POD" -c emqx -- \
  emqx ctl data export --dir /tmp
```

The export command prints the generated archive path. Set `EMQX_BACKUP_FILE` to the file name from that path, then copy the archive out of the Pod and verify it:

```bash
export EMQX_BACKUP_FILE='<exported-file-name>.tar.gz'

kubectl cp -c emqx \
  "$EMQX_NAMESPACE/$OLD_EMQX_CORE_POD:/tmp/$EMQX_BACKUP_FILE" \
  "./$EMQX_BACKUP_FILE"

chmod 600 "./$EMQX_BACKUP_FILE"
test -s "./$EMQX_BACKUP_FILE"
tar -tzf "./$EMQX_BACKUP_FILE" >/dev/null
```

Alternatively, create and download a global backup from **System** -> **Backup & Restore** in Dashboard. For details, see [Backup and Restore](../../../../operations/backup-restore.md). Do not use the Operator-generated API key for this backup: API-key-authenticated exports omit Dashboard users and API keys.

The backup contains supported configuration, files from the EMQX data directory, and built-in database data such as authentication records, API keys, and retained messages. It does not contain the live MQTT state listed in the warning above. A good idea is to verify the archive by restoring it in a test environment before continuing.

## 3. Convert the EMQX manifest

Create a new manifest named `emqx-v3.yaml`. Use a different `metadata.name`, such as the value of `$NEW_EMQX`, so that Operator 3.0 does not mistake the orphaned Operator 2.3 workloads for its own resources.

Apply these changes to the manifest:

| Operator 2.3 setting | Change in Operator 3.0 | Action Needed |
| --- | --- | --- |
| `apiVersion: apps.emqx.io/v2` | Replaced by `apps.emqx.io/v3beta1` | Change `apiVersion` in the manifest. |
| `.spec.config.data` | Replaced by `.spec.config.roots` | Convert literal HOCON to structured YAML. |
| `.spec.coreTemplate.spec.volumeClaimTemplates` | Renamed to `.spec.coreTemplate.spec.persistentVolumeClaimSpec` | Rename the field and preserve its storage settings. |
| `.spec.coreTemplate.spec.replicas`<br/>`.spec.replicantTemplate.spec.replicas` | Default changed from `2` to `1` | Set the intended counts explicitly. When Replicants are enabled, configure at least two Core replicas. |
| `.spec.bootstrapAPIKeys` | Removed | Restore existing API keys from the global backup, then manage them through EMQX. |
| `.spec.updateStrategy.initialDelaySeconds` | Removed; no direct replacement | Remove the field and review rollout timing. Do not map it to `minReadySeconds`, which has different semantics. |
| `.spec.updateStrategy.evacuationStrategy.connEvictRate` | Renamed to `.spec.updateStrategy.evacuationStrategy.connectionEvictionRate` | Rename the field and preserve the value. |
| `.spec.updateStrategy.evacuationStrategy.sessEvictRate` | Renamed to `.spec.updateStrategy.evacuationStrategy.sessionEvictionRate` | Rename the field and preserve the value. |
| `.spec.coreTemplate.spec.minAvailable`<br/>`.spec.coreTemplate.spec.maxUnavailable`<br/>`.spec.replicantTemplate.spec.minAvailable`<br/>`.spec.replicantTemplate.spec.maxUnavailable` | Removed | Create separate PDBs after the migration; see [Configure Pod Disruption Budgets](../tasks/configure-disruption-budgets.md). |
| `Rebalance` resources | Removed | Delete existing `Rebalance` resources before removing the Operator 2.3 CRD. |

For example, convert this Operator 2.3 configuration:

```yaml
spec:
  config:
    mode: Merge
    data: |
      log.console.level = warning
      dashboard.listeners.http.bind = 18083
```

To this Operator 3.0 structure:

```yaml
spec:
  config:
    roots:
      log:
        console:
          level: warning
      dashboard:
        listeners:
          http:
            bind: 18083
```

For configuration details, see [Configure EMQX](../tasks/configure-emqx-config.md).

Before deploying the new cluster, copy the old Operator bootstrap API-key Secret to the name expected by the new EMQX resource:

```bash
kubectl get secret "$OLD_EMQX-bootstrap-api-key" \
  -n "$EMQX_NAMESPACE" -o jsonpath='{.data.bootstrap_api_key}' \
  | base64 --decode \
  | kubectl create secret generic "$NEW_EMQX-bootstrap-api-key" \
      -n "$EMQX_NAMESPACE" \
      --from-file=bootstrap_api_key=/dev/stdin
```

The global backup contains the Operator controller API-key record. Reusing the bootstrap Secret keeps that credential consistent when the record is restored. Do not copy the old node-cookie Secret or configure `node.cookie`. Operator 3.0 creates a new cookie for the independent cluster used by this walkthrough.

## 4. Stop Operator 2.3 and preserve its workloads

Scale the Operator 2.3 controller to zero. Adjust the namespace and Deployment name if you used a custom installation:

```bash
kubectl scale deployment emqx-operator-controller-manager \
  -n emqx-operator-system --replicas=0
```

Delete each Operator 2.3 EMQX resource with orphan propagation. This removes the custom resource but leaves its StatefulSets, ReplicaSets, Pods, Services, and PVCs running:

```bash
kubectl delete emqx.apps.emqx.io "$OLD_EMQX" \
  -n "$EMQX_NAMESPACE" --cascade=orphan --wait=true
```

Verify that the old resources and client endpoint remain available:

```bash
kubectl get statefulset,replicaset,pod,service,pvc \
  -n "$EMQX_NAMESPACE" \
  -l "apps.emqx.io/instance=$OLD_EMQX"
```

Uninstall Operator 2.3 by using the same method that you used to install it, then remove its CRDs:

```bash
kubectl delete --ignore-not-found crd \
  emqxes.apps.emqx.io rebalances.apps.emqx.io
```

Install Operator 3.0 by following [Install Operator and Deploy EMQX](../getting-started.md), but do not deploy the example EMQX resource from that page.

## 5. Deploy and restore the new cluster

Validate and apply the converted manifest:

```bash
kubectl apply --dry-run=server -f emqx-v3.yaml
kubectl apply -f emqx-v3.yaml
```

Wait for both the workload and configuration to become ready:

```bash
kubectl wait emqx.apps.emqx.io/"$NEW_EMQX" \
  -n "$EMQX_NAMESPACE" --for=condition=Ready --timeout=15m

kubectl wait emqx.apps.emqx.io/"$NEW_EMQX" \
  -n "$EMQX_NAMESPACE" --for=condition=ConfigApplied --timeout=15m
```

Select a running Core Pod, copy the backup into it, and restore the data:

```bash
NEW_EMQX_CORE_POD="$(kubectl get pod \
  -n "$EMQX_NAMESPACE" \
  -l "apps.emqx.io/instance=$NEW_EMQX,apps.emqx.io/db-role=core" \
  --field-selector=status.phase=Running \
  -o jsonpath='{.items[0].metadata.name}')"

kubectl cp -c emqx \
  "./$EMQX_BACKUP_FILE" \
  "$EMQX_NAMESPACE/$NEW_EMQX_CORE_POD:/tmp/$EMQX_BACKUP_FILE"

kubectl exec -n "$EMQX_NAMESPACE" "$NEW_EMQX_CORE_POD" -c emqx -- \
  emqx ctl data import "/tmp/$EMQX_BACKUP_FILE"

kubectl exec -n "$EMQX_NAMESPACE" "$NEW_EMQX_CORE_POD" -c emqx -- \
  rm -f "/tmp/$EMQX_BACKUP_FILE"
```

Keep the converted `.spec.config.roots` as the source of truth for configuration. Run the readiness checks above again and verify that the import completed successfully before switching client traffic.

## 6. Switch client traffic

Run representative connection, authentication, publish, subscribe, retained message, rule, and integration tests against the new listener Service. Then update your load balancer, ingress, or DNS record to send new client connections to `<new-emqx-name>-listeners`.

Treat this cutover as the session boundary. Clients might disconnect and must reconnect to the new cluster. Verify client reconnect behavior and monitor authentication failures, reconnect loops, and message flow before continuing.

## 7. Complete the migration

After the acceptance period:

1. List the orphaned Operator 2.3 StatefulSets and ReplicaSets, then scale each one to zero.
2. Take a new EMQX data backup from the Operator 3.0 cluster.
3. Remove the orphaned Operator 2.3 workloads only after the rollback window has closed.

Do not delete the old PVCs or node-cookie Secret until you have verified the new cluster and stored both backups outside Kubernetes. Deleting these resources is irreversible.

## Roll back

Confirm that the old StatefulSets, ReplicaSets, PVCs, configuration resources, Services, and Secrets are still present.

If the old workloads are still running, test the old listener Service and direct client traffic back to it.

If you already scaled the old workloads to zero, use the following procedure to return to Operator 2.3 through a blue-green update:

1. Scale the same StatefulSets and ReplicaSets that were running before the migration back to their previous replica counts. Use `emqx-v2-workloads.yaml` to identify them and their counts.

2. Wait for EMQX to start in an old Core Pod:

   ```bash
   kubectl exec -n "$EMQX_NAMESPACE" <old-core-pod> -c emqx -- \
     emqx ctl status
   ```

   The recreated Pod can remain not ready because its readiness gate is managed by Operator 2.3. Do not switch client traffic yet.

3. Uninstall Operator 3.0 by using the same method that you used to install it. This also removes the new EMQX resource, its managed workloads, and the EMQX CRD. If you installed the CRD separately, remove it before continuing.

4. Reinstall the same Operator 2.3 version that managed the old cluster, then reapply the saved resource:

   ```bash
   kubectl apply -f emqx-v2.yaml
   ```

5. Wait for the restored resource to become ready, then test the old listener Service before switching traffic:

   ```bash
   kubectl wait emqx.apps.emqx.io/"$OLD_EMQX" \
     -n "$EMQX_NAMESPACE" --for=condition=Ready --timeout=15m
   ```

   Operator 2.3 does not strictly adopt the old workloads. It uses the running workloads as the starting revision for a blue-green update, performing a complete data migration.
