# Configure Pod Disruption Budgets

EMQX Operator 3.0 does not create Kubernetes PodDisruptionBudgets (PDBs) for EMQX clusters. This page shows you how to create separate PDBs for Core and Replicant Pods to limit voluntary disruptions caused by operations such as node drains, cluster autoscaler scale-downs, or descheduler evictions.

A PDB limits evictions requested through the Kubernetes Eviction API. It does not prevent involuntary failures, direct Pod deletion, or Pod replacement managed by EMQX Operator during a rolling update.

## Before You Begin

This example assumes that your current `kubectl` namespace contains an EMQX resource named `emqx`. If your resource has a different name, replace `emqx` in the `apps.emqx.io/instance` selectors and EMQX resource commands with its `metadata.name`. Create each PDB in the same namespace as its EMQX resource. The PDB resource names `emqx-core` and `emqx-replicant` are examples. If you change them, use the new names in the PDB verification commands.

EMQX Operator publishes the current Pod selectors in the EMQX status:

- `status.coreSelector`
- `status.replicantSelector`

For an EMQX cluster named `emqx`, these selectors use the following stable labels:

```yaml
apps.emqx.io/instance: emqx # Use the corresponding EMQX CR metadata.name.
apps.emqx.io/managed-by: emqx-operator
apps.emqx.io/db-role: core # Use replicant for Replicant Pods.
```

Choose `minAvailable` or `maxUnavailable` based on the number of Pods in each role and your availability requirements. For example, `maxUnavailable: 1` does not preserve availability for a role with only one Pod. Use `minAvailable: 1` if voluntary disruptions must not remove that Pod.

## Create PodDisruptionBudgets

1. Save the following resources as `emqx-pdb.yaml`.

   This example defines two PDBs to protect Core and Replicant Pods as separate availability pools. If one PDB matched both roles, an eviction in one role could consume disruption capacity needed by the other. If your cluster does not use Replicant Pods, omit the `emqx-replicant` PDB.

   ```yaml
   apiVersion: policy/v1
   kind: PodDisruptionBudget
   metadata:
     name: emqx-core
   spec:
     maxUnavailable: 1
     selector:
       matchLabels:
         apps.emqx.io/instance: emqx
         apps.emqx.io/managed-by: emqx-operator
         apps.emqx.io/db-role: core
   ---
   apiVersion: policy/v1
   kind: PodDisruptionBudget
   metadata:
     name: emqx-replicant
   spec:
     maxUnavailable: 1
     selector:
       matchLabels:
         apps.emqx.io/instance: emqx
         apps.emqx.io/managed-by: emqx-operator
         apps.emqx.io/db-role: replicant
   ```

2. Apply the PDBs:

   ```bash
   kubectl apply -f emqx-pdb.yaml
   ```

## Verify the PodDisruptionBudgets

1. Check the selectors reported by EMQX Operator:

   ```bash
   kubectl get emqx emqx -o jsonpath='{.status.coreSelector}{"\n"}{.status.replicantSelector}{"\n"}'
   ```

2. Verify that each selector matches the expected Pods. If your cluster does not use Replicant Pods, omit the second command.

   ```bash
   kubectl get pods -l 'apps.emqx.io/instance=emqx,apps.emqx.io/managed-by=emqx-operator,apps.emqx.io/db-role=core'
   kubectl get pods -l 'apps.emqx.io/instance=emqx,apps.emqx.io/managed-by=emqx-operator,apps.emqx.io/db-role=replicant'
   ```

3. Check each PDB and review its allowed disruptions. If your cluster does not use Replicant Pods, omit `emqx-replicant` and its `describe` command.

   ```bash
   kubectl get pdb emqx-core emqx-replicant
   kubectl describe pdb emqx-core
   kubectl describe pdb emqx-replicant
   ```

   A restrictive PDB can block node drains indefinitely. Check each PDB's `status.disruptionsAllowed` value and ensure that enough matching Pods are ready before starting maintenance.
