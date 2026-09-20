# Configure Pod Disruption Budgets

## Objective

Create separate Kubernetes PodDisruptionBudgets (PDBs) for EMQX Core and Replicant Pods.

## Background

EMQX Operator 3.0 does not create PDBs for EMQX clusters. Create them separately when you need to limit voluntary disruptions caused by operations such as node drains, cluster autoscaler scale-downs, or descheduler evictions.

Note that a PDB limits evictions requested through the Kubernetes Eviction API. It does not prevent involuntary failures, direct Pod deletion, or Pod replacement managed by EMQX Operator during a rolling update.

EMQX Operator publishes the current Pod selectors in the EMQX status:

- `status.coreSelector`
- `status.replicantSelector`

For an EMQX cluster named `emqx`, these selectors use the following stable labels:

```yaml
apps.emqx.io/instance: emqx # Use corresponding EMQX CR metadata.name
apps.emqx.io/managed-by: emqx-operator
apps.emqx.io/db-role: core # Use replicant for Replicant Pods.
```

## Create PodDisruptionBudgets

1. Save the following resources as `emqx-pdb.yaml`. Create each PDB in the same namespace as its EMQX resource.

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

   This example defines two PDBs to protect Core and Replicant Pods as separate availability pools. If one PDB matched both roles, an eviction in one role could consume disruption capacity needed by the other.

   ::: tip
   Choose `minAvailable` or `maxUnavailable` based on the number of Pods in each role and your availability requirements. For example, `maxUnavailable: 1` does not preserve availability for a role with only one Pod. Use `minAvailable: 1` if voluntary disruptions must not remove that Pod.
   :::

2. Apply the PDBs:

   ```bash
   kubectl apply -f emqx-pdb.yaml
   ```

## Verify the PodDisruptionBudgets

1. Check the selectors reported by EMQX Operator:

   ```bash
   kubectl get emqx emqx -o jsonpath='{.status.coreSelector}{"\n"}{.status.replicantSelector}{"\n"}'
   ```

2. Check that each PDB matches the expected Pods and review its allowed disruptions:

   ```bash
   kubectl get pdb emqx-core emqx-replicant
   kubectl describe pdb emqx-core
   kubectl describe pdb emqx-replicant
   ```

   A restrictive PDB can block node drains indefinitely. Check each PDB's `status.disruptionsAllowed` value and ensure that enough matching Pods are ready before starting maintenance.
