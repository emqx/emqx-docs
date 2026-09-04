# Enable Persistence in EMQX Cluster

## Objective

Configure persistence for the set of Core nodes of an EMQX cluster through the `persistentVolumeClaimSpec` field.

## Configure EMQX Cluster Persistence

EMQX CRD `apps.emqx.io/v3beta1` supports configuring persistence of each Core node data through `.spec.coreTemplate.spec.persistentVolumeClaimSpec`.

The definition and semantics of the `.spec.coreTemplate.spec.persistentVolumeClaimSpec` field are consistent with those of `PersistentVolumeClaimSpec` defined in the Kubernetes API.

EMQX Operator 3.0 manages Core nodes with a single StatefulSet. Each Core Pod has a stable identity and a stable PVC across image updates and rolling upgrades. When you specify `.spec.coreTemplate.spec.persistentVolumeClaimSpec`, EMQX Operator configures the `/opt/emqx/data` volume of the EMQX container to be backed by a Persistent Volume Claim (PVC), which provisions a Persistent Volume (PV) using a specified [StorageClass](https://kubernetes.io/docs/concepts/storage/storage-classes/).

## PVC Lifecycle

Core node PVCs are tied to StatefulSet Pod ordinals. For example, the PVC for `emqx-core-0` stays attached to `emqx-core-0` during image updates and rolling updates, so the node keeps using the same data volume.

EMQX Operator configures Kubernetes to delete Core node PVCs when they are no longer needed:

- When you scale down Core nodes, PVCs for the removed Pod ordinals are deleted.
    
    For example, scaling from 5 Core replicas to 3 deletes the PVCs for ordinals 3 and 4. EMQX Operator ensures that this incurs no data or durability loss: any Durable Storage data is "rebalanced away" from those Core replicas before scaling the StatefulSet down.

- When you delete the EMQX custom resource, Kubernetes deletes the Core StatefulSet and its associated PVCs.

- During rolling updates, PVCs are preserved because the StatefulSet name and Pod ordinals do not change.

This automatic cleanup depends on the Kubernetes `StatefulSetAutoDeletePVC` feature gate. It is enabled by default in Kubernetes 1.32 and later. On Kubernetes 1.27 through 1.31, make sure the feature gate is enabled; otherwise Kubernetes ignores the deletion policy and you must clean up unused PVCs manually.

For more details about PVs and PVCs, refer to the [Persistent Volumes](https://kubernetes.io/docs/concepts/storage/persistent-volumes/) documentation.

1. Save the following content as a YAML file and deploy it using `kubectl apply`.

   ```yaml
   apiVersion: apps.emqx.io/v3beta1
   kind: EMQX
   metadata:
     name: emqx
   spec:
     image: emqx/emqx:@EE_VERSION@
     config:
       roots:
         license:
           key: "..."
     coreTemplate:
       spec:
         persistentVolumeClaimSpec:
           storageClassName: standard
           resources:
             requests:
               storage: 1Gi
           accessModes:
             - ReadWriteOnce
         replicas: 3
     listenersServiceTemplate:
       spec:
         type: LoadBalancer
     dashboardServiceTemplate:
       spec:
         type: LoadBalancer
   ```

   ::: tip

   Use the `storageClassName` field to choose the appropriate [StorageClass](https://kubernetes.io/docs/concepts/storage/storage-classes/) for EMQX data. Run `kubectl get storageclass` to list the StorageClasses that already exist in the Kubernetes cluster, or create a StorageClass according to your needs.

   :::

2. Wait for the EMQX cluster to become ready.

   Check the status of the EMQX cluster with `kubectl get` and ensure that `STATUS` is `Ready`. This may take some time.

   ```bash
   $ kubectl get emqx emqx
   NAME   STATUS   AGE
   emqx   Ready    10m
   ```

## Verify Persistence

Verify that Kubernetes reattaches the same PVC when a Core Pod is replaced. Do not delete the EMQX resource for this test: EMQX Operator configures its StatefulSet to delete associated PVCs when the StatefulSet is deleted.

1. Record the UID of the PVC attached to the first Core Pod:

   ```bash
   pvc_name=emqx-core-data-emqx-core-0
   pvc_uid_before=$(kubectl get pvc "${pvc_name}" -o jsonpath='{.metadata.uid}')
   kubectl get pvc "${pvc_name}"
   ```

2. Delete the Pod and wait for the StatefulSet to recreate it:

   ```bash
   kubectl delete pod emqx-core-0
   kubectl wait --for=condition=Ready pod/emqx-core-0 --timeout=10m
   ```

3. Compare the PVC UID after the Pod is ready:

   ```bash
   pvc_uid_after=$(kubectl get pvc "${pvc_name}" -o jsonpath='{.metadata.uid}')
   test "${pvc_uid_before}" = "${pvc_uid_after}" && echo "The Core Pod reused the same PVC."
   ```

   Matching UIDs confirm that the replacement Pod reused the existing persistent volume instead of creating a new one.
