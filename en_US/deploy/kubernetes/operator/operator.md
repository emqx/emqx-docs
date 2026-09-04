# EMQX Operator Overview

The EMQX Operator provides native [Kubernetes](https://kubernetes.io/) support for deploying and managing [EMQX](https://www.emqx.io/) clusters. Its primary goal is to simplify and automate the lifecycle management of EMQX in Kubernetes environments.

EMQX Operator 3.0 requires Kubernetes 1.27 or later. On Kubernetes 1.27 through 1.31, enable the `StatefulSetAutoDeletePVC` feature gate. This feature is enabled by default in Kubernetes 1.32 and later.

EMQX Operator includes, but is not limited to, the following features:

* **Simplified Deployment**: Declare EMQX clusters with EMQX custom resources and deploy them quickly.

    For more details, see the [Getting Started](./getting-started.md) guide.

* **Cluster Management**: Automate operations and maintenance of EMQX clusters, including rolling updates, runtime data persistence, keeping Kubernetes managed resources up to date, etc.

    For more details, see the [Manage EMQX](./tasks/overview.md) section.

<img src="./assets/architecture.png" style="zoom:20%;" />

## EMQX and EMQX Operator Compatibility

### EMQX Operator 3.0.x

The EMQX Operator 3.0.x release series is compatible with the following EMQX versions:
- EMQX 5.9 and 5.10
- EMQX 6.0 and later

The following API versions are supported:
- [apps.emqx.io/v3beta1](./reference/v3beta1-reference.md)

EMQX Operator 3.0 introduces in-place rolling updates for Core nodes, Deployment-style rollouts for Replicant nodes, and HPA-compatible scaling of Core-Replicant clusters. It does not provide backward compatibility with earlier EMQX CR API versions.

### Past Releases

#### EMQX Operator 2.3.x

The EMQX Operator 2.3.x release series is fully compatible with the following EMQX versions:
- EMQX 5.9 and 5.10
- EMQX 6.0 and later

The following API versions are supported:
- [apps.emqx.io/v2](./reference/v2-reference.md)
- [apps.emqx.io/v2beta1](./reference/v2beta1-reference.md) (deprecated)

#### EMQX Operator 2.2.x

EMQX Operator 2.2.x release series is compatible with the following EMQX versions:
- EMQX Open Source & Enterprise 5.1.1 – 5.8.x
- EMQX 5.9 & 5.10 (limited support<sup>*</sup>)
- EMQX 6.0 and higher (limited support<sup>*</sup>)

The following API versions are supported:
- [apps.emqx.io/v2beta1](./reference/v2beta1-reference.md)
- apps.emqx.io/v2alpha1 (deprecated)
- apps.emqx.io/v1beta4
- apps.emqx.io/v1beta3 (deprecated)

::: tip
<sup>*</sup> Automatic management of Durable Storage replication is not supported for these versions.
:::
