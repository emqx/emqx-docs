# EMQX Operator Overview

The EMQX Operator provides native [Kubernetes](https://kubernetes.io/) support for deploying and managing [EMQX](https://www.emqx.io/) clusters. Its primary goal is to simplify and automate the lifecycle management of EMQX in Kubernetes environments.

EMQX Operator requires Kubernetes 1.24 or higher.

EMQX Operator includes, but is not limited to, the following features:

* **Simplified Deployment**: Declare EMQX clusters with EMQX custom resources and deploy them quickly.

    For more details, see the [Getting Started](./getting-started.md) guide.

* **Cluster Management**: Automate operations and maintenance of EMQX clusters, including cluster upgrades with workload migrations, runtime data persistence, keeping Kubernetes managed resources up to date, etc.

    For more details, see the [Manage EMQX](./tasks/overview.md) section.

<img src="./assets/architecture.png" style="zoom:20%;" />

## EMQX and EMQX Operator Compatibility

The current EMQX Operator release series 2.2.x is compatible with the following EMQX versions:
- EMQX Open Source & Enterprise 5.1.1 ~ 5.8.x
- EMQX 5.9 & 5.10 <sup>*</sup>
- EMQX 6.0 and higher <sup>*</sup>

The following API versions are supported:
- [apps.emqx.io/v2beta1](./reference/v2beta1-reference.md)
- apps.emqx.io/v2alpha1 (deprecated)
- apps.emqx.io/v1beta4
- apps.emqx.io/v1beta3 (deprecated)

::: tip
<sup>*</sup> Automatic management of Durable Storage replication is not supported for these versions, scheduled for the upcoming 2.3.0 release.
:::
