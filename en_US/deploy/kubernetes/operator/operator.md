# EMQX Operator Overview

The EMQX Operator provides [Kubernetes](https://kubernetes.io/) native deployment and management of [EMQX](https://www.emqx.io/) software. The purpose of this project is to simplify and automate maintenance of EMQX clusters.

EMQX Operator requires Kubernetes 1.24 or higher.

EMQX Operator includes, but is not limited to, the following features:

* **Simplified Deployment**: Declare EMQX clusters with EMQX custom resources and deploy them quickly.

    For more details, please check [Getting Started](./getting-started.md).

* **Manage EMQX Cluster**: Automate operations and maintenance of EMQX clusters: cluster upgrades with workload migrations, runtime data persistence, keeping Kubernetes managed resources up to date, etc.

    For more details, please check [Manage EMQX](./tasks/overview.md).

<img src="./assets/architecture.png" style="zoom:20%;" />

## EMQX and EMQX Operator compatibility

Current EMQX Operator release series 2.2.x are compatible with the following EMQX releases:
- EMQX Open Source & Enterprise 5.1.1 ~ 5.8.x
- EMQX 5.9 & 5.10 (limited support)
- EMQX 6.0 and higher (limited support)

Following APIVersions are supported:
- [apps.emqx.io/v2beta1](./reference/v2beta1-reference.md)
- apps.emqx.io/v2alpha1 (deprecated)
- apps.emqx.io/v1beta4
- apps.emqx.io/v1beta3 (deprecated)
