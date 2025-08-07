# Deploy EMQX in Kubernetes

Deploying EMQX on Kubernetes can be approached in two primary ways: using the EMQX Operator or using the official Helm chart. Both methods are fully supported, but they cater to different needs and levels of operational complexity.

This document will help you choose the best method for your scenario by outlining the pros and cons of each and providing links to the detailed documentation.

## Recommended Method: EMQX Operator

The EMQX Operator is the recommended method for deploying and managing EMQX clusters on Kubernetes, especially for production environments or when you require advanced lifecycle management. The Operator is a piece of software that extends the Kubernetes API to create, configure, and manage instances of EMQX on behalf of a user.

[**Learn how to deploy with EMQX Operator](./operator/operator.md)**

### Pros

- **Automated Operations:** The Operator automates complex tasks such as cluster scaling, upgrades, and failure recovery, reducing manual effort and potential for error.
- **Advanced Lifecycle Management:** It supports sophisticated deployment strategies like blue-green updates, ensuring zero-downtime upgrades and graceful connection migration.
- **Simplified Configuration:** Manages EMQX through a high-level Custom Resource Definition (CRD), making configuration more declarative and easier to manage than extensive Helm values.
- **Encapsulated Expertise:** The Operator encapsulates the operational knowledge of running a stateful application like EMQX, ensuring best practices are followed.

### Cons

- **Additional Component:** Requires the installation and maintenance of the Operator itself within your Kubernetes cluster.
- **Steeper Learning Curve:** Users need to familiarize themselves with the concepts of Kubernetes Operators and the specific EMQX CRDs.

## Alternative Method: Helm Chart

The EMQX Helm chart provides a straightforward way to deploy EMQX on Kubernetes using the popular package manager, Helm. This method is well-suited for getting started quickly, for development/testing environments, or for users who prefer to manage Kubernetes resources directly.

[**Learn how to deploy with Helm](./chart.md)**

### Pros

- **Simplicity and Familiarity:** Helm is a widely adopted tool in the Kubernetes ecosystem, making it a familiar entry point for many users.
- **Direct Control:** Provides direct, granular control over the generated Kubernetes resources (like StatefulSets, Services, and ConfigMaps) through the `values.yaml` file.
- **No Extra Dependencies:** Does not require a separate operator controller to be running in the cluster.

### Cons

- **Manual Management:** Lifecycle operations such as upgrades, scaling, and complex configuration changes are more manual and less automated.
- **Limited Automation:** Lacks built-in advanced features like automated blue-green deployments. Day-2 operations are entirely the user's responsibility.
- **Configuration Complexity:** For a production-grade setup, the `values.yaml` file can become large and complex to manage.

## Which Method Should You Choose?

- **For most production and serious pre-production use cases, the EMQX Operator is strongly recommended.** It simplifies long-term management and reduces operational overhead.
- **For quick evaluations, development, testing, or if you have a strong preference for managing resources directly with Helm, the Helm chart is an excellent choice.**

