# Deploy EMQX in Kubernetes

EMQX can be deployed on Kubernetes using two fully supported methods:

- **EMQX Operator**
- **EMQX Helm Chart**

Each method is suited for different use cases and operational requirements. This guide outlines the advantages and trade-offs of each approach to help you select the most appropriate deployment strategy.

## Recommended Method: Use EMQX Operator

The EMQX Operator is the recommended solution for deploying and managing EMQX clusters on Kubernetes, particularly in production environments or when advanced lifecycle automation is required.

Developed and maintained by the EMQX team, the Operator is purpose-built to help users deploy, configure, and manage EMQX clusters natively within Kubernetes, leveraging standard mechanisms such as Custom Resource Definitions (CRDs). By extending the Kubernetes API, it enables declarative cluster management and automates complex operational tasks such as scaling, upgrades, and failure recovery.

[View the Deployment Guide for EMQX Operator](./operator/operator.md)

### Key Advantages

- **Automated Operations:** The Operator automates complex tasks such as cluster scaling, upgrades, and failure recovery, reducing manual effort and potential for error.
- **Advanced Lifecycle Management:** It supports advanced deployment strategies, ensuring zero-downtime upgrades and graceful connection migration.
- **Simplified Configuration:** Manages EMQX through a high-level CRD, making configuration more declarative and easier to manage than extensive Helm values.
- **Encapsulated Expertise:** The Operator encapsulates the operational knowledge of running a stateful application like EMQX, ensuring best practices are followed.

### Considerations

- **Require Operator Deployment:** An additional controller (the EMQX Operator) must be installed and maintained within your Kubernetes cluster.
- **Steeper Learning Curve:** Users must become familiar with Kubernetes Operators and EMQX-specific custom resources.

## Alternative Method: Use Helm Chart

The EMQX Helm chart offers a flexible and streamlined approach to deploying EMQX on Kubernetes using Helm, the most widely used package manager in the Kubernetes ecosystem. This method is ideal for quick evaluations, development or testing environments, and for teams that prefer direct control over Kubernetes resources.

Maintained by the EMQX team, the Helm chart packages all necessary Kubernetes objects into a reusable and configurable chart. Users can define deployment parameters in a `values.yaml` file, enabling repeatable and customizable EMQX installations without writing raw YAML manifests. With a compatible Gateway API controller, the chart can also expose the EMQX Dashboard, MQTT over WebSocket, MQTTS, and WSS through `HTTPRoute` and `TLSRoute` resources.

[View the Deployment Guide for Helm Chart](./chart.md)

### Key Advantages

- **Simplicity and Familiarity:** Helm is a widely adopted tool in the Kubernetes ecosystem, making it a familiar entry point for many users.
- **Direct Control:** Provides direct, granular control over the generated Kubernetes resources, such as StatefulSets, Services, and ConfigMaps, through the `values.yaml` file.
- **No Extra Dependencies:** Does not require a separate operator controller to be running in the cluster.

### Considertaions

- **Manual Management:** Lifecycle operations such as upgrades, scaling, and complex configuration changes are more manual and less automated.
- **Limited Automation:** Lacks built-in advanced features such as automated blue-green deployments. All Day-2 operations, including scaling, upgrades, and maintenance, must be performed manually by the user.
- **Configuration Complexity:** For a production-grade setup, the `values.yaml` file can become large and complex to manage.

## Choose the Right Deployment Method

Selecting between the EMQX Operator and the Helm Chart depends on your deployment goals, environment maturity, and operational preferences. The following guidance can help you determine which approach best fits your needs:

- For most production and serious pre-production use cases, the EMQX Operator is strongly recommended. It simplifies long-term cluster management and reduces operational overhead.
- For quick evaluations, development, testing, or scenarios where direct control over Kubernetes resources is preferred, the Helm chart offers a lightweight and flexible deployment option.

