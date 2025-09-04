# Deploy EMQX on Kubernetes Using Helm Chart

This page provides step-by-step instructions for deploying EMQX on a Kubernetes cluster using the official Helm chart.

The official EMQX Helm chart simplifies Kubernetes-based deployments by packaging all required EMQX components, such as StatefulSets, Services, ConfigMaps, and Ingress rules, into a single, configurable Helm chart.

## Prerequisites

Before you begin, make sure the following are installed and configured:

+ A running Kubernetes cluster (version 1.6+)
+ [Helm](https://github.com/helm/helm/releases)

## Install the EMQX Helm Chart

You can install the EMQX Helm chart either from the EMQX GitHub repository or from the official Helm chart repository.

### Install from GitHub

To install the chart with the release name `my-emqx` from GitHub:

```bash
$ git clone https://github.com/emqx/emqx.git
$ cd emqx/deploy/charts/emqx-enterprise
$ helm install my-emqx .
```

### Install from Helm Repository

To install the chart with the release name `my-emqx` from the official Helm chart repository:

```bash
helm repo add emqx https://repos.emqx.io/charts
helm install my-emqx emqx/emqx-enterprise
```
> If you want to install an unstable version, add the `--devel` flag:
>
> ```bash
> helm install my-emqx emqx/emqx-enterprise --devel
> ```

## Uninstall the Chart

To remove the EMQX release named `my-emqx` and delete all associated Kubernetes resources:

**For Helm v3 and above**

```bash
$ helm uninstall  my-emqx
```

**For Helm v2 (legacy)**

```bash
$ helm del  my-emqx
```

## Configuration Parameters

The EMQX Helm chart offers a wide range of configurable parameters through the `values.yaml` file. Refer to the table below for key parameters and default values.

| Parameter                            | Description                                                  | Default Value                                           |
| ------------------------------------ | ------------------------------------------------------------ | ------------------------------------------------------- |
| `replicaCount`                       | Recommended to use an odd number of nodes for automatic healing in case of a network split. | 3                                                       |
| `image.repository`                   | EMQX Image name                                              | emqx/emqx-enterprise                                    |
| `image.pullPolicy`                   | The image pull policy                                        | IfNotPresent                                            |
| `image.pullSecrets `                 | The image pull secrets                                       | `[]` (does not add image pull secrets to deployed pods) |
| `serviceAccount.create`              | If `true`, create a new service account.                     | `true`                                                  |
| `serviceAccount.name`                | Service account to be used. If not set and `serviceAccount.create` is `true`, a name is generated using the full-name template. |                                                         |
| `serviceAccount.annotations`         | Annotations to add to the service account.                   |                                                         |
| `envFromSecret`                      | The name pulls a secret in the same Kubernetes namespace, which contains values that will be added to the environment. | nil                                                     |
| `recreatePods`                       | Forces the recreation of pods during upgrades, which can be useful to always apply the most recent configuration. | false                                                   |
| `podAnnotations `                    | Annotations for pod                                          | `{}`                                                    |
| `podManagementPolicy`                | To redeploy a chart with existing PVC(s), the value must be set to Parallel to avoid deadlock. | `Parallel`                                              |
| `persistence.enabled`                | Enable EMQX persistence using PVC.                           | false                                                   |
| `persistence.storageClass`           | Storage class of backing PVC                                 | `nil` (uses alpha storage class annotation)             |
| `persistence.existingClaim`          | EMQX data Persistent Volume existing claim name, evaluated as a template. | ""                                                      |
| `persistence.accessMode`             | PVC Access Mode for EMQX volume                              | ReadWriteOnce                                           |
| `persistence.size`                   | PVC Storage Request for EMQX volume                          | 20Mi                                                    |
| `initContainers`                     | Containers that run before the creation of EMQX containers. They can contain utilities or setup scripts. | `{}`                                                    |
| `resources`                          | CPU/Memory resource requests/limits                          | {}                                                      |
| `extraVolumeMounts`                  | Additional volumeMounts to the default backend container.    | []                                                      |
| `extraVolumes`                       | Additional volumes to the default backend pod.               | []                                                      |
| `nodeSelector`                       | Node labels for pod assignment                               | `{}`                                                    |
| `tolerations`                        | Toleration labels for pod assignment                         | `[]`                                                    |
| `affinity`                           | Map of node/pod affinities                                   | `{}`                                                    |
| `service.type`                       | Kubernetes Service type                                      | ClusterIP                                               |
| `service.mqtt`                       | Port for MQTT                                                | 1883                                                    |
| `service.mqttssl`                    | Port for MQTT(SSL)                                           | 8883                                                    |
| `service.ws`                         | Port for WebSocket/HTTP                                      | 8083                                                    |
| `service.wss`                        | Port for WSS/HTTPS                                           | 8084                                                    |
| `service.dashboard`                  | Port for dashboard and API                                   | 18083                                                   |
| `service.customPorts`                | Custom Ports to be exposed in the Service                    | {}                                                      |
| `service.nodePorts.mqtt`             | Kubernetes node port for MQTT                                | nil                                                     |
| `service.nodePorts.mqttssl`          | Kubernetes node port for MQTT(SSL)                           | nil                                                     |
| `service.nodePorts.ws`               | Kubernetes node port for WebSocket/HTTP                      | nil                                                     |
| `service.nodePorts.wss`              | Kubernetes node port for WSS/HTTPS                           | nil                                                     |
| `service.nodePorts.dashboard`        | Kubernetes node port for dashboard                           | nil                                                     |
| `service.customNodePorts`            | Kubernetes node port for custom ports                        | {}                                                      |
| `service.loadBalancerClass`          | The load balancer implementation this Service belongs to     |                                                         |
| `service.loadBalancerIP`             | loadBalancerIP for Service                                   | nil                                                     |
| `service.loadBalancerSourceRanges`   | Address(es) that are allowed when service is LoadBalancer.   | []                                                      |
| `service.externalIPs`                | ExternalIPs for the service                                  | []                                                      |
| `service.externalTrafficPolicy`      | External Traffic Policy for the service                      | `Cluster`                                               |
| `service.annotations`                | Service/ServiceMonitor annotations                           | {}(evaluated as a template)                             |
| `service.labels`                     | Service/ServiceMonitor labels                                | {}(evaluated as a template)                             |
| `ingress.dashboard.enabled`          | Enable ingress for EMQX Dashboard                            | false                                                   |
| `ingress.dashboard.ingressClassName` | Set the ingress class for EMQX Dashboard                     |                                                         |
| `ingress.dashboard.path`             | Ingress path for EMQX Dashboard                              | /                                                       |
| `ingress.dashboard.pathType`         | Ingress pathType for EMQX Dashboard                          | `ImplementationSpecific`                                |
| `ingress.dashboard.hosts`            | Ingress hosts for EMQX Dashboard                             | dashboard.emqx.local                                    |
| `ingress.dashboard.tls`              | Ingress tls for EMQX Dashboard                               | []                                                      |
| `ingress.dashboard.annotations`      | Ingress annotations for EMQX Dashboard                       | {}                                                      |
| `ingress.dashboard.ingressClassName` | Set the ingress class for EMQX Dashboard                     |                                                         |
| `ingress.mqtt.enabled`               | Enable ingress for MQTT                                      | false                                                   |
| `ingress.mqtt.ingressClassName`      | Set the ingress class for MQTT                               |                                                         |
| `ingress.mqtt.path`                  | Ingress path for MQTT                                        | /                                                       |
| `ingress.mqtt.pathType`              | Ingress pathType for MQTT                                    | `ImplementationSpecific`                                |
| `ingress.mqtt.hosts`                 | Ingress hosts for MQTT                                       | mqtt.emqx.local                                         |
| `ingress.mqtt.tls`                   | Ingress tls for MQTT                                         | []                                                      |
| `ingress.mqtt.annotations`           | Ingress annotations for MQTT                                 | {}                                                      |
| `ingress.mqtt.ingressClassName`      | Set the ingress class for MQTT                               |                                                         |
| `metrics.enable`                     | If set to true, [prometheus-operator](https://github.com/prometheus-operator/prometheus-operator) needs to be installed, and emqx_prometheus needs to enable. | false                                                   |
| `metrics.type`                       | Now we only support "prometheus".                            | "prometheus"                                            |
| `ssl.enabled`                        | Enable SSL support                                           | false                                                   |
| `ssl.useExisting`                    | Use existing certificate or let cert-manager generate one.   | false                                                   |
| `ssl.existingName`                   | Name of existing certificate                                 | emqx-tls                                                |
| `ssl.dnsnames`                       | DNS name(s) for certificate to be generated                  | {}                                                      |
| `ssl.commonName`                     | Common name for or certificate to be generated               |                                                         |
| `ssl.issuer.name`                    | Issuer name for certificate generation                       | letsencrypt-dns                                         |
| `ssl.issuer.kind`                    | Issuer kind for certificate generation                       | ClusterIssuer                                           |

### EMQX-Specific Parameters

The following table lists the configurable EMQX-specific parameters of the chart and their default values.
| Parameter                                                                                                                                                              | Description                                                                   | Default Value |
|------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------|---------------|
| `emqxConfig`                                                                                                                                                           | A map of [configuration](https://docs.emqx.com/en/emqx/latest/configuration/configuration.html) items defined using either [environment variables](https://docs.emqx.com/en/emqx/latest/configuration/configuration.html#environment-variables) (the `EMQX_` prefix is optional) or the namespaced dotted notation used in EMQX configuration files. | `nil`         |
| `emqxLicenseSecretName`                                                                                                                                                | Name of the secret that holds the license information (deprecated)         | `nil`         |
| `emqxLicenseSecretRef.name`                                                                                                                                         | Name of the secret that holds the license information                         | `""`         |
| `emqxLicenseSecretRef.key`                                                                                                                                          | Key of the secret that holds the license information                          | `""`         |

## SSL Settings
When using `cert-manager`, TLS certificates are stored in Kubernetes secrets using the standard keys: `tls.crt` and `tls.key`. The EMQX Helm chart automatically mounts these certificate files to the following directory within the container:

```
/tmp/ssl/
```

To enable SSL support in EMQX, you must explicitly configure the file paths in the EMQX configuration. This can be done either by modifying the EMQX configuration file or by passing the following environment variables:

```yaml
EMQX_LISTENERS__SSL__DEFAULT__SSL_OPTIONS__CERTFILE: /tmp/ssl/tls.crt
EMQX_LISTENERS__SSL__DEFAULT__SSL_OPTIONS__KEYFILE: /tmp/ssl/tls.key
```

::: tip

If you are using an existing TLS certificate (instead of one generated by `cert-manager`), ensure that the file paths match the actual mounted file locations in your deployment.

:::

## Proxy Protocol Support (HAProxy, Nginx)

If deploying EMQX behind a reverse proxy that supports Proxy Protocol (e.g., HAProxy or Nginx), enable it by setting the following environment variable:

```yaml
EMQX_LISTENERS__TCP__DEFAULT__PROXY_PROTOCOL: "true"
```

For HAProxy Ingress Controller, add this annotation:

```yaml
haproxy-ingress.github.io/proxy-protocol: "v2"
```

This preserves the original client IP addresses forwarded through the proxy.
