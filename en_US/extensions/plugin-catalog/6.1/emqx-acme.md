# EMQX ACME Plugin

The EMQX ACME plugin integrates with ACME-compatible certificate authorities, such as Let's Encrypt, to automatically issue and renew TLS certificates for EMQX SSL listeners. This page describes how to configure and use the plugin with EMQX 6.1. The issued certificates are stored in EMQX managed certificate bundles.

::: warning Important Notice
Persist `<data_dir>/certs2/` across EMQX redeployments. The plugin stores the following files under `<data_dir>/certs2/global/<cert_bundle_name>/`:

- `chain.pem` and `key.pem`: The issued certificate bundle. If these files are lost, the plugin issues a new certificate the next time it starts. The new certificate counts against the Let's Encrypt limit of 5 duplicate certificates per domain each week.
- `acc-key.pem`: The ACME account key that identifies the account registered with Let's Encrypt. If this file is lost, each redeployment creates a new account. This can consume the limit of 10 new accounts per IP address every 3 hours and prevent revocation of certificates associated with the previous account.

In Docker, `<data_dir>` is `/opt/emqx/data`. On DEB/RPM installations, it is `/var/lib/emqx`. Bind-mount the entire `data/` directory, or at least `data/certs2/`, to a host volume in Docker. In Kubernetes, use a persistent volume claim (PVC). On the first issuance, the plugin generates the account key inside the bundle and replicates it to every cluster node through `emqx_managed_certs`.
:::

## Prerequisites

- The domains must resolve to the public IP address of the EMQX node.
- Public port 80 must be reachable from the internet for HTTP-01 challenge validation. If `challenge_port` is not `80`, forward traffic from public port 80 to the configured `challenge_port`.
- For staging tests, use the Let's Encrypt staging URL: `https://acme-staging-v02.api.letsencrypt.org/directory`.

## Quick Start

To configure the plugin for a single EMQX node with a publicly resolvable domain:

1. In the EMQX Dashboard, click **Management** -> **Plugins**, and install and enable the plugin.
2. Configure the following fields. Keep the default values for the other fields:
   - `domains = "mqtt.example.com"`: Enter a comma-separated list of domains. Each domain must publicly resolve to this node.
   - `contact = "mailto:admin@example.com"`: Enter a comma-separated list of contact addresses for certificate authority (CA) renewal and revocation notices.
   - `challenge_port = 5080`: Enter a high port that EMQX can bind to. Configure a reverse proxy or `iptables` redirect so that traffic arriving at public port 80 reaches this port. See [Configure Port 80 Access](#configure-port-80-access).
   - `dir_url`: Keep the default Let's Encrypt production URL, or use the staging URL while testing the configuration.
3. In the plugin UI, click **Issue / Renew Now**. On the first issuance, when the bundle is empty, the plugin performs the following actions:
   - Generates an ACME account key in the managed certificate bundle if one does not exist.
   - Issues the certificate through HTTP-01.
   - Rewrites each listener in `listener_ids`, which defaults to `ssl:default,wss:default`, to use the new bundle.
   - Creates the Dashboard HTTPS listener on port `18084` with the same certificate because `enable_dashboard_https` is `true` by default.

   On subsequent runs, the plugin updates only the bundle files. The listener configuration and Dashboard HTTPS configuration remain unchanged. The Erlang SSL PEM cache reloads the new certificate without restarting the listeners.
4. Open `https://your.domain:18084/` and log in to the Dashboard. In the plugin UI, click **Disable Dashboard HTTP Listener**. This button is available only when the plugin page is open over HTTPS. After the operation succeeds, the plaintext listener on port `18083` is disabled across the cluster. This configuration is recommended for production deployments because leaving the HTTP listener enabled allows plaintext access to the Dashboard.

The plugin checks the certificate at the interval specified by `check_interval_hours` and renews it automatically when required.

## How It Works

1. The plugin registers an ACME account (or reuses an existing one) with the configured CA.
2. It starts an ephemeral HTTP listener to respond to HTTP-01 challenges during issuance.
3. The issued certificate chain and private key are stored in a managed certificate bundle. By default, the plugin also stores the ACME account key in this bundle. If `acc_key` is configured, the plugin uses the operator-managed file at that path instead. For more information, see [ACME Account Key](#acme-account-key).
4. SSL listeners reference the bundle through `ssl_options.managed_certs.bundle_name`. On the first issuance, the plugin can rewrite this field for the listeners specified by `listener_ids`.
5. The plugin checks the certificate at the interval specified by `check_interval_hours`. If the certificate expires within the period specified by `renew_before_expiry_days`, the plugin renews it. The renewal updates the bundle files in place, and the Erlang SSL PEM cache loads the new certificate without restarting the listeners.

## Example Configuration

The plugin renders the field descriptions from `config_schema.avsc` in the Dashboard configuration form. The following HOCON example shows a typical plugin configuration. Hover over a field label in the Dashboard to view its description.

```hocon
dir_url = "https://acme-v02.api.letsencrypt.org/directory"
# Comma-separated list of SAN domains for the cert.
domains = "mqtt.example.com,mqtt2.example.com"
# Comma-separated list of CA contact addresses (renewal/revocation notices).
contact = "mailto:admin@example.com,mailto:ops@example.com"
cert_bundle_name = "acme"
# Comma-separated list of listener IDs to migrate (each "ssl:<name>" or "wss:<name>").
listener_ids = "ssl:default,wss:default"
cert_type = "ec"
# High port EMQX can bind; reverse-proxy or iptables-redirect 80 -> this.
challenge_port = 5080
renew_before_expiry_days = 30
check_interval_hours = 24
enable_dashboard_https = true
dashboard_https_port = 18084
# acc_key is left unset; the plugin manages it inside the cert bundle.
```

Then configure an SSL listener to use the bundle. For listeners specified by `listener_ids`, the plugin rewrites this setting on the first issuance.

```hocon
listeners.ssl.default {
  bind = "0.0.0.0:8883"
  ssl_options {
    managed_certs {
      bundle_name = "acme"
    }
  }
}
```

## ACME Account Key

In RFC 8555, the ACME account private key identifies the account. The client generates the key locally and sends a `newAccount` request signed with the key. The CA then creates the account. The key is not registered separately through a portal.

**Default behavior:** Leave `acc_key` unset. On the first issuance, the plugin generates an EC P-256 key, or an RSA-2048 key if `cert_type = "rsa"`, in memory. The plugin then uses `emqx_managed_certs:add_managed_files/3` to write the key to `<data_dir>/certs2/global/<cert_bundle_name>/acc-key.pem` on every cluster node. Subsequent issuances reuse the same file. Persist the data directory with a bind mount or PVC to retain both the account key and certificate chain. See the persistence warning at the beginning of this page.

**Operator override:** Set `acc_key` when the key must use a path outside the bundle, such as a Kubernetes Secret mounted at a known location or a key shared with other software. Set `acc_key` to the `file://` URI of the PEM file. The plugin reads the file during every issuance and does not overwrite it. If the file does not exist on the local node, the plugin generates one on that node. This file is not replicated across the cluster, so you must distribute it to each cluster node. If the PEM file is encrypted, set `acc_key_password` to a `file://` URI that points to a plaintext password file. `${EMQX_ETC_DIR}` and `${VAR}` are expanded when used, allowing the same configuration to work with Docker and DEB/RPM installations.

## Configure Port 80 Access

ACME CAs always perform the HTTP-01 challenge against port 80 of the domain being validated. This behavior is defined by RFC 8555 and cannot be configured on the CA. EMQX runs as the non-root user `emqx` and cannot normally bind to ports below `1024`. Therefore, setting `challenge_port = 80` typically fails with `eacces`.

Set `challenge_port` to a high port that EMQX can bind to, such as `5080`. Then use one of the following methods to route traffic from public port 80 to the configured `challenge_port`:

- **Reverse proxy:** Run NGINX, Caddy, or HAProxy on the same host as root or with the `CAP_NET_BIND_SERVICE` capability. Proxy `http://domain/.well-known/acme-challenge/*` to `http://127.0.0.1:<challenge_port>`. Other paths can return `404`.
- **Port forwarding:** On Linux, use `iptables` to redirect inbound traffic on port 80 to the high port:

  ```bash
  iptables -t nat -A PREROUTING -p tcp --dport 80 \
                  -j REDIRECT --to-port 5080
  ```

  You can also use `socat` or `systemd` socket activation to bridge the two ports.
- **Kernel capability:** Grant the EMQX binary the `CAP_NET_BIND_SERVICE` capability so that it can bind directly to port 80:

  ```bash
  setcap 'cap_net_bind_service=+ep' \
         /opt/emqx/erts-*/bin/beam.smp
  ```

  This approach depends on the operating system and packaging method and is not recommended for containerized deployments. Use a reverse proxy when possible.

## API Endpoints

The following table lists the main endpoints available through the plugin API gateway at `/api/v5/plugin_api/emqx_acme-<version>/`:

| Method | Path | Description |
| --- | --- | --- |
| GET | `/status` | Return the current state, including `domains`, `cert_bundle_name`, `in_progress`, `last_result`, `last_check`, and `certificate`. If a certificate exists, `certificate` contains `exists`, `chain_path`, `key_path`, and `expiry`. Otherwise, it contains `exists: false`. |
| POST | `/issue` | Kick off issuance asynchronously. Returns `202 {"result":"started"}`; poll `/status` for the outcome. Returns `409` if another action is already running. |
| POST | `/renew` | Same shape as `/issue`, but for renewal. |
| POST | `/disable_dashboard_http` | Set `dashboard.listeners.http.bind = 0` cluster-wide, stopping the plaintext listener. Returns `409 NO_HTTPS_LISTENER` if no Dashboard HTTPS listener is configured. |

These endpoints support the primary certificate-management operations. You typically do not need to call them directly because the plugin UI performs these operations.

## Troubleshooting

### Issuance Succeeds in Let's Encrypt Staging but Fails in Production

**Symptom:** Certificate issuance fails with an error that contains the following text:

> `During secondary validation: DNS problem: query timed out looking up A for ...`

**Cause:** The error reports a DNS lookup timeout during a secondary validation attempt. Let's Encrypt uses multi-perspective validation in both staging and production. A successful staging request therefore does not guarantee that a later production request will succeed. Transient DNS or network conditions, inconsistent DNS responses, or unreachable addresses in the domain's DNS records can cause different validation results.

**Resolution:**

- Confirm that the domain's authoritative name servers consistently return the expected `A` and `AAAA` records. For example, run `dig @8.8.8.8 your.domain` and `dig @1.1.1.1 your.domain`.
- Confirm that public port 80 is reachable for every address returned by the domain's `A` and `AAAA` records and that traffic reaches the configured `challenge_port`.
- Use the [Let's Debug diagnostic service](https://letsdebug.net) to check the domain from an external validation perspective.
- Avoid repeated retries. Let's Encrypt production allows up to 5 authorization failures per identifier per account each hour. Resolve the DNS or network issue before requesting another certificate.

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## Download

Tarballs for each EMQX release:

| EMQX Version | Plugin Version | Package |
|---|---|---|
| 6.1.2 | 0.2.0 | [emqx_acme-0.2.0.tar.gz](https://packages.emqx.io/emqx-plugins/6.1.2/emqx_acme-0.2.0.tar.gz) |
| 6.1.3 | 0.2.0 | [emqx_acme-0.2.0.tar.gz](https://packages.emqx.io/emqx-plugins/6.1.3/emqx_acme-0.2.0.tar.gz) |

<!-- PLUGIN-DOWNLOADS:END -->
