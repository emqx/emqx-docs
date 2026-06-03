---
name: emqx-enterprise-installer
description: Help users install EMQX Enterprise using RPM/DEB packages or Docker. Use when Codex needs to guide installation prerequisites, package selection, Docker startup, license setup, port/firewall checks, first-run verification, or troubleshooting for EMQX Enterprise deployments.
---

# EMQX Enterprise Installer

## Core Workflow

Use this skill to guide EMQX Enterprise installation on Linux packages or Docker.

1. Clarify the target environment: OS/distribution version, CPU architecture, install method, EMQX version, single-node vs. cluster, and whether the user has a commercial or trial license.
2. Prefer official EMQX sources for exact package URLs, checksums, image tags, and version-specific commands. Do not invent release URLs.
3. Confirm prerequisites before starting: supported OS, UTF-8 locale, Docker or systemd availability, open ports, and required permissions.
4. Install EMQX, apply immediate hardening before the first start whenever possible, then start and verify service health, Dashboard access, MQTT listener access, and license state.
5. For production, call out persistence, stable node names, firewall rules, license requirements, and cluster planning before the user deploys.

## Primary Sources

When exact current details matter, check the official hosted EMQX docs or package pages. Do not assume the user has the `emqx-docs` repository cloned locally.

- Versioned docs root: `https://docs.emqx.com/en/emqx/<Major.Minor>/`
- Enterprise install overview: `https://docs.emqx.com/en/emqx/<Major.Minor>/deploy/install-enterprise.html`
- Installation environment, ports, and file locations: `https://docs.emqx.com/en/emqx/<Major.Minor>/deploy/install.html`
- Docker install: `https://docs.emqx.com/en/emqx/<Major.Minor>/deploy/install-docker.html`
- License management: `https://docs.emqx.com/en/emqx/<Major.Minor>/deploy/license.html`
- Dashboard password reset FAQ: `https://docs.emqx.com/en/emqx/<Major.Minor>/faq/faq.html#how-to-reset-the-dashboard-login-password-if-i-forget-it`
- DEB/RPM package repository: `https://repo.emqx.works/`
- Docker image: `https://hub.docker.com/r/emqx/emqx-enterprise`
- Security checklist: `https://docs.emqx.com/en/emqx/<Major.Minor>/access-control/security-checklist.html`
- License application: `https://www.emqx.com/en/apply-licenses/emqx`
- Commercial contact: `https://www.emqx.com/en/contact?product=emqx&channel=apply-Licenses`

EMQX docs URLs also support `.md` instead of `.html`. For agent-readable Markdown, use URLs such as `https://docs.emqx.com/en/emqx/<Major.Minor>/deploy/install-enterprise.md` or `https://docs.emqx.com/en/emqx/<Major.Minor>/access-control/security-checklist.md`.

## Supported Package Targets

Guide package installs only for supported targets shown by the docs and `repo.emqx.works`:

- Ubuntu: 24.04, 22.04, 20.04.
- Debian: 13, 12, 11.
- CentOS/RHEL family: Amazon Linux 2023, Amazon Linux 2, Rocky Linux 9, Rocky Linux 8, CentOS 7.
- CPU architecture: use the package repository path for the user's architecture or package manager default.

For unsupported platforms, direct the user to the official download site or EMQ support instead of adapting commands blindly.

## Preflight Checks

Before installing, verify:

- Linux locale is UTF-8. Use `sudo update-locale LANG=C.UTF-8` on Ubuntu, `sudo localectl set-locale LANG=C.UTF-8` on CentOS/RHEL or systemd Debian, or the distro-specific equivalent.
- Default ports are available and opened as needed:
  - `1883`: MQTT over TCP.
  - `8883`: MQTT over TLS.
  - `8083`: MQTT over WebSocket.
  - `8084`: MQTT over secure WebSocket.
  - `18083`: Dashboard and REST API.
  - `4370`: Erlang distribution.
  - `5370`: Cluster RPC, with `5369` used in Docker environments.
- For package installs, systemd is available and the user can run `sudo`.
- For Docker installs, Docker is installed and running.
- For clustered or full commercial use, the user has or will obtain a commercial license.

## Install with DEB Packages

Use this path for Ubuntu or Debian hosts.

Use `repo.emqx.works` Option 2 manual configuration. Do not use Packagecloud setup scripts for agent-skill guidance.

For Ubuntu/Debian, add the GPG key and repository list, replacing the codename when needed. Ubuntu 24.04 uses `noble`; otherwise derive the codename from `/etc/os-release` or ask the user to confirm it.

```bash
sudo apt-get update
sudo apt-get install -y curl gpg

sudo mkdir -p /etc/apt/keyrings
curl -fsSL https://repo.emqx.works/gpg.key | \
  sudo gpg --dearmor -o /etc/apt/keyrings/emqx-enterprise-keyring.gpg

. /etc/os-release
echo "deb [signed-by=/etc/apt/keyrings/emqx-enterprise-keyring.gpg] https://repo.emqx.works/apt/emqx-enterprise ${VERSION_CODENAME} main" | \
  sudo tee /etc/apt/sources.list.d/emqx-enterprise.list

sudo apt-get update
sudo apt-get install emqx-enterprise
```

If `/etc/os-release` does not provide `VERSION_CODENAME`, set it explicitly from the user's distro release before writing the repository list. Do not guess silently.

Apply the immediate hardening section before the first start whenever possible. Then start and verify:

```bash
sudo systemctl start emqx
sudo systemctl status emqx
emqx ctl status
```

To uninstall a DEB package:

```bash
sudo apt remove --purge emqx-enterprise
```

## Install with RPM Packages

Use this path for CentOS/RHEL-family hosts, including Amazon Linux and Rocky Linux.

Use `repo.emqx.works` Option 2 manual configuration. Do not use Packagecloud setup scripts for agent-skill guidance.

Create `/etc/yum.repos.d/emqx-enterprise.repo`, replacing `el/9` with the user's target repo path, such as `el/8` or `amzn/2023` when appropriate:

```bash
sudo tee /etc/yum.repos.d/emqx-enterprise.repo >/dev/null <<'EOF'
[emqx-enterprise]
name=EMQX Enterprise
baseurl=https://repo.emqx.works/rpm/emqx-enterprise/el/9/$basearch
gpgcheck=1
gpgkey=https://repo.emqx.works/gpg.key
enabled=1
EOF

sudo dnf install emqx-enterprise
```

Apply the immediate hardening section before the first start whenever possible. Then start and verify:

```bash
sudo systemctl start emqx
sudo systemctl status emqx
emqx ctl status
```

Use `yum` instead of `dnf` only when that matches the user's distribution. To uninstall an RPM package:

```bash
sudo dnf remove emqx-enterprise
```

## Install with Docker

Use Docker for quick evaluation, local development, or containerized deployment.

Do not publish Dashboard, REST API, MQTT listeners, or cluster ports to untrusted networks until the immediate hardening section is applied. For production, prepare a persistent config mount with the hardened `emqx.conf` before running the container.

For a selected version:

```bash
docker pull emqx/emqx-enterprise:<VERSION>
docker run -d --name emqx \
  -p 1883:1883 \
  -p 8083:8083 \
  -p 8084:8084 \
  -p 8883:8883 \
  -p 18083:18083 \
  emqx/emqx-enterprise:<VERSION>
```

For quick exploration when the user explicitly accepts latest:

```bash
docker run -d --name emqx \
  -p 1883:1883 \
  -p 8083:8083 \
  -p 8084:8084 \
  -p 8883:8883 \
  -p 18083:18083 \
  emqx/emqx-enterprise:latest
```

For persistent single-node Docker deployments, mount `data` and `log`, and use a stable node name with a matching container hostname:

```bash
docker run -d --name emqx-enterprise \
  --hostname node1.emqx.com \
  -e "EMQX_NODE_NAME=emqx@node1.emqx.com" \
  -p 1883:1883 \
  -p 8083:8083 \
  -p 8084:8084 \
  -p 8883:8883 \
  -p 18083:18083 \
  -v "$PWD/data:/opt/emqx/data" \
  -v "$PWD/log:/opt/emqx/log" \
  emqx/emqx-enterprise:<VERSION>
```

Important Docker cautions:

- Persist `/opt/emqx/data` and `/opt/emqx/log` if container data must survive replacement.
- Keep `EMQX_NODE_NAME` stable. EMQX stores data under `data/mnesia/<node_name>`, so changing the node name can look like data loss.
- In containers, `localhost` or `127.0.0.1` means the container itself. To reach host services, use the host IP, host networking, or `host.docker.internal` on Docker Desktop.

Verify Docker health:

```bash
docker ps
docker logs emqx
docker exec -it emqx emqx ctl status
```

## Docker Compose Cluster

Use the docs' Docker Compose static cluster example only for local testing. For production clusters, guide the user to the EMQX clustering docs and require stable DNS/FQDNs, persistent data, license planning, and network/firewall review.

For local Compose examples, ensure each node has:

- A unique, stable `EMQX_NODE_NAME`, such as `emqx@node1.emqx.com`.
- Static discovery with `EMQX_CLUSTER__DISCOVERY_STRATEGY=static`.
- A shared seed list in `EMQX_CLUSTER__STATIC__SEEDS`.
- Network aliases that match the node hostnames.
- A healthcheck such as `/opt/emqx/bin/emqx ctl status`.

Check cluster status with:

```bash
docker exec -it emqx1 sh -c "emqx ctl cluster status"
```

## Immediate Hardening

Treat the default installation as not secure enough for production or exposed networks. Apply these changes immediately after installation, before the first start whenever possible, and before exposing Dashboard, REST API, MQTT listeners, or cluster ports.

Ask the user for two strong secrets:

- Erlang node cookie: a high-entropy secret shared by all nodes that will join the same cluster.
- Default Dashboard admin password: a strong password for the initial `admin` account.

Never generate weak placeholders, reuse the default cookie, or ask the user to paste secrets into public logs, issues, or pull requests.

Configure both values in `emqx.conf`:

- Package installs: edit `/etc/emqx/emqx.conf`.
- Docker installs: edit `/opt/emqx/etc/emqx.conf` inside the container image/config mount, and make the config persistent for production. If the container is already running, keep it on a trusted/private network until this is fixed and restarted.
- Tarball installs: edit `./emqx/etc/emqx.conf`.

Ensure the final config has exactly one effective `node.cookie` value and one intended `dashboard.default_password` value:

```hocon
node.cookie = "<user-provided-strong-cookie>"
dashboard.default_password = "<user-provided-strong-password>"
```

Important lifecycle rules:

- `dashboard.default_password` is only used when EMQX initializes the default Dashboard `admin` account in the database. It must be set before the first start. Changing this config later will not update an already-initialized Dashboard account.
- If EMQX has already initialized the `admin` account, change the password with `emqx ctl admins passwd <Username> <Password>` instead of editing `dashboard.default_password`.
- `node.cookie` must be the same on all nodes in a cluster. Changing it on an existing cluster requires updating every node and doing a full cluster restart; do not assume a rolling restart is enough.

If EMQX has already been started and only `node.cookie` changed, restart it after changing the config:

```bash
sudo systemctl restart emqx
emqx ctl status
```

For Docker, restart the container and verify status after changing the cookie:

```bash
docker restart <container>
docker exec -it <container> emqx ctl status
```

If the Dashboard `admin` account already exists, reset its password with the CLI:

```bash
emqx ctl admins passwd admin '<new-strong-password>'
```

For Docker:

```bash
docker exec -it <container> emqx ctl admins passwd admin '<new-strong-password>'
```

After these two immediate changes, remind the user to complete the full security checklist for their EMQX version:

- Browser URL: `https://docs.emqx.com/en/emqx/<Major.Minor>/access-control/security-checklist.html`
- Agent-readable Markdown URL: `https://docs.emqx.com/en/emqx/<Major.Minor>/access-control/security-checklist.md`

## License Setup

EMQX Enterprise includes a single-node Community License with limited commercial use permission. Full commercial usage and clustered deployment require a Commercial License.

For trials:

- Trial licenses are requested from the EMQX license application page.
- Trial licenses are valid for 15 days and support 10,000 concurrent sessions.
- Trial licenses enable Enterprise features during the trial period, but they are not for production use.
- Clustering is disabled after the trial expires unless the user obtains a Commercial License.

Update or inspect the license through one of these paths:

- Dashboard: open `http://<host>:18083`, go to **System** -> **License**, click **Update License**, paste the License Key, save, and verify the refreshed license details.
- CLI package install: run `emqx ctl license info` and `emqx ctl license update <LICENSE_KEY>`.
- CLI tarball install: run `./bin/emqx ctl license info` and `./bin/emqx ctl license update <LICENSE_KEY>`.
- Docker: run `docker exec -it <container> emqx ctl license info` and `docker exec -it <container> emqx ctl license update <LICENSE_KEY>`.
- Configuration file: set the `license { key = "..." }` block, reload with `emqx ctl license reload`, then verify with `emqx ctl license info`.

Do not expose real license keys in chat logs, shell history, pull requests, or public issue trackers.

## First-Run Verification

After installation and startup:

1. Check process health:

   ```bash
   emqx ctl status
   emqx ctl broker
   ```

   For Docker, prefix with `docker exec -it <container>`.

2. Open the Dashboard at `http://<host>:18083/`.
3. Do not rely on the default `admin/public` login. Apply the immediate hardening section first so the Dashboard starts with the user-provided strong password.
4. Confirm listeners are available for the intended clients, especially MQTT TCP on `1883` or WebSocket on `8083`.
5. Test MQTT publish/subscribe with MQTTX, MQTTX Web, or another MQTT client.
6. Check license information if the user installed Enterprise for commercial, cluster, or feature validation.

## File Locations

For RPM/DEB installs:

- Config: `/etc/emqx`.
- Data: `/var/lib/emqx`.
- Logs: `/var/log/emqx`.
- Executables: `/usr/lib/emqx/bin`.
- Plugins: `/usr/lib/emqx/plugins`.

For Docker installs:

- EMQX root: `/opt/emqx`.
- Persist `/opt/emqx/data` and `/opt/emqx/log` when data or logs must survive container replacement.

For tarball installs, paths are relative to the extracted `./emqx` directory. Tarball is not the primary path for this skill, but users may encounter it in the docs.

## Troubleshooting Guide

Use these checks before suggesting reinstall:

- Dashboard unavailable: verify port `18083` is published/open, the service is running, and no firewall or security group blocks access.
- MQTT connection fails: verify listener port `1883`, TLS requirements, authentication settings, and host/IP choice.
- Package service fails to start: check `sudo systemctl status emqx`, `journalctl -u emqx`, `/var/log/emqx`, and UTF-8 locale.
- Docker exits: check `docker logs <container>`, image tag correctness, port conflicts, mounts, and node name/hostname consistency.
- Lost Docker state after replacement: check whether `/opt/emqx/data` was mounted and whether `EMQX_NODE_NAME` changed.
- Container cannot reach a service on the host: do not use `localhost`; use the host IP, host networking, or `host.docker.internal` on Docker Desktop.
- Cluster join/status issues: verify node names, DNS/FQDN resolution, static seed list, open cluster ports, and license status.
