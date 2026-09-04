# Upgrade EMQX Operator

EMQX Operator 3.0 introduces the `apps.emqx.io/v3beta1` API and cannot convert EMQX custom resources created with earlier API versions. Moving from Operator 2.3 to 3.0 therefore requires a migration rather than an in-place upgrade.

Support for a live migration workflow is planned for EMQX Operator 3.1.

## Migrate from Operator 2.3 to 3.0

Follow [Migrate from EMQX Operator 2.3 to 3.0](./migration/migrate-from-2.3-to-3.0.md) to back up the existing cluster, convert its manifest, deploy a new cluster under Operator 3.0, and switch client traffic.

## Upgrade from Operator 2.2 to 2.3

If you must stay on the 2.3 release line, use the EMQX Operator 2.3 documentation. Operator 3.0 does not support EMQX API versions earlier than
`v3beta1`.
