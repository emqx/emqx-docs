# EMQX Version Lifecycle and End-of-Life (EOL) Dates

EMQX serves a wide range of users, from fast-moving SaaS platforms to large enterprise deployments. To meet these diverse needs, EMQX has adopted a multi-tiered release and support policy that balances innovation, stability, and long-term maintenance.

This lifecycle policy is designed to:

- **Support rapid innovation** in [EMQX Cloud](../get-started/concept.md#which-products-do-we-offer) through frequent internal updates.
- **Deliver new features** to on-premise users on a predictable, quarterly schedule.
- **Provide long-term stability** to enterprise customers through designated **Long-Term Support (LTS)** versions.
- **Ensure clear and predictable timelines** for version support and **End-of-Life (EOL)**.

By offering three distinct release tiers (Internal, Feature, and LTS), EMQX empowers users to choose the version cadence that best fits their operational needs, whether they value rapid access to the latest capabilities or require long-term platform stability.

## Release Tiers and Cadence

To balance rapid innovation with enterprise-grade stability, EMQX follows a three-tier release model:

### Internal Releases

*   **Frequency:** Monthly.
*   **Audience:** Internal EMQX teams, EMQX Cloud, and prospects.
*   **Purpose:** Quickly deliver and validate new features in EMQX-controlled environments (e.g., EMQX Cloud, internal testing, and limited public staging). Internal Releases also provide early access to selected customers and serve as a foundation for future public Feature Releases.
*   **Quality Assurance:** Although not publicly downloadable, Internal Releases power EMQX Cloud and undergo full QA to ensure they are production-ready. They are **not** experimental or unstable.
*   **Support:** Maintained directly by the EMQX team for the Cloud environment. Not for direct on-premise customer consumption.

### Feature Releases (Public)

*   **Frequency:** Quarterly (typically in March, June, September, December).
*   **Audience:** On-premise users seeking timely access to the latest stable features.
*   **Purpose:** Package and deliver validated features from recent Internal Releases for general on-premise use.
*   **Support Duration:** 18 months from the initial release date.

### Long-Term Support (LTS) Releases (Public)

*   **Frequency:** Annually (typically the June Feature Release is designated as LTS).
*   **Audience:** Enterprise users with mission-critical deployments requiring maximum stability and long-term planning.
*   **Purpose:** Provide a highly stable, extensively validated version with an extended support lifecycle.
*   **Support Duration:** 3 years from the initial release (with 4.4.x as a special case supported for 5 years).

This tiered release model allows features to mature through internal validation before public release, ensuring greater stability and quality for on-premise deployments.

## Current LTS Versions

*   **4.4.x** (Initial release date: Dec 21, 2021, EOL date: Dec 20, 2026)
*   **5.8.x** (Initial release date: Aug 28, 2024, EOL Date: Aug 27, 2027)

## Version Numbering and Naming Conventions

EMQX uses Semantic Versioning (`MAJOR.MINOR.PATCH`, e.g., `5.8.1`) as the foundation, with specific conventions for different release tiers.

### Major Releases (e.g., `4.0.0`, `5.0.0`, `6.0.0`)

*   Introduce significant updates, potentially including non-backward-compatible changes.
*   Direct in-place upgrades between major versions are generally not supported. Migration typically involves setting up a new deployment and manually replicating configurations/data. Refer to specific major version migration guides.

### Internal Releases (e.g., `5.9.0-M3.202506`)

*   **Format:** `X.Y.Z-M[1-3].YYYYMM` (e.g., `5.9.0-M2.202505`, `5.9.1-M3.202506`). 
*   `X.Y` often aligns with the upcoming or current public Feature Release minor version being developed. 
*   `Z` can increment with internal iterations. Ad-hoc fixes may apply.

### Feature Releases (e.g., `5.7.0`, `5.9.0`, `5.10.0`)

*   Public minor releases that deliver new features, improvements, and bug fixes quarterly. 
*   Backward-compatible (API and functionality) with previous minor releases within the same major version (e.g., `5.x`) and support rolling cluster upgrades.
*   **Format:** `X.Y.0`. 
*   `Y` (minor version) increments with each quarterly release.
*   **Naming:** EMQX Enterprise X.Y.0.

### LTS Releases (e.g., `5.8.0`)

*   A designated annual Feature Release promoted to Long-Term-Support status. The initial LTS release (`X.Y.0`) includes the latest stable and fully validated feature set available at the time of its release.
*   **Format:** `X.Y.0`.
*   **Naming:** `EMQX Enterprise X.Y.0`.
*   LTS status is clearly indicated in documentation and on the official download page.

### Patch Releases (e.g., `5.8.1`, `5.9.2`)

*   Contains only critical bug fixes and security patches.
*   No new features or breaking changes are introduced.
*   Issued for:
    *   **Feature Releases:** Each Feature Minor Release line that is within its 18-month support lifecycle (starting from the initial release of the corresponding minor version, i.e., X.Y.0) will receive patches for critical bug fixes and security vulnerabilities **that are applicable to (i.e., affect) that specific release line.**
    *   **LTS Releases:** Each active LTS Release line will receive patches for critical bug fixes and security vulnerabilities **that are applicable to (i.e., affect) that specific release line** throughout its 3-year support period.

## Maintenance Policies

EMQX releases receive different maintenance based on their type and lifecycle stage:

*   **Internal Release:**
    *   **Maintenance:** Handled by the EMQX team. Bugs found are typically addressed in subsequent internal iterations or via hotfixes within the Cloud environment. These fixes are merged into the dev branch for inclusion in future public releases.
*   **Feature Releases (Public):**
    *   **Support Scope:** New features and improvements are introduced with each new quarterly Feature Minor Release (e.g., 5.9.0, then 5.10.0). During their 18-month lifecycle, each Feature Release line (e.g., all 5.9.x versions) will receive **critical bug fixes and security patches** via its own patch releases (e.g., 5.9.1, 5.9.2) for issues applicable to that line.
    *   **Target Users:** Users and customers who want rapid access to new features and are comfortable with a shorter support cycle.
*   **Long-Term Support (LTS) Releases (Public):**
    *   **Initial LTS Release (e.g., 5.8.0):** Released with the latest stable feature set.
    *   **Subsequent LTS Patch Releases (e.g., 5.8.1, 5.8.2):** Throughout their 3-year lifecycle, LTS versions will receive the following updates via Patch releases:
        *   **Critical Bug Fixes**
        *   **Security Vulnerability Patches**
        *   **Important Note:** To ensure maximum stability, patch releases (X.Y.Z) for an LTS version will **not introduce any new features.**
    *   **Target Users:** Enterprise customers with stringent requirements for system stability, predictability, and long-term support, as well as scenarios where minimizing changes post-deployment is critical.
*   **Patch Release:**
    *   Such as 5.10.1, 5.10.2, contain only bug fixes and security patches. No new features are introduced.
    *   In rare cases, to support migration to newer versions, the maintenance team may, at its discretion, consider adding minimal, essential non-disruptive changes to versions in maintenance mode.

## Maintenance Schedule for Public Releases

This section provides the maintenance and End-of-Life (EOL) timelines for public Feature and LTS releases. Internal releases are maintained separately and are not listed here.

The following tables list active release lines and release lines that have reached EOL. Dates and support periods apply to the entire release line, including its patch releases.

### Active Releases

| Version | Release Date | Support Duration (Months) | EOL Date | Notes |
| :--- | :--- | :--- | :--- | :--- |
| **4.4 LTS** | **2021-12-21** | **60** | **2026-12-21** | Extended support during the transition period. |
| **5.8 LTS** | **2024-08-28** | **36** | **2027-08-27** | **Current Active LTS Version** |
| **5.10** | 2025-06-09 | 18 | 2026-12-08 |  |
| **6.0** | 2025-09-30 | 18 | 2027-03-30 |  |
| **6.1** | 2025-12-30 | 18 | 2027-06-30 |  |
| **6.2** | 2026-03-31 | 18 | 2027-09-30 |  |
| **6.3 LTS** | **2026-09-03** | **36** | **2029-09-02** | **Latest Active LTS Version** |

### End-of-Life Releases

These release lines have reached EOL. The last patch release is listed for each line.

| Version | Release Date | Support Duration (Months) | EOL Date | Last Patch Release |
| :--- | :--- | :--- | :--- | :--- |
| **3.4** | 2019-12-02 | 18 | 2021-06-30 | 3.4.5 |
| **4.0** | 2020-01-18 | 18 | 2021-07-17 | 4.0.10 |
| **4.1** | 2020-07-18 | 18 | 2022-01-17 | 4.1.5 |
| **4.2** | 2020-10-13 | 18 | 2022-04-12 | 4.2.13 |
| **4.3** | 2021-05-19 | 18 | 2022-11-18 | 4.3.19 |
| **5.0** | 2023-02-03 | 18 | 2024-08-02 | 5.0.4 |
| **5.1** | 2023-06-21 | 18 | 2024-12-20 | 5.1.1 |
| **5.2** | 2023-09-07 | 18 | 2025-03-06 | 5.2.1 |
| **5.3** | 2023-09-29 | 18 | 2025-03-28 | 5.3.2 |
| **5.4** | 2023-12-23 | 18 | 2025-06-22 | 5.4.1 |
| **5.5** | 2024-02-01 | 18 | 2025-07-31 | 5.5.1 |
| **5.6** | 2024-03-28 | 18 | 2025-09-27 | 5.6.1 |
| **5.7** | 2024-05-27 | 18 | 2025-11-26 | 5.7.2 |
| **5.9** | 2025-05-02 | 18 | 2026-11-01 | 5.9.4 |

## Upgrade Path Recommendations

To ensure access to the latest features, performance improvements, and security updates, we recommend users follow a regular upgrade strategy based on their deployment type:

*   **EMQX Cloud:** Fully managed by the EMQX team; users benefit from monthly updates seamlessly.
*   **On-Premise Feature Releases:** Users can upgrade from one quarterly Feature Release to the next to access new features. It is recommended to stay on a Feature Release that is actively supported (within its 18-month lifecycle).
*   **On-Premise LTS Releases:** Enterprise users can plan migrations from their current LTS version to the next, with sufficient overlap to ensure uninterrupted long-term support. Upgrades from a Feature Release to an LTS Release are also supported.

Each release includes detailed upgrade guides and compatibility notes to assist with planning and execution.
