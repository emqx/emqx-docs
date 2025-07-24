# EMQX Version Lifecycle and Support Policy

## Background and Challenges

EMQX is committed to delivering a powerful, reliable, and feature-rich MQTT platform. We recognize that our diverse user base (especially now, under the BSL license, we have distinct audiences) has varying needs:

1.  **EMQX Cloud** requires rapid iteration to deliver new capabilities and improvements to SaaS customers.
2.  Some **customers** frequently require access to specific, often newly developed, features to validate EMQX's suitability for their needs during PoC stages. The ability to deliver these features quickly, even before they are in a general public release, can be critical to securing new business.
3.  Some **on-premise users** desire the latest features and improvements in a timely but predictable manner.
4.  Some **Enterprise customers** prioritize maximum stability, predictability, and long-term support for their mission-critical deployments.

A single release cadence struggles to meet these varied requirements effectively. A very frequent public release cycle can be perceived as immature by enterprises, while a slow cycle frustrates Cloud and feature-driven users.

## Goals

To address this, EMQX is implementing a refined, multi-tier versioning, release, and End-of-Life (EOL) policy. The goals are to:

*   **Accelerate Innovation:** Enable monthly updates and validation via Internal Releases, primarily benefiting EMQX Cloud.
*   **Provide Balanced On-Premise Updates:** Offer well-vetted new features to on-premise users quarterly via public Feature Releases.
*   **Ensure Enterprise Stability:** Deliver annual Long-Term Support (LTS) releases for enterprises requiring maximum stability and extended support.
*   **Enhance Predictability & Confidence:** Offer clear, predictable release cadences and support lifecycles for each tier.
*   **Clarify BSL Implications:** Align our Business Source License (BSL) model with these commitments for community users and commercial customers.

This revised policy is designed to create a transparent and sustainable framework that supports rapid innovation for EMQX Cloud while ensuring robust, predictable options for on-premise deployments.

## Release Tiers and Cadence

The EMQX team strives to provide a robust, reliable, and feature-rich MQTT platform for our users and customers. To balance rapid innovation with enterprise-grade stability requirements, EMQX adopts a three-tier release model:

*   **Internal Releases:**
    *   **Frequency:** Monthly.
    *   **Purpose:** Rapidly introduce and validate new features and improvements within EMQX-controlled environments (including EMQX Cloud, internal dogfooding, and potentially public EMQX cluster staging) and to deliver specific new features to selected customers. Serves as a proving ground for functionality that may later be included in public Feature Releases.
    *   **Audience:** Primarily internal EMQX teams, EMQX Cloud, and prospects.
    *   **Quality Assurance:** Despite not being directly downloadable by on-premise customers, Internal Releases power the public EMQX Cloud service. Therefore, they undergo rigorous quality assurance, including full QA testing, and must be production-ready for the cloud environment. They are **not** experimental or unstable builds.
    *   **Support:** Maintained directly by the EMQX team for the Cloud environment. Not for direct on-premise customer consumption.
*   **Feature Releases (Public):**
    *   **Frequency:** Quarterly (typically targeting March, June, September, December).
    *   **Purpose:** Consolidate mature, validated features from the preceding Internal Release cycles into a publicly available version for on-premise deployments.
    *   **Audience:** On-premise users and customers who want access to the latest stable features.
    *   **Support:** Supported for **18 months** from their initial release date.
*   **Long-Term Support (LTS) Releases (Public):**
    *   **Frequency:** Annually (typically the June Feature Release is designated as LTS).
    *   **Purpose:** Provide a highly stable, extensively validated version with an extended support lifecycle.
    *   **Audience:** Enterprise customers with mission-critical deployments requiring maximum stability and long planning horizons.
    *   **Support:** Supported for **3 years** from their initial release date. (4.4.x is a special case with a 5-year support cycle.)

This tiered approach allows features to mature through internal validation before wider public availability, ensuring higher quality for all on-premise releases.

### Current LTS Versions

*   **4.4.x** (Initial release date: Dec 21, 2021, EOL date: Dec 20, 2026)
*   **5.8.x** (Initial release date: Aug 28, 2024, EOL Date: Aug 27, 2027)

## Version Numbering and Naming Conventions

EMQX follows Semantic Versioning as a base, with specific conventions for each tier.

**MAJOR.MINOR.PATCH** e.g. 5.8.1

*   **Major Releases - Public (e.g., 4.0.0, 5.0.0, 6.0.0):**
    *   Introduce significant updates, potentially including non-backward-compatible changes.
    *   Direct in-place upgrades between major versions are generally not supported. Migration typically involves setting up a new deployment and manually replicating configurations/data. Consult specific major version migration guides.
*   **Internal Releases (e.g., 5.9.0-M3-202506):**
    *   **Numbering & Suffix:** X.Y.Z-M[1-3].YYYYMM (e.g., 5.9.0-M2.202505, 5.9.1-M3.202506). The X.Y often aligns with the upcoming or current public Feature Release minor version being developed. Z can increment with internal iterations. Ad-hoc fixes may apply.
*   **Feature Releases (Public Minor Releases):**
    *   Deliver new features, improvements, and bug fixes. These versions are backward-compatible (API and functionality) with previous minor releases within the same major version series and support rolling cluster upgrades.
    *   **Numbering:** X.Y.0 (e.g., 5.7.0, 5.9.0, 5.10.0). The Y (minor version) increments with each quarterly release.
    *   **Naming:** EMQX Enterprise X.Y.0.
*   **LTS Releases (Public Minor Releases):**
    *   A specific minor version designated as LTS. Its initial release (X.Y.0) will contain the latest stable feature set at that time.
    *   **Numbering & Suffix:** X.Y.0 (e.g., 5.8.0). Based on a designated annual Feature Release.
    *   **Naming:** EMQX Enterprise X.Y.0. LTS status is reflected in the documentation and on the official website's download page.
*   **Patch Releases - Public:**
    *   Contains only critical bug fixes and security patches.
    *   Do not introduce any new features or breaking changes into the version line they are patching.
    *   Patch releases are issued for:
        *   **Feature Releases:** Each Feature Minor Release line that is within its 18-month support lifecycle (start from the initial release of the corresponding minor version, i.e., X.Y.0) will receive patches for critical bug fixes and security vulnerabilities **that are applicable to (i.e., affect) that specific release line.**
        *   **LTS Releases:** Each active LTS Release line will receive patches for critical bug fixes and security vulnerabilities **that are applicable to (i.e., affect) that specific release line** throughout its 3-year support period.

## Maintenance policies

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

## BSL (Business Source License) and Version Support

*   **Community Users:**
    *   Community users can use all EMQX versions (including those based on LTS branches) under the BSL terms.
    *   Usage by community users is **"as is," without any warranty, express or implied.**
    *   The BSL ensures that the code transitions to a full open-source license (e.g., Apache 2.0) after a "Change Date". This means the community will eventually receive a fully open-source software version that has benefited from long-term maintenance (especially LTS versions).
    *   Prior to the "Change Date," community users benefit from patch releases (primarily bug fixes and security updates) made available for BSL versions.
*   **Enterprise Customers:**
    *   EMQX Enterprise customers, through their commercial license and subscription, receive access to premium features and **official commercial support, including any applicable warranties as per their contractual agreement.**
    *   The 5-year support commitment for LTS versions is a key component of the Enterprise offering, ensuring customers receive long-term, guaranteed support for their production environments, even if the corresponding code branch has transitioned to an open-source license under BSL terms.

## Maintenance table (Public Releases)

This table focuses on public Feature and LTS releases. Internal releases are managed separately.

The following table outlines release dates, support types, lifecycles, and estimated End-of-Life (EOL) dates for EMQX major versions. This table will be updated periodically.

| Version | Release Date | Support Type | Lifetime | EOL Date | Notes |
| :--- | :--- | :--- | :--- | :--- | :--- |
| **3.4.x** | 2019-12-02 | Feature Release | 18 months | 2021-06-30 | EOLed |
| **4.0.x** | 2020-01-18 | Feature Release | 18 months | 2021-07-17 | EOLed |
| **4.1.x** | 2020-07-18 | Feature Release | 18 months | 2022-01-17 | EOLed |
| **4.2.x** | 2020-10-13 | Feature Release | 18 months | 2022-04-12 | EOLed |
| **4.3.x** | 2021-05-19 | Feature Release | 18 months | 2022-11-18 | EOLed |
| **4.4.x LTS** | **2021-12-21** | **LTS** | **3 years** For 4.4.x LTS, as a special case during the transition period, the EOL Date is set to five years after the release date. | **2026-12-21** | **Current Active LTS Version** |
| **5.0.x** | 2023-02-03 | Feature Release | 18 months | 2024-08-02 | |
| **5.1.x** | 2023-06-21 | Feature Release | 18 months | 2024-12-20 | |
| **5.2.x** | 2023-09-07 | Feature Release | 18 months | 2025-03-06 | |
| **5.3.x** | 2023-09-29 | Feature Release | 18 months | 2025-03-28 | |
| **5.4.x** | 2023-12-23 | Feature Release | 18 months | 2025-06-22 | |
| **5.5.x** | 2024-02-01 | Feature Release | 18 months | 2025-07-31 | |
| **5.6.x** | 2024-03-28 | Feature Release | 18 months | 2025-09-27 | |
| **5.7.x** | 2024-05-27 | Feature Release | 18 months | 2025-11-26 | |
| **5.8.x LTS** | **2024-08-28** | **LTS** | **3 years** | **2027-08-27** | **Latest Active LTS Version** |
| **5.9.x** | 2025-05-02 | Feature Release | 18 months | 2026-11-01 | Latest Feature Release |
| **5.10.x** | 2025-06-09 | Feature Release | 18 months | 2026-12-08 | Next Feature Release |
| **6.0.0** | 2025-??-?? | | 18 months | 202?-??-?? | Next Major Release |

## Upgrade Path Recommendations

*   In general, we encourage users and customers to plan regular upgrades to take advantage of new features, performance improvements, and the latest security patches.
*   **EMQX Cloud:** Managed by the EMQX team; users benefit from monthly updates seamlessly.
*   **On-Premise Feature Releases:** Users can upgrade from one quarterly Feature Release to a subsequent one to access new features. It is recommended to stay on a Feature Release that is actively receiving patches (i.e., within its 18-month lifecycle).
*   **On-Premise LTS Releases:** Customers can plan migrations from their current LTS version to a newer LTS version, typically allowing for overlap to ensure continuous long-term support. Upgrades from a Feature Release to an LTS Release are also supported.
*   Detailed upgrade guides and compatibility notes will be provided with each release.
