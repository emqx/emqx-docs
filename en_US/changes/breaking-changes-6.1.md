# Incompatible Changes in EMQX 6.1

<<<<<<< HEAD
=======
## 6.1.2

- [#17157](https://github.com/emqx/emqx/pull/17157) Introduced a new Rule Engine configuration, `rule_engine.limit_selects_in_namespace`, whose default value is `true`. When enabled, rules will only trigger on messages published by clients on the same namespace as the rule itself.

- [#17325](https://github.com/emqx/emqx/pull/17325) Removed the hot-upgrade REST API endpoints (`/api/v5/relup/*`). Hot-upgrade is now operated exclusively through the `emqx ctl relup` CLI on each node, with no Dashboard surface.

  Place the target release tarball and its `.sha256` sidecar (same base name, same directory) anywhere readable by the EMQX process. Run `emqx ctl relup upgrade <TarballPath>` on each node to apply the upgrade; the target version is read from `releases/emqx_vars` (`REL_VSN`) inside the tarball.

>>>>>>> origin/release-6.1
## 6.1.0

- [#16368](https://github.com/emqx/emqx/pull/16368) The internal regular expression engine has been upgraded to PCRE2, providing improved matching performance and stricter syntax enforcement.

  If you use the `regex_match`, `regex_replace`, or `regex_extract` functions in Rule Engine SQL, some existing regular expressions that relied on lenient or undefined behavior may no longer compile or match as expected.

  **Key changes to be aware of include**:

  - **Stricter escaping rules**: Invalid or unnecessary escape sequences that were previously ignored are now treated as errors.
    - **Broken**: `[\w-\.]`, escaping `.` inside a character class is unnecessary and no longer accepted; only metacharacters require escaping.
    - **Broken**: `\x` without valid hexadecimal digits (for example, `\xGG`) now causes a compilation error instead of being interpreted as a literal `x`.
  - **Stricter group name validation**: Regular expressions with duplicate or empty named capture groups are no longer permitted.

  **Action required:** Review and validate all Rule Engine SQL definitions that use regular expressions. For complex patterns, verify compatibility with a PCRE2-compliant tester (most online regex tools support PCRE2) or test thoroughly in a staging environment before upgrading.