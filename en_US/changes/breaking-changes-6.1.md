# Incompatible Changes in EMQX 6.1

## 6.1.0

- [#16368](https://github.com/emqx/emqx/pull/16368) The internal regular expression engine has been upgraded to PCRE2, providing improved matching performance and stricter syntax enforcement.

  If you use the `regex_match`, `regex_replace`, or `regex_extract` functions in Rule Engine SQL, some existing regular expressions that relied on lenient or undefined behavior may no longer compile or match as expected.

  **Key changes to be aware of include**:

  - **Stricter escaping rules**: Invalid or unnecessary escape sequences that were previously ignored are now treated as errors.
    - **Broken**: `[\w-\.]`, escaping `.` inside a character class is unnecessary and no longer accepted; only metacharacters require escaping.
    - **Broken**: `\x` without valid hexadecimal digits (for example, `\xGG`) now causes a compilation error instead of being interpreted as a literal `x`.
  - **Stricter group name validation**: Regular expressions with duplicate or empty named capture groups are no longer permitted.

  **Action required:** Review and validate all Rule Engine SQL definitions that use regular expressions. For complex patterns, verify compatibility with a PCRE2-compliant tester (most online regex tools support PCRE2) or test thoroughly in a staging environment before upgrading.