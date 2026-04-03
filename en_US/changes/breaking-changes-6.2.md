# Incompatible Changes in EMQX 6.2

## 6.2.0

- [#16589](https://github.com/emqx/emqx/pull/16589) The jq language in the Rule Engine runtime has been upgraded from version 1.6.1 to 1.8.1, introducing several subtle incompatibilities. These are unlikely to affect your deployment but are documented here for completeness.

  - **Providing an empty string as a jq program is now considered an error**: use `"."` instead. ([jq#2790](https://github.com/jqlang/jq/pull/2790))

  - **String functions now use code point indices**: `indices/1`, `index/1`, and `rindex/1` functions now use code point indices instead of byte indices; use `utf8bytelength/0` to get byte index if needed. ([jq#3065](https://github.com/jqlang/jq/pull/3065))

  - **`tonumber/0` rejects numbers with leading or trailing whitespace**: use `trim/0` to remove leading and trailing whitespace before calling `tonumber/0`. ([jq#3055](https://github.com/jqlang/jq/pull/3055), [jq#3195](https://github.com/jqlang/jq/pull/3195))

  - **`last(empty)` behavior changed**: `last(empty)` now yields no output values, consistent with `first(empty)`. ([jq#3179](https://github.com/jqlang/jq/pull/3179))

  - **`limit/2` errors on negative count**, instead of silently accepting it. ([jq#3181](https://github.com/jqlang/jq/pull/3181))

  - **Tcl-style multiline comments supported**: this may subtly affect parsing of existing code. ([jq#2989](https://github.com/jqlang/jq/pull/2989))

  - **Decimal number conversion changed**: Decimal numbers are now converted to binary64 (double) instead of decimal64, making jq behave more like the JSON specification and similar to other languages. ([jq#2949](https://github.com/jqlang/jq/pull/2949))

  - **`nth/2` emits empty on index out of range**, instead of erroring. ([jq#2674](https://github.com/jqlang/jq/pull/2674))

  - **String multiplication by 0 or less than 1**: now emits an empty string instead of the original string. ([jq#2142](https://github.com/jqlang/jq/pull/2142))
