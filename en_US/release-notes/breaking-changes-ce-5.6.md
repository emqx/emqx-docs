# Incompatible Changes in EMQX 5.6

## v5.6.0

- [#12576](https://github.com/emqx/emqx/pull/12576) Starting from 5.6, the "Configuration Manual" document will no longer include the `bridges` config root.

  A `bridge` is now either `action` + `connector` for egress data integration, or `source` + `connector` for ingress data integration.
  Please note that the `bridges` config (in `cluster.hocon`) and the REST API path `api/v5/bridges` still works, but considered deprecated.

- [#12634](https://github.com/emqx/emqx/pull/12634) Triple-quote string values in HOCON config files no longer support escape sequence.

  The detailed information can be found in [this pull request](https://github.com/emqx/hocon/pull/290).
  Here is a summary of the impact on EMQX users:

  - EMQX 5.6 is the first version to generate triple-quote strings in `cluster.hocon`,
    meaning for generated configs, there is no compatibility issue.
  - For user hand-crafted configs (such as `emqx.conf`) a thorough review is needed
    to inspect if escape sequences are used (such as `\n`, `\r`, `\t` and `\\`), if yes,
    such strings should be changed to regular quotes (one pair of `"`) instead of triple-quotes.
