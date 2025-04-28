# Incompatible Changes in EMQX 5.9

## v5.9.0

- [#14957](https://github.com/emqx/emqx/pull/14957) Plugin subsystem now respects the result of the plugin's `on_config_changed` callback.

  The callback is called before updating the config. If the callback does not return `ok`, the config is not updated and a error is returned to the dasboard.

- [#14773](https://github.com/emqx/emqx/pull/14773) Rate limiting configuration options have been changed.

  - This change is incompatible with versions prior to 5.1.0
  - This change is also incompatible with manually modified limiter configurations that use structures from versions prior to 5.1.0
  - The undocumented endpoint `/configs/limiter` has been removed

- [#14703](https://github.com/emqx/emqx/pull/14703) Changed the maximum allowed value for `force_shutdown.max_heap_size` to `128GB`.


