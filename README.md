# EMQX Documentation

[![Slack](https://img.shields.io/badge/Slack-EMQ%20X-39AE85?logo=slack)](https://slack-invite.emqx.io/)
[![Twitter](https://img.shields.io/badge/Twitter-EMQ-1DA1F2?logo=twitter)](https://twitter.com/EMQTech)
[![Reddit](https://img.shields.io/badge/Reddit-EMQ%20X-orange?logo=reddit)](https://www.reddit.com/r/emqx/)

[![The best IoT MQTT open source team looks forward to your joining](https://static.emqx.net/images/github_readme_en_bg.png)](https://www.emqx.com/en/careers)

English | [简体中文](./README-CN.md)

---

Welcome to the repo for [EMQX](https://github.com/emqx/emqx) documentation. This is the source for [https://docs.emqx.io/en/broker/latest/](https://docs.emqx.io/en/broker/latest/).

EMQX is a fully open-source, highly scalable, highly available distributed MQTT messaging broker for IoT, M2M, and Mobile applications that can handle tens of millions of concurrent clients.

For more information about EMQX, please access [EMQ website](https://www.emqx.com/en).

## Contributing

If you find EMQX documentation issues, please create an Issue to let us know or directly create a Pull request to help fix or update it. Our docs are completely open-source, and we sincerely appreciate contributions from our community!

See [EMQX Documentation Contributing Guide](./CONTRIBUTING-EN.md) to become a contributor!


## Preview

```sh
# for community
./preview.sh ce 8080

# for enterprise
./preview.sh ee 8080
```

Now, open <http://localhost:8080/docs/en/latest/> or <http://localhost:8080/en/enterprise/latest/>, if `directory.json` and `directory_ee.json` has been updated, you can re-run the above command to update the docs.

## Release a New Version

Both community and enterprise edition documents are managed in the same repo and same `release-Major.Minor` branch.

Community and Enterprise may have different content so they may release at different pace.

### Cut a new release for community edition

```sh
NEW_TAG="$(./cut-release.sh ce)"
git push origin "${NEW_TAG}"
```

### Cut a New Release for Enterprise Edition

```sh
NEW_TAG="$(./cut-release.sh ee)"
git push origin "${NEW_TAG}"
```

## Community

You can reach the EMQ community and developers via the following channels.

- [Slack](https://slack-invite.emqx.io/)
- [Twitter](https://twitter.com/EMQTech)
- [Facebook](https://www.facebook.com/emqxmqtt)
- [Reddit](https://www.reddit.com/r/emqx/)
- [Blog](https://emqx.medium.com)

