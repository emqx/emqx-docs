---
# 编写日期
date: 2021-05-14 13:22:18
# 作者 Github 名称
author: wivwiv
# 关键字
keywords:
# 描述
description:
# 分类
category: 
# 引用
ref:
---

# End of life dates

## Summary

EMQX Enterprise Edition will provide an 18-month maintenance cycle for the major release of the product from the release date, and we will continue to maintain the last minor release of the last two major release branches.

## Versions

- Major releases, such as 3.0.0, 4.0.0, 5.0.0, and 6.0.0, provide an opportunity to introduce non-backward compatible features.
- Minor releases, such as 4.1.0 and 4.2.0, allow us to add new features.
- Maintenance versions, such as 4.1.1 and 4.1.2, are only used to fix problems. Maintenance activities occur on all releases but are primarily used to determine how long maintenance is provided for a particular branch of code through a tributary release of a minor release (for example, 4.1.x). Active maintenance on minor releases means that we fix some bugs and migrate some fixes back to the code branch.

## Maintenance policies

Our goal is to maintain the latest minor version of the current major release, as well as the latest minor version of the previous major release. We have observed that some users frequently upgrade to keep up with our version tributaries. These users can always use the latest minor tributary release and get fixes through the maintenance release they choose to deploy. For example, these users can keep up with our EMQX releases: 4.0.0, 4.0.1, 4.2.0, etc.

We know that not all users will upgrade soon after we release a new version. For the convenience of users in this category, we will continue to maintain the latest minor release of the last major release. In the case of EMQX 3.x, we will continue to provide maintenance for the 3.x.x series. In this way, users in this category can make only a few minor changes to the software they are currently running to complete the fix. Our maintenance of the most recent minor release will continue until the next major release. For example, our maintenance of EMQX 3.x.x will continue until the official release of EMQX 5.0.0. After the release of EMQX 5.0.0, we will continue to maintain the recently released 4.x series and begin to maintain the 5.0. X minor series, followed by the 5.1.x minor series, and the 5.2.x minor series.

We sometimes migrate fixes backward to other tributary versions of minor releases. For example, when multiple branches have the potential for a serious security breach, we make a prudent decision to move backward, but we want to keep that scenario to a minimum.

## Maintenance table

The following table is based on the above policy. Occasionally, if we release a new minor release after a new major release,  we will update the table below to implement the policy above.

| **Version** | **Release Date** | **EOL Date** | **Maintenance until** |
| :---------- | :--------------- | :----------- | :-------------------- |
| 3.4.x       | 2019-12-02       | 2021-06-30   | 5.0.0                 |
| 4.0.x       | 2020-01-18       | 2021-07-17   | 4.1.0                 |
| 4.1.x       | 2020-07-18       | 2022-01-17   | 4.2.0                 |
| 4.2.x       | 2020-10-13       | 2022-04-12   | 4.3.0                 |
| 4.3.x       | 2021-05-19       | 2022-11-18   | 6.0.0                 |