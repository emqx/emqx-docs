# End of Life Dates

## Summary

EMQX Enterprise Edition follows an 18-month maintenance cycle for each major release, starting from its release date. We also maintain the latest minor release of the last two major release branches.

## Versions

- Major releases, such as 3.0.0, 4.0.0, 5.0.0, and 6.0.0, introduce new features that may not be backward compatible.
- Minor releases, such as 5.1.0 and 5.2.0, add new features while maintaining backward compatibility.
- Maintenance versions, such as 5.7.1 and 5.7.2, focus on fixing issues. Maintenance activities occur on all releases but are primarily used to determine how long maintenance is provided for a particular branch of code through a tributary release of a minor release (for example, 5.7.x). Active maintenance on minor releases means that we fix some bugs and migrate some fixes back to the code branch.

## Maintenance Policies

Our goal is to maintain the latest minor version of the current major release, as well as the latest minor version of the previous major release. We have observed that some users frequently upgrade to keep up with our version tributaries. These users can always use the latest minor tributary release and get fixes through the maintenance release they choose to deploy. For example, these users can keep up with our EMQX releases: 5.7.0, 5.7.1, 5.7.2, etc.

We understand that not all users will upgrade immediately after a release a new release. For the convenience of these users, we continue to maintain the latest minor release of the last major release. In the case of EMQX 4.x, we maintain the 4.x.x series, allowing users to make only a few minor changes to the software they are currently running to apply fixes. Our maintenance of the most recent minor release will continue until the next major release. For example, our maintenance of EMQX 4.x.x will continue until the official release of EMQ X 6.0.0. After the release of EMQX 6.0.0, we will maintain the recently released 5.x series and begin to maintain the 6.0. X minor series, followed by the 6.1.x minor series, and the 6.2.x minor series.

Occasionally, we may backport fixes to other tributary versions of minor releases. For example, if multiple branches are susceptible to a severe security vulnerability, we make a prudent decision to move backward, although we aim to minimize such occurrences..

## Maintenance Table

The following table reflects our maintenance policy. Occasionally, if we release a new minor release after a new major release,  we will update the table below accordingly.

| **Version** | **Release Date** | **EOL Date** | **Maintenance Ends with** |
| :---------- | :--------------- | :----------- | :------------------------ |
| 3.4.x       | 2019-12-02       | 2021-06-30   | 5.0.0                     |
| 4.0.x       | 2020-01-18       | 2021-07-17   | 4.1.0                     |
| 4.1.x       | 2020-07-18       | 2022-01-17   | 4.2.0                     |
| 4.2.x       | 2020-10-13       | 2022-04-12   | 4.3.0                     |
| 4.3.x       | 2021-05-19       | 2022-11-18   | 4.4.0                     |
| 4.4.x       | 2021-12-21       | 2023-06-20   | 6.0.0                     |
| 5.0.x       | 2023-02-03       | 2024-08-02   | 5.1.0                     |
| 5.1.x       | 2023-06-21       | 2024-12-20   | 5.2.0                     |
| 5.2.x       | 2023-09-07       | 2025-03-06   | 5.3.0                     |
| 5.3.x       | 2023-09-29       | 2025-03-28   | 5.4.0                     |
| 5.4.x       | 2023-12-23       | 2025-06-22   | 5.5.0                     |
| 5.5.x       | 2024-02-01       | 2025-07-31   | 5.6.0                     |
| 5.6.x       | 2024-03-28       | 2025-09-27   | 5.7.0                     |
| 5.7.x       | 2024-05-27       | 2025-11-26   | 5.8.0                     |
| 5.8.x       | 2024-08-28       | 2026-02-27   | 5.9.0                     |