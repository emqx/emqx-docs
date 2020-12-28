# EMQ X Documentation Contributing Guide

We welcome your participation to help make the documentation better!

When you offer feedback, questions, edits, or new content, you help us, the projects you work on, and the EMQ X open source community.


## Table of Contents

- [Documentation Writing Guide](#documentation-writing-guide)
- [How to contribute](#how-to-contribute)
  - [Feedback questions](#feedback-questions)
  - [Online editing](#online-editing)
  - [Local editing](#local-editing)
- [PR automatic check](#pr-automatic-check)
- [How to get help](#how-to-get-help)


## Documentation Writing Guide

To ensure consistency throughout all EMQ documentation, we ask that all contributors reference our [Documentation Writing Guide](./DOCS-WRITING-GUIDE-EN.md). This guideline provides detailed instructions on document **directory configuration** and **Markdown writing specifications**.


## How to contribute

EMQ X's documentation will be published at [https://docs.emqx.io/](https://docs.emqx.io/). You can contribute documentation via the followings methods.

> **Note:** The following methods require a GitHub account to operate. If you do not have a GitHub account, please click on the [GitHub registration page](https://github.com/join) to create one.

### Feedback questions

Clicking on `Edit this page` or `Request docs changes` on the top right corner of each page on the documentation site [https://docs.emqx.io/](https://docs.emqx.io/) will take you directly to the Edit or Issue pages on the GitHub site, which means you don't need to know much about Git or even Markdown.

You can also directly access this page [https://github.com/emqx/emqx-docs/issues](https://github.com/emqx/emqx-docs/issues) of the documentation repository to submit an Issue for feedback questions of documentation.

> This is similar to clicking **Request doc changes** on a published docs page, but if you manually file an issue you need to fill in links to the related pages.

![edit-online-cn](./assets/edit-online-en.jpg)

### Online editing

#### Fork

As you can see above, when you click on `Edit this page` in the top right corner of the documentation site, you will be taken to the following page on GitHub, follow the instructions, and click `Fork this repository`.

![github-fork](./assets/github-fork.jpg)

#### Commit

Click `Fork this repository` to jump to the document editing page and then you can start editing your document. After completing editing, you need to enter the title and description of this submission at the bottom of the page to complete your submission.

> You need to ensure that the title and description clearly describe what you are modifying.

![github-commit](./assets/github-commit.jpg)

#### Pull request

Click on `Propose changes` above and you will be redirected to the following page, click on `Create pull request` to submit.

![github-pr](./assets/github-pr.jpg)

After you have jumped to the following page, confirm the pull request information and click `Create pull request` again to complete this submission.

![github-pr-confirm](./assets/github-pr-confirm.jpg)

### Local editing

Local editing requires contributors to have a good foundation in Git. We recommend that first-time contributors directly use the [Online editing](#online-editing) method above to contribute documents.

1. Open the document repository [https://github.com/emqx/emqx-docs](https://github.com/emqx/emqx-docs) for Fork.

2. Clone the forked repository into local and then go to the local directory and add the upstream repository.

   ```
   git remote add upstream https://github.com/emqx/emqx-docs.git
   ```

3. Determine the branch of the contributed document and corresponding rules: latest correspond to develop branch, v4.1 correspond to release-4.1, v4.0 correspond to release-4.0, and so on.

4. Switch to the corresponding branch to edit, and commit to the Fork repository when you are done.

5. Create a Pull request from Fork's repository to the upstream repository.


## PR automatic check

Document projects will be automatically checked according to the rules in the [Documentation Writing Guide](./DOCS-WRITING-GUIDE-EN.md), and only the PR that passes the check will be merged. If you encounter a `markdownlint check` failure when submitting PR, the error message will indicate which line in which file is the problem, so please follow the instructions to modify and update the PR.


## How to get help

If you encounter any problems when contributing to the documentation, you can contact us for getting help in the following methods.

- Slack: [https://slack-invite.emqx.io/](https://slack-invite.emqx.io/)

- Submit the GitHub Issue directly: [https://github.com/emqx/emqx-docs/issues/new](https://github.com/emqx/emqx-docs/issues/new).

