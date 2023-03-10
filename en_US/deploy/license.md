# Work with license

EMQX Enterprise is the commercial version of EMQX, which requires a license certificate (License) when using it.

As part of the installation package, EMQX Enterprise already includes a trial license, which is valid for a maximum of 100 concurrent client connections. However,  if you wish to satisfy higher concurrent demands, you will need a formal license. 

This chapter will guide you through the process of obtaining a license and importing it into EMQX.

## Apply for a License

To apply for a license, please contact your EMQ sales representative or fill out the contact information on our [Contact Us](https://www.emqx.com/en/contact?product=emqx&channel=apply-Licenses) page to apply for a license. Our sales representative will contact you as soon as possible. 

If you prefer to try EMQX Enterprise before purchasing, you can apply for a trial license on our [trial license application page](https://www.emqx.com/en/apply-licenses/emqx) and the license file will be sent to your emailbox immediately:

- The license is valid for 15 days;
- The license supports 10,000 concurrent connection lines;

If you wish to extend the trial period or request support for a higher number of concurrent users, please contact our sales department.

:::tip

As EMQX Enterprise 5.0 uses a different license format, please ensure that the version you are applying for is compatible. For EMQX Enterprise customers who want to upgrade to version 5.0, please contact your sales representative.

:::

## Import License to EMQX

You can upload your License file with EMQX Dashboard or configuration file.

### Import with Dashboard

1. On EMQX Dasboard, click **Dashboard** on the left navigation tree. You can check the current License Information at the bottom of the page, where you can check the **License usage** and **Expire At** of the current license file. <!-- 需要界面词 -->
2. Click the **Update License** button, and paste your License Key in the popup dialog box, and click Submit. 
3. The page data will automatically refresh following your submission, and please confirm that the new license file has taken effect.

<!-- TODO 发版后提供截图 -->

### Import with configuration file

You can also configure the license file with the configuration file. After the configuration, you can run `emqx_ctl license reload` in [EMQX command line tool](../admin/cli.md) to reload the license. 

```bash
license {
  key = "..."
}
```

After execution, you can run `emqx_ctl license info` to confirm that the new license file has taken effect.

<!-- 您也可以通过环境变量 `EMQX_LICENSE__KEY` 变量名设置您的 License。TODO 确认是否可以 reload -->