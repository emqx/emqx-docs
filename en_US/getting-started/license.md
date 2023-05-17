# Work with License

EMQX Enterprise is the commercial version of EMQX, which requires a license certificate (License) when using it.

As part of the installation package, EMQX Enterprise already includes a trial license, which is valid for a maximum of 10 concurrent client connections. However, if you want to satisfy higher concurrent demands, you need a formal license.

This chapter guides you through the process of obtaining a license and importing it into EMQX.

## Apply for a License

To apply for a license, contact your EMQ sales representative or fill out the contact information on our [Contact Us](https://www.emqx.com/en/contact?product=emqx&channel=apply-Licenses) page to apply for a license. Our sales representative will contact you as soon as possible.

If you prefer to try EMQX Enterprise before purchasing, you can apply for a trial license on our [trial license application page](https://www.emqx.com/en/apply-licenses/emqx) and the license file will be sent to your emailbox immediately:

- The license is valid for 15 days;
- The license supports 10,000 concurrent connection lines;

If you want to extend the trial period or request support for a higher number of concurrent users, contact our sales department.

## Import License to EMQX

You can upload your License file with EMQX Dashboard or configuration file.

When working with the configuration file, update the license path to the license file in the configuration file: modify `license.file` in the `etc/license.conf` file, and make sure that the license file is indeed in the updated path and EMQX Enterprise has read permission to it. Then, start EMQX Enterprise. 

Or on EMQX Dashboard, on the **Monitor** page, click the **Update License** button on the page bottom. 

### Update License File

If the running EMQX cluster needs to update the license file, you can use the command to directly update the license file without restarting any nodes. 

```
emqx_ctl license reload [path of the license file]
```

 It should be noted that the certificate loaded by the `emqx_ctl license reload` command will be applied to the whole EMQX cluster, and it will be saved in EMQX's data directory under the `licenses` subdirectory (i.e.: `data/licenses/emqx.lic`) in each node.  After restart, this new license file will be loaded and applied.