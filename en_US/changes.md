# Changes 

## Version 3.2.7 

*Release Date: 2019-12-03*

EMQ X 3.2.7 is now available. This version resupports to configure the default ` username ` and ` clientid ` through the configuration file. 

### emqx-auth-username (plugin) 

Enhancements: 

  * Resupport to configure the default ` username ` through the configuration file 

Github PR: [ emqx/emqx-auth-username#127 ](https://github.com/emqx/emqx-auth-username/pull/127)




### emqx-auth-clientid (plugin) 

Enhancements: 

  * Resupport to configure the default ` clientid ` through the configuration file 

Github PR: [ emqx/emqx-auth-clientid#123 ](https://github.com/emqx/emqx-auth-clientid/pull/123)




## Version 3.2.6 

*Release Date: 2019-11-23*

EMQ X 3.2.6 is now available. This version focuses on feature improvements and bug fixes. 

### emqx (major) 

Bug fixes: 

  * Fix the issue that messages maybe disordered when forwarding messages to remote nodes via ` gen_rpc `

Github PR: [ emqx/emqx#3049 ](https://github.com/emqx/emqx/pull/3049)

  * Fix ` emqx ` crash caused by the crash of auth plugin 

Github PR: [ emqx/emqx#3048 ](https://github.com/emqx/emqx/pull/3048)




## Version 4.0-beta.4 

*Release Date: 2019-11-18*

EMQ X 4.0-beta.4 is now available. This version focuses on feature improvements and bug fixes. 

### emqx (major) 

Enhancements: 

  * Any Client that detects flapping will be banned 

Github PR: [ emqx/emqx#3033 ](https://github.com/emqx/emqx/pull/3033)

  * Improve the ` emqx_vm ` module and update test cases 

Github PR: [ emqx/emqx#3034 ](https://github.com/emqx/emqx/pull/3034)




### emqx-management (plugin) 

Enhancements: 

  * Update banned API 

Github PR: [ emqx/emqx-management#141 ](https://github.com/emqx/emqx-management/pull/141)




Bug fixes: 

  * Fix some bad return values 

Github PR: [ emqx/emqx-management#142 ](https://github.com/emqx/emqx-management/pull/142)




### minirest (plugin) 

Bug fixes: 

  * Add error handling and log 

Github PR: [ emqx/minirest#20 ](https://github.com/emqx/minirest/pull/20)




### esockd (dependency) 

Enhancements: 

  * Adjust some interfaces and add test cases 

Github PR: [ emqx/esockd#124 ](https://github.com/emqx/esockd/pull/124)




### ekka (dependency) 

Enhancements: 

  * Adjust some interfaces and add test cases 

Github PR: [ emqx/ekka#67 ](https://github.com/emqx/ekka/pull/67)




## Version 3.2.5 

*Release Date: 2019-11-15*

EMQ X 3.2.5 is now available. This version focuses on bug fixes. 

### emqx-rule-engine (plugin) 

Bug fixes: 

  * Support rule SQL: FOREACH/DO/INCASE 

Github Commit: [ emqx/emqx-rule-engine#a962e3 ](https://github.com/emqx/emqx-rule-engine/commit/a962e364cfde9a7f9bbde3d4d6613625b8d00ce7)

  * Support rule SQL: CASE/WHEN 

Github Commit: [ emqx/emqx-rule-engine#40e68e ](https://github.com/emqx/emqx-rule-engine/commit/40e68e9607198613cc93d001488d40b2bfb4f23e)

  * Support comparing atom to binary in WHERE SQL clause 

Github Commit: [ emqx/emqx-rule-engine#b240cc ](https://github.com/emqx/emqx-rule-engine/commit/b240cc0434815bafb5cfcd366692257336d26e8c)

  * Fix column validation failure in select and foreach 

Github Commit: [ emqx/emqx-rule-engine#6a1267 ](https://github.com/emqx/emqx-rule-engine/commit/6a1267cb1530d00972899ecb3abb7a3220e28175)

  * Fix race-conditions when re-build rules 

Github Commit: [ emqx/emqx-rule-engine#af8967 ](https://github.com/emqx/emqx-rule-engine/commit/af8967793d4f554134955c620d9e31b8c3876445)

  * Fix incorrect publish message by adding default flags in republish action 

Github Commit: [ emqx/emqx-rule-engine#60e45c ](https://github.com/emqx/emqx-rule-engine/commit/60e45c28596a6cb42437043fbba5509502a3cf41)




### minirest (plugin) 

Bug fixes: 

  * Fix missing error data in log 

Github PR: [ emqx/minirest#20 ](https://github.com/emqx/minirest/pull/20)




### emqx-web-hook (plugin) 

Bug fixes: 

  * Fix bad match 

Github Commit: [ emqx/emqx-web-hook#3dd041 ](https://github.com/emqx/emqx-web-hook/commit/3dd041afaf39eabe71ab473648d57f4b55735224)




## Version 4.0-beta.3 

*Release Date: 2019-11-01*

EMQ X 4.0-beta.3 is now available. This version mainly improves test coverage, and fixes bugs. 

Bug fixes: 

  * Fix message out-of-order issues when forwarding across clusters 

Github PR: [ emqx/emqx#3000 ](https://github.com/emqx/emqx/pull/3000)




### emqx-management (plugin) 

Enhancements: 

  * REST API supports IPv6 

Github PR: [ emqx/emqx-management#135 ](https://github.com/emqx/emqx-management/pull/135)




Bug fixes: 

  * Fix the issue that encoded URI isn't handled correctly 

Github PR: [ emqx/emqx-management#137 ](https://github.com/emqx/emqx-management/pull/137)




### emqx-dashboard (plugin) 

Enhancements: 

  * Support for IPv6 access to Dashbaord 

Github PR: [ emqx/emqx-dashboard#162 ](https://github.com/emqx/emqx-dashboard/pull/162)




### emqx-delayed-publish (plugin) 

Bug fixes: 

  * Fix the issue that the plugin can only start one in cluster 

Github PR: [ emqx/emqx-delay-publish#50 ](https://github.com/emqx/emqx-delay-publish/pull/50)

  * Fix the issue that delayed messages are published disorderly, thanks contribution of [ soldag ](https://github.com/soldag)

Github PR: [ emqx/emqx-delay-publish#49 ](https://github.com/emqx/emqx-delay-publish/pull/49)

Github Issue: [ emqx/emqx-delay-publish#15 ](https://github.com/emqx/emqx-delay-publish/issues/15)




## Version 3.2.4 

*Release Date: 2019-10-28*

EMQ X 3.2.4 is now available. This version mainly adds IPv6 support for Dashbaord and REST APIs, and fixes some bugs. 

Bug fixes: 

  * Fix the issue that 'max_subscriptions' don't working 

Github PR: [ emqx/emqx#2922 ](https://github.com/emqx/emqx/pull/2922)

Github Issue: [ emqx/emqx#2908 ](https://github.com/emqx/emqx/issues/2908)




### emqx-auth-mysql (plugin) 

Bug fixes: 

  * Gets the value corresponding to placeholders more securely 

Github PR: [ emqx/emqx-auth-mysql#180 ](https://github.com/emqx/emqx-auth-mysql/pull/180)

Github Issue: [ emqx/emqx#2937 ](https://github.com/emqx/emqx/issues/2937)




### emqx-dashboard (plugin) 

Enhancements: 

  * Support for IPv6 access to Dashbaord 

Github PR: [ emqx/emqx-dashboard#161 ](https://github.com/emqx/emqx-dashboard/pull/161)




### emqx-management (plugin) 

Enhancements: 

  * REST API supports IPv6 

Github PR: [ emqx/emqx-management#134 ](https://github.com/emqx/emqx-management/pull/134)




### emqx-delay-publish (plugin) 

Bug fixes: 

  * Fix the issue that delayed messages are published disorderly, thanks contribution of [ soldag ](https://github.com/soldag)

Github PR: [ emqx/emqx-delay-publish#49 ](https://github.com/emqx/emqx-delay-publish/pull/49)

Github Issue: [ emqx/emqx-delay-publish#15 ](https://github.com/emqx/emqx-delay-publish/issues/15)




### emqx-rule-engine (plugin) 

Enhancements: 

  * Improved the SQL syntax for decoding the payload of JSON format 

Github Repository: [ emqx/emqx-rule-engine ](https://github.com/emqx/emqx-rule-engine)




## Version 4.0-beta.2 

*Release Date: 2019-10-14*

EMQ X 4.0-beta.2 is now available. This version focuses on bug fixes and continues to optimize the internal module design. 

Bug fixes: 

  * Fix the issue that failure of SSL handshake could cause crash 

Github PR: [ emqx/emqx#2963 ](https://github.com/emqx/emqx/pull/2963)

  * Check topic level for PUBLISH packet 

Github PR: [ emqx/emqx#2964 ](https://github.com/emqx/emqx/pull/2964)




### emqtt (plugin) 

Enhancements: 

  * Implement the command line interface 

Github PR: [ emqx/emqtt#91 ](https://github.com/emqx/emqtt/pull/91)




### emqx-sn (plugin) 

Bug fixes: 

  * Upgrade the MQTT-SN plugin to version 4.0 

Github PR: [ emqx/emqx-sn#145 ](https://github.com/emqx/emqx-sn/pull/145)




### emqx-coap (plugin) 

Bug fixes: 

  * Upgrade the CoAP plugin to version 4.0 

Github Commit: [ emqx/emqx-coap#c7c175 ](https://github.com/emqx/emqx-coap/commit/c7c17540c1248dcdd402b41323c23a211e8292fc) , [ emqx/emqx-coap#9b8ede ](https://github.com/emqx/emqx-coap/commit/9b8ede093cfc3b7211663520e496c579c11611f6)




## Version 4.0-beta.1 

*Release Date: 2019-09-30*

EMQ X 4.0-beta.1 is now available. We redesigned the internal modules to increase throughtput dramatically. 

## Version 3.2.3 

*Release Date: 2019-09-16*

EMQ X 3.2.3 is now available, and this version focuses on bug fixes. 

Bug fixes: 

  * Fix the issue that the alarm of CPU usage triggered abnormally when emqx container is running 

GitHub Commit: [ emqx/emqx#9cdaa7 ](https://github.com/emqx/emqx/commit/9cdaa71a66c44d6bfd7606f8e64bc6670f619cdf)

  * Fix the issue that the mechanism of message expiration doesn't take effect 

Github Commit: [ emqx/emqx#31671f ](https://github.com/emqx/emqx/commit/31671f5ee5516e04ca6c648679f030b790c84fd9)

  * Fix the issue thar placeholder like '%c' in mountpoint doesn't take effect 

Github Commit: [ emqx/emqx#58ba22 ](https://github.com/emqx/emqx/commit/58ba22dfc79ce81ac74fffae60a624d2238585ca)




### emqx-dashboard (plugin) 

Bug fixes: 

  * Fix the issue that the function of SSL is unavailable 

Github Commit: [ emqx/emqx-dashboard#272a42 ](https://github.com/emqx/emqx-dashboard/commit/272a42b5ac7b28f52e5e71fae540e47278fac9d5)




## Version 3.2.2 

*Release Date: 2019-08-03*

EMQ X 3.2.2 is now available, and this version focuses on bug fixes. 

Enhancements: 

  * Extends configurations of ` gen_rpc `

Github PR: [ emqx/emqx#2732 ](https://github.com/emqx/emqx/pull/2732)




### emqx-rule-engine (plugin) 

Bug fixes: 

  * Fix the issue testing URL connectivity 

Github PR: [ emqx/emqx-rule-engine#88 ](https://github.com/emqx/emqx-rule-engine/pull/88)




### emqx-dashboard (plugin) 

Enhancements: 

  * Add help page 



### ekka (dependency) 

Bug fixes: 

  * Fix the issue that releasing lock could causes crash 

Github PR: [ emqx/ekka#60 ](https://github.com/emqx/ekka/pull/60)




## Version 3.2.1 

*Release Date: 2019-07-20*

EMQ X 3.2.1 is now available. We've enhanced performance and fixed bugs. 

Enhancements: 

  * Optimize the performance of ` gen_rpc `

Github PR: [ emqx/emqx#2694 ](https://github.com/emqx/emqx/pull/2694)

  * Support using hostname to automatically discover k8s cluster 

Github PR: [ emqx/emqx#2699 ](https://github.com/emqx/emqx/pull/2699)

  * Change the default uptime heartbeat interval to 30s 

Github PR: [ emqx/emqx#2696 ](https://github.com/emqx/emqx/pull/2696)




Bug fixes: 

  * Fix the issue that encouter crash when Websocket sessions go offline abnormally 

Github PR: [ emqx/emqx#2697 ](https://github.com/emqx/emqx/pull/2697)

  * Fix the issue that ws_channel is still online when session closed on exception 

Github PR: [ emqx/emqx#2704 ](https://github.com/emqx/emqx/pull/2704)




### emqx-rule-engine (plugin) 

Enhancements: 

  * Improve parameters for republish action 

Github PR: [ emqx/emqx-rule-engine#81 ](https://github.com/emqx/emqx-rule-engine/pull/81)




Bug fixes: 

  * Fix the issue that fail to select payload fields using '.' 

Github PR: [ emqx/emqx-rule-engine#83 ](https://github.com/emqx/emqx-rule-engine/pull/83)




### emqx-dashboard (plugin) 

Bug fixes: 

  * Fix the issue rendering resources list incorrectly in Dashboard on Safari 

Github PR: [ emqx/emqx-dashboard#124 ](https://github.com/emqx/emqx-dashboard/pull/124) , [ emqx/emqx-dashboard#125 ](https://github.com/emqx/emqx-dashboard/pull/125) , [ emqx/emqx-dashboard#126 ](https://github.com/emqx/emqx-dashboard/pull/126)




### emqx-lwm2m (plugin) 

Enhancements: 

  * Compatible with client login using LwM2M v1.1 

Github Commit: [ emqx/emqx-lwm2m#1c03bf ](https://github.com/emqx/emqx-lwm2m/commit/1c03bf3b6a9cae7ed52f87ee219e9dd9d8824892)




### emqx-rel (build project) 

Enhancements: 

  * Support building ` emqx-rel ` with built-in rebar3 

Github PR: [ emqx/emqx-rel#394 ](https://github.com/emqx/emqx-rel/pull/394)

  * Delay EMQ X windows service auto start 

Github PR: [ emqx/emqx-rel#395 ](https://github.com/emqx/emqx-rel/pull/395)




## Version 3.2.0 

*Release Date: 2019-07-12*

EMQ X 3.2.0 is mainly for improvements of rule engine. 

### Rule Engine 

Improve rule engine and ui of dashboard, support more actions. 

### Project building 

Support rebar3 to build project. 

### MQTT Broker Bridge 

Bridging to MQTT Broker is now provided by emqx-bridge-mqtt (plugin) instead. 

### HTTP Plugin 

Support HTTPs. 

### Cluster (ekka) 

Improve stability of emqx cluster. 

### Other Plugins and Dependencies 

Fix Windows service registering issue. 

## Version 3.2-rc.3 

*Release Date: 2019-07-06*

EMQ X 3.2-rc.3 is now available. We've enhanced features and fixed bugs. 

Bug fixes: 

  * Fix [ emqx/emqx: issue#2635 ](https://github.com/emqx/emqx/issues/2635)

Github PR: [ emqx/emqx#2663 ](https://github.com/emqx/emqx/pull/2663)




### emqx-web-hook (plugin) 

Bug fixes: 

  * Fix the issue that the count of ` actions.failure ` doesn't grow 

Github PR: [ emqx/emqx-web-hook#137 ](https://github.com/emqx/emqx-web-hook/pull/137)




### emqx-bridge-mqtt (plugin) 

Enhancements: 

  * Add the option of bridge mode 

Github PR: [ emqx/emqx-bridge-mqtt#6 ](https://github.com/emqx/emqx-bridge-mqtt/pull/6)

  * Optimize the ACK mechanism for RPC messages 

  * Support for MQTT/RPC Bridge of Rule Engine caching messages to local disk queues 

  * Fix the issue that MQTT/RPC Bridge of Rule Engine can’t bridge to remote EMQ X node 

Github PR: [ emqx/emqx-bridge-mqtt#7 ](https://github.com/emqx/emqx-bridge-mqtt/pull/7)




### emqx-rule-engine (plugin) 

Enhancements: 

  * Add cluster support for rules and resource API 

Github PR: [ emqx/emqx-rule-engine#75 ](https://github.com/emqx/emqx-rule-engine/pull/75)

  * Add API for returning available columns of trigger events 

Github PR: [ emqx/emqx-rule-engine#74 ](https://github.com/emqx/emqx-rule-engine/pull/74) , [ emqx/emqx-rule-engine#77 ](https://github.com/emqx/emqx-rule-engine/pull/77)




Bug fixes: 

  * Fix the issue caused by fetching resource status timeout 

Github PR: [ emqx/emqx-rule-engine#76 ](https://github.com/emqx/emqx-rule-engine/pull/76)




### emqx-dashboard (plugin) 

Enhancements: 

  * Rules engine metrics are subdivided into nodes 

Github PR: [ emqx/emqx-dashboard#114 ](https://github.com/emqx/emqx-dashboard/pull/114)




Bug fixes: 

  * Fix bugs in resource creation 

Github PR: [ emqx/emqx-dashboard#114 ](https://github.com/emqx/emqx-dashboard/pull/114)




## Version 3.2-rc.2 

*Release Date: 2019-06-29*

EMQ X 3.2-rc.2 is mainly for bug fixes. 

Enhancements: 

  * Change the default logger level to **warning**

Github PR: [ emqx/emqx#2657 ](https://github.com/emqx/emqx/pull/2657)

  * Add history alarm list API 

Github PRs: [ emqx/emqx#2660 ](https://github.com/emqx/emqx/pull/2660) [ emqx/emqx-management#98 ](https://github.com/emqx/emqx-management/pull/98)




Bug fixes: 

  * Clean the stale sessions: 

Github PR: [ emqx/emqx#2655 ](https://github.com/emqx/emqx/pull/2655)

  * Fix the massages order when batch dispatching: 

Github PR: [ emqx/emqx#2650 ](https://github.com/emqx/emqx/pull/2650)

Thank the contributions from [ tradingtrace ](https://github.com/tradingtrace) ! 




### emqx-rule-engine (plugin) 

Enhancements: 

  * Add an action "do nothing" for debug purpose. 

Github PR: [ emqx/emqx-rule-engine#70 ](https://github.com/emqx/emqx-rule-engine/pull/70)

  * Change data type of the ` retain ` flag to integer 

Github RP: [ emqx/emqx-rule-engine#72 ](https://github.com/emqx/emqx-rule-engine/pull/72)




Bug fixes: 

  * Escape SQL reserved keyword timestamp: 

Github PR: [ emqx/emqx-rule-engine#71 ](https://github.com/emqx/emqx-rule-engine/pull/71)




### emq-bridge-mqtt (plugin) 

  * Migrate the MQTT bridge from emqx project to a separated plugin: 

Github PR: [ emqx/emqx-bridge-mqtt#2 ](https://github.com/emqx/emqx-bridge-mqtt/pull/2)




### emqx-rel (build project) 

Bug fixes: 

  * Fixed a windows service register bug: 

Github PR: [ emqx/emqx-rel#381 ](https://github.com/emqx/emqx-rel/pull/381)




## Version 3.2-rc.1 

*Release Date: 2019-06-22*

EMQ X 3.2-rc.1 is now available. We've enhanced features and fixed bugs. 

Enhancements: 

  * Support setting prefix of log message 

Github PR: [ emqx/emqx#2627 ](https://github.com/emqx/emqx/pull/2627)

  * Improve precision of the timestamp of clients' connecting and disconnecting in system messages 

Github PR: [ emqx/emqx#2641 ](https://github.com/emqx/emqx/pull/2641)

  * Optimize develop workflow and support ` make run `

Github PR: [ emqx/emqx#2644 ](https://github.com/emqx/emqx/pull/2644)




Bug fixes: 

  * Fix the issue that flapping module can't read configuration correctly 

Github PR: [ emqx/emqx#2628 ](https://github.com/emqx/emqx/pull/2628)

  * Fix the issue that unavailable ` cpu_sup:util/0 ` result in crash in Windows 

Github PR: [ emqx/emqx#2629 ](https://github.com/emqx/emqx/pull/2629)

  * Fix [ emqx/emqx: issue#2619 ](https://github.com/emqx/emqx/issues/2619)

Github PR: [ emqx/emqx#2646 ](https://github.com/emqx/emqx/pull/2646)




### emqx-rule-engine (plugin) 

Enhancements: 

  * Support fetching resource status periodically and setting alarms 

Github PR: [ emqx/emqx-rule-engine#67 ](https://github.com/emqx/emqx-rule-engine/pull/67)




### emqx-sn (plugin) 

Bug fixes: 

  * Fix the misjudgement of ` keepalive_timeout `

Github PR: [ emqx/emqx-sn#127 ](https://github.com/emqx/emqx-sn/pull/127)

  * Fix the issue that don't read ` idle_timeout ` correctly 

Github PR: [ emqx/emqx-sn#128 ](https://github.com/emqx/emqx-sn/pull/128)

  * Fix the test case 

Github PR: [ emqx/emqx-sn#130 ](https://github.com/emqx/emqx-sn/pull/130)




### emqx-auth-jwt (plugin) 

Bug fixes: 

  * Read pubkey correctly 

Github PR: [ emqx/emqx-auth-jwt#88 ](https://github.com/emqx/emqx-auth-jwt/pull/88)




### emqx-rel (build-project) 

Enhancements: 

  * Make the building more intelligent and robust 

GitHub PR: [ emqx/emqx-rel#375 ](https://github.com/emqx/emqx-rel/pull/375) , [ emqx/emqx-rel#376 ](https://github.com/emqx/emqx-rel/pull/376)




## Version 3.2-beta.3 

*Release Date: 2019-06-14*

EMQ X 3.2-beta.3 is now available. We've improved rule engine and fixed bugs. 

### EMQ X Core 

Bug fixes: 

  * Fix the issue that not checking ` Will Retain ` flag 

Github PR: [ emqx/emqx#2607 ](https://github.com/emqx/emqx/pull/2607)

  * Fix [ emqx/emqx: issue#2591 ](https://github.com/emqx/emqx/issues/2591)

Github PR: [ emqx/emqx#2615 ](https://github.com/emqx/emqx/pull/2615)

  * Remove characters limit for logging by default 

Github PR: [ emqx/emqx#2617 ](https://github.com/emqx/emqx/pull/2617)

  * Fix the issue that can’t handle fragmented tcp packet correctly 

Github PR: [ emqx/emqx#2611 ](https://github.com/emqx/emqx/pull/2611)




### emqx-rule-engine (plugin) 

Enhancements: 

  * Support metrics like count of rule matched 

Github PR: [ emqx/emqx-rule-engine#63 ](https://github.com/emqx/emqx-rule-engine/pull/63)




### emqx-management (plugin) 

Bug fixes: 

  * Fix the issue that CLI kicks websocket connection failed 

Github PR: [ emqx/emqx-management#93 ](https://github.com/emqx/emqx-management/pull/93)




## Version 3.2-beta.2 

*Release Date: 2019-06-06*

EMQ X 3.2-beta.2 is now available. We've improved rule engine and fixed bugs. 

### EMQ X Core 

Bug fixes: 

  * Fix [ emqx/emqx: issue#2553 ](https://github.com/emqx/emqx/issues/2553)

Github PR: [ emqx/emqx#2596 ](https://github.com/emqx/emqx/pull/2596)




### emqx-rule-engine (plugin) 

Enhancements: 

  * Support testing SQL in the dashboard 

Github Commit: [ emqx/emqx-rule-engine#3e7c4c ](https://github.com/emqx/emqx-rule-engine/commit/3e7c4cbe275d8f120ad8efb83fd23ee571d465db)

  * Preprocess prepared statement for better performance 

Github Commit: [ emqx/emqx-rule-engine#fa3720 ](https://github.com/emqx/emqx-rule-engine/commit/fa37205850c6efe9af5f8ca2f230e17c7de2adb4) , [ emqx/emqx-rule-engine#b00fad ](https://github.com/emqx/emqx-rule-engine/commit/b00fad45c283fa2ec3aa57353bbe161960547461)

  * Adapt rule engine to emqx cluster 

Github Commit: [ emqx/emqx-rule-engine#3da7fe ](https://github.com/emqx/emqx-rule-engine/commit/3da7fed60d92c9a994c2aed5f34509c0d0d4eff4) , [ emqx/emqx-rule-engine#4963b0 ](https://github.com/emqx/emqx-rule-engine/commit/4963b0ee3a6114ebe74b48876d25723137df14ad)

  * Support showing resource status from dashboard 

Github Commit: [ emqx/emqx-rule-engine#dd9a8d ](https://github.com/emqx/emqx-rule-engine/commit/dd9a8d4801f650c1ac888f7420f5497f7d0d6c73) , [ emqx/emqx-rule-engine#d16224 ](https://github.com/emqx/emqx-rule-engine/commit/d162246c0b630e059c21f7b36e50154f3d7832e3) , [ emqx/emqx-rule-engine#e4574c ](https://github.com/emqx/emqx-rule-engine/commit/e4574c9554d7e7d79a8ce55a6c9e4089ee00db79)

  * Support restarting resources in dashboard 

Github Commit: [ emqx/emqx-rule-engine#ccbffd ](https://github.com/emqx/emqx-rule-engine/commit/ccbffd7d5db514adf6cd20e8d139e73f80bc1c96)

  * Support check HTTP connectivity 

Github Commit: [ emqx/emqx-rule-engine#3feffc ](https://github.com/emqx/emqx-rule-engine/commit/3feffcd5a3f0da78725f1208594cea1b3273ec0b)




Bug fixes: 

  * Fix check dependency error before deleting resources 

Github Commit: [ emqx/emqx-rule-engine#3265ff ](https://github.com/emqx/emqx-rule-engine/commit/3265ffe10584f0edccc084e6f78ae035ba310c07)

  * Fix resources never destroyed 

Github Commit: [ emqx/emqx-rule-engine#58a1ce ](https://github.com/emqx/emqx-rule-engine/commit/58a1ce45e1cf96cf05481d8ed076febef0d41976)

  * Fix SQL nested put failure 

Github Commit: [ emqx/emqx-rule-engine#64776a ](https://github.com/emqx/emqx-rule-engine/commit/64776aebde1fe48c1038fba3b61f457590ab4408)




### emqx-auth-http (plugin) 

Enhancements: 

  * Support HTTPS 

Github PR: [ emqx/emqx-auth-http#133 ](https://github.com/emqx/emqx-auth-http/pull/133)




### emqx-docker (plugin) 

Bug fixes: 

  * Fix [ emqx/emqx-docker: issue#115 ](https://github.com/emqx/emqx-docker/issues/115)

Github Commit: [ emqx/emqx-docker#f3c219 ](https://github.com/emqx/emqx-docker/commit/f3c21978f5ffefd5d419bc78a1caf1ad71de9c91)




### emqx-management (plugin) 

Bug fixes: 

  * Fix the issue of reloading plugin 

Github PR: [ emqx/emqx-management#91 ](https://github.com/emqx/emqx-management/pull/91)




### ekka (deps) 

Bug fixes: 

  * Fix the issue makes  emqx_sm_locker  crash 

Github Commit: [ emqx/ekka#2d5bf2 ](https://github.com/emqx/ekka/commit/2d5bf2a1f10d84408e4b35d3e274a49f395056c3)




## Version 3.2-beta.1 

*Release Date: 2019-05-27*

EMQ X 3.2-beta.1 is now available. We've changed our build tool from erlang.mk to rebar3, and improved the rule-engine. 

### EMQ X Core 

Enhancements: 

  * Build with rebar3 

Github PR: [ emqx/emqx#2475 ](https://github.com/emqx/emqx/pull/2475) , [ emqx/emqx#2510 ](https://github.com/emqx/emqx/pull/2510) , [ emqx/emqx#2518 ](https://github.com/emqx/emqx/pull/2518) , [ emqx/emqx#2521 ](https://github.com/emqx/emqx/pull/2521)

  * Support  {active, N}  for SSL connection 

Github PR: [ emqx/emqx#2531 ](https://github.com/emqx/emqx/pull/2531)

  * Improve the behaviour of anonymous access 

Github PR: [ emqx/emqx#2355 ](https://github.com/emqx/emqx/pull/2355)

  * Speed up zone access 

Github PR: [ emqx/emqx#2548 ](https://github.com/emqx/emqx/pull/2548)




Bug fixes: 

  * Fixed a fatal error in the  emqx_sm 

Github PR: [ emqx/emqx#2559 ](https://github.com/emqx/emqx/pull/2559)

  * Fixed an error when publishing MQTT-SN, CoAP messages 

Github PR: [ emqx/emqx#2556 ](https://github.com/emqx/emqx/pull/2556)




### emqx-rule-engine (plugin) 

Enhancements: 

  * Better rule engine 

Github Repository: [ emqx/emqx-rule-engine ](https://github.com/emqx/emqx-rule-engine)




### emqx-web-hook (plugin) 

Enhancements: 

  * Add an option for encoding payload field 

Github PR: [ emqx/emqx-web-hook#119 ](https://github.com/emqx/emqx-web-hook/pull/119)




### emqx-auth-http (plugin) 

Enhancements: 

  * More opts for http request 

Github PR: [ emqx/emqx-auth-http#128 ](https://github.com/emqx/emqx-auth-http/pull/128)




### emqx-sn (plugin) 

Bug fixes: 

  * Fix wrong function call 

Github PR: [ emqx/emqx-sn#118 ](https://github.com/emqx/emqx-sn/pull/118)




## Version 3.1.2 

*Release Date: 2019-06-06*

EMQ X 3.1.2 is now available. We've fixed bugs and improved stability. 

### EMQ X Core 

Bug fixes: 

  * Fix [ emqx/emqx: issue #2595 ](https://github.com/emqx/emqx/issues/2595)

Github PR: [ emqx/emqx#2601 ](https://github.com/emqx/emqx/pull/2601)

  * Fix the issue that failed when setting the log level 

Github PR: [ emqx/emqx#2600 ](https://github.com/emqx/emqx/pull/2600)

  * Fix the issue that doesn't match the return value 

Github PR: [ emqx/emqx#2560 ](https://github.com/emqx/emqx/pull/2560)

  * Hotfix for ` emqx_sn ` and ` emqx_coap ` plugins 

Github PR: [ emqx/emqx#2556 ](https://github.com/emqx/emqx/pull/2556)




### emqx-coap (plugin) 

Bug fixes: 

  * Fix the issue that messages can't be published 

Github PR: [ emqx/emqx-coap#120 ](https://github.com/emqx/emqx-coap/pull/120)




### ekka (deps) 

Bug fixes: 

  * Fix the issue makes ` emqx_sm_locker ` crash 

Github PR: [ emqx/ekka#54 ](https://github.com/emqx/ekka/pull/54)

  * Fix the issue that k8s can't use dns cluster 

Github PR: [ emqx/ekka#53 ](https://github.com/emqx/ekka/pull/53)

  * Fix the issue that etcd cluster is unusable 

Github PR: [ emqx/ekka#52 ](https://github.com/emqx/ekka/pull/52)




## Version 3.1.1 

*Release Date: 2019-05-10*

EMQ X 3.1.1 is now available. In this version we've fixed bugs and improved stability. 

### EMQ X Core 

Enhancements: 

  * Enlarge the maximum number of characters printed by each log event 

Github PR: [ emqx/emqx#2509 ](https://github.com/emqx/emqx/pull/2509)

  * ` force_shutdown_policy ` will use a different value according to digits of system 

Github PR: [ emqx/emqx#2515 ](https://github.com/emqx/emqx/pull/2515)




Bug fixes: 

  * Configure and use ` long_gc ` 与 ` long_schedule ` correctly 

Github PR: [ emqx/emqx#2504 ](https://github.com/emqx/emqx/pull/2504) , [ emqx/emqx#2513 ](https://github.com/emqx/emqx/pull/2513)

  * Fix the issue ` suboptions/count ` not been updated 

Github PR: [ emqx/emqx#2507 ](https://github.com/emqx/emqx/pull/2507)




### emqx-lwm2m (plugin) 

Bug fixes: 

  * Fix the issue that mountpoint didn't take effect 

Github PR: [ emqx/emqx-lwm2m#34 ](https://github.com/emqx/emqx-lwm2m/pull/34)

  * Fix the issue that message couldn't be forwarded by ` emqx-web-hook `

Github PR: [ emqx/emqx-lwm2m#35 ](https://github.com/emqx/emqx-lwm2m/pull/35)




## Version 3.1.0 

*Release Date: 2019-04-26*

EMQ X 3.1.0 is now available. The rule engine has become stable and production ready. We've also introduced an emqx-edge manager - the ` Storm ` , and improved some code for flapping. 

### EMQ X Core 

Enhancements: 

  * Add emqx_ct_helpers as deps and refactor test suites 

Github PR: [ emqx/emqx#2480 ](https://github.com/emqx/emqx/pull/2480)

  * Refactor flapping code 

Github PR: [ emqx/emqx#2476 ](https://github.com/emqx/emqx/pull/2476)




### emqx-management (plugin) 

Bug fixes: 

  * Fixed listeners acceptors is undefined 

Github PR: [ emqx/emqx-management#76 ](https://github.com/emqx/emqx-management/pull/76)




### emqx-rule-engine (plugin) 

Enhancements: 

  * Support validation of rule action params 

Github PR: [ emqx/emqx-rule-engine#b28318 ](https://github.com/emqx/emqx-rule-engine/commit/b283184dcbb207e8d58ac308c027a093a4f4ab88)

  * Check dependency when deleting resources 

Github PR: [ emqx/emqx-rule-engine#fa75b9 ](https://github.com/emqx/emqx-rule-engine/commit/fa75b952efb7951bc57242adc8e953dbbba6b2ed)

  * Remove ` from ` param from republish action 

Github PR: [ emqx/emqx-rule-engine#8721eb ](https://github.com/emqx/emqx-rule-engine/commit/8721ebe583d5426f239b5b1f044fe381bf4ea0b7)

  * Fix where clause of SQL cannot handle integers 

Github PR: [ emqx/emqx-rule-engine#c9c761 ](https://github.com/emqx/emqx-rule-engine/commit/c9c7616f86019657861dff408854e9c5238d666b)




### emqx-storm (plugin) 

Enhancements: 

  * Support edge storm 

Github Repository: [ emqx/emqx-storm ](https://github.com/emqx/emqx-storm)




## Version 3.1-rc.3 

*Release Date: 2019-04-19*

EMQ X 3.1-rc.3 is now available. In this version we've enhanced the Rule-Engine and fixed some bugs. 

::: tip Tip
Starting with this release, add OpenSUSE's installation package, and no longer provide Debian 7's installation package. 
:::

### EMQ X Core 

Enhancements: 

  * Support flapping detection for clients, and banning abnormal clients 

Github PR: [ emqx/emqx#2438 ](https://github.com/emqx/emqx/pull/2438)

  * Support configuring output length of log 

Github PR: [ emqx/emqx#2461 ](https://github.com/emqx/emqx/pull/2461)




Bug fixes: 

  * Fix an issue that ` emqx_client ` doesn't set Keep Alive field correctly for CONNECT packet 

Github PR: [ emqx/emqx#2443 ](https://github.com/emqx/emqx/pull/2443)




### emqx-auth-mysql (plugin) 

Enhancements: 

  * Support proxysql 

Github PR: [ emqx/emqx-auth-mysql#134 ](https://github.com/emqx/emqx-auth-mysql/pull/134)




### emqx-statsd (plugin) 

Bug fixes: 

  * Fix an Windows compatibility issue 

Github PR: [ emqx/emqx-statsd#24 ](https://github.com/emqx/emqx-statsd/pull/24)




### emqx-web-hook (plugin) 

Enhancements: 

  * Support event actions in webhook 

Github Commit: [ emqx/emqx-web-hook#8367e0 ](https://github.com/emqx/emqx-web-hook/commit/8367e02f5ccafc7df9600c258348461a67c171bd)

  * Improve specs of webhook resource 

Github Commit: [ emqx/emqx-web-hook#5a1345 ](https://github.com/emqx/emqx-web-hook/commit/5a13457d4f823fa80df1c7eab9a8e945ae6a0701)

  * Support search actions by hook type 

Github Commit: [ emqx/emqx-web-hook#fb3b1b ](https://github.com/emqx/emqx-web-hook/commit/fb3b1ba98ca3f2557a51be98a06537781119132c)




### emqx-rule-engine (plugin) 

Enhancements: 

  * Support search actoins by resource type 

Github PR: [ emqx/emqx-rule-engine#25 ](https://github.com/emqx/emqx-rule-engine/pull/25)

  * Load resource providers instead of register providers 

Github PR: [ emqx/emqx-rule-engine#26 ](https://github.com/emqx/emqx-rule-engine/pull/26)

  * Improve the input data for actions 

Github PR: [ emqx/emqx-rule-engine#27 ](https://github.com/emqx/emqx-rule-engine/pull/27)




### emqx-rel 

Bug fixes: 

  * Fix start fail after changing log.rotation.size 

Github PR: [ emqx/emqx-rel#336 ](https://github.com/emqx/emqx-rel/pull/336)




## Version 3.1-rc.2 

*Release Date: 2019-04-13*

EMQ X 3.1-rc.2 is now available. In this version we've enhanced the Rule-Engine and fixed some bugs. 

### EMQ X Core 

Enhancements: 

  * Redesign ` ensure_start ` and ` ensure_stop ` api of ` emqx_bridge `

Github PR: [ emqx/emqx#2423 ](https://github.com/emqx/emqx/pull/2423)

  * Expose handler of ` emqx_bridge `

Github PR: [ emqx/emqx#2414 ](https://github.com/emqx/emqx/pull/2414)




Bug fixes: 

  * Fix an issue that metrics are missed in statistics when session terminated 

Github PR: [ emqx/emqx#2416 ](https://github.com/emqx/emqx/pull/2416)

  * Check log level before tracing 

Github PR: [ emqx/emqx#2408 ](https://github.com/emqx/emqx/pull/2408)




### emqx-auth-http (plugin) 

Enhancements: 

  * Support updating ` mountpoint ` from user's Web Server in credentials 

Github PR: [ emqx/emqx-auth-http#116 ](https://github.com/emqx/emqx-auth-http/pull/116)




### emqx-auth-username (plugin) 

Enhancements: 

  * Remove the function that configures username in the emqx_auth_username.conf 

Github PR: [ emqx/emqx-auth-username#96 ](https://github.com/emqx/emqx-auth-username/pull/96)




### emqx-auth-clientid (plugin) 

Enhancements: 

  * Remove the function that configures clientid in the emqx_auth_clientid.conf 

Github PR: [ emqx/emqx-auth-clientid#81 ](https://github.com/emqx/emqx-auth-clientid/pull/81)




### emqx-rule-engine (plugin) 

Enhancements: 

  * Support Posix-Style CLI in rule engine CLI 

Github PR: [ emqx/emqx-rule-engine#23 ](https://github.com/emqx/emqx-rule-engine/pull/23)




Bug fixes: 

  * Fix some Bugs in HTTP APIs 

Github PR: [ emqx/emqx-rule-engine#21 ](https://github.com/emqx/emqx-rule-engine/pull/21)




### emqx-packages (plugin) 

Bug fixes: 

  * Fix issue that EMQ X boots abortively on CentOS 

Github Commit: [ emqx/emqx-packages#64760523ea29ca0ad1d85b763f0e8a8e6954db9c ](https://github.com/emqx/emqx-packages/commit/64760523ea29ca0ad1d85b763f0e8a8e6954db9c)




### emqx-dashboard (plugin) 

Enhancements: 

  * Add interactive web interface for Rule-Engine 

Github PR: [ emqx/emqx-dashboard#50 ](https://github.com/emqx/emqx-dashboard/pull/50)

  * Support managing users of Dashboard in cluster 

Github PR: [ emqx/emqx-dashboard#48 ](https://github.com/emqx/emqx-dashboard/pull/48)




## Version 3.1-rc.1 

*Release Date: 2019-04-04*

EMQ X 3.1-rc.1 is now available. In this version we've improved rule engine, fixed some bugs, improved the stability, and so on. 

### EMQ X Core 

Enhancements: 

  * Support compress websocket message 

Github PR: [ emqx/emqx#2356 ](https://github.com/emqx/emqx/pull/2356)

  * etcd  cluster support SSL connection 

Github PR: [ emqx/emqx#2367 ](https://github.com/emqx/emqx/pull/2367)

  * Support proxy protocol of websocket 

Github PR: [ emqx/emqx#2372 ](https://github.com/emqx/emqx/pull/2372)




Bug fixes: 

  * Fix the error logic in the monitor modules 

Github PR: [ emqx/emqx#2353 ](https://github.com/emqx/emqx/pull/2353)

  * Fix  allow_anonymous  behavoir error 

Github PR: [ emqx/emqx#2355 ](https://github.com/emqx/emqx/pull/2355)

  * Fix drain the session process mailbox handling error 

Github PR: [ emqx/emqx#2373 ](https://github.com/emqx/emqx/pull/2373)

  * Fix the problem that  message.dropped  hook will not be triggered in some cases 

Github PR: [ emqx/emqx#2399 ](https://github.com/emqx/emqx/pull/2399)




### emqx-auth-http (plugin) 

Enhancements: 

  * Support for using Subject Name and Common Name for authentication 

Github PR: [ emqx/emqx-auth-http ](https://github.com/emqx/emqx-auth-http/pull/113)




### emqx-auth-clientid (plugin) 

Enhancements: 

  * Support for operating ClientId via REST API 

Github PR: [ emqx/emqx-auth-clientid ](https://github.com/emqx/emqx-auth-clientid/pull/78)




### emqx-auth-jwt (plugin) 

Enhancements: 

  * Support to verify the specified claims fields 

Github PR: [ emqx/emqx-auth-jwt#69 ](https://github.com/emqx/emqx-auth-jwt/pull/69)




### emqx-rule-engine (plugin) 

Enhancements: 

  * Improve rule engine 

Github Repository: [ emqx/emqx-rule-engine ](https://github.com/emqx/emqx-rule-engine)




### emqx-rel 

Bug fixes: 

  * Fix windows boot twice problem 

Github Commit: [ emqx/emqx-rel#75de3441db9bf03d489609dcbb340a74de263508 ](https://github.com/emqx/emqx-rel/commit/75de3441db9bf03d489609dcbb340a74de263508)

  * Fix the problem when boot path contains spaces or chinese character 

Github Commit: [ emqx/emqx-rel#75de3441db9bf03d489609dcbb340a74de263508 ](https://github.com/emqx/emqx-rel/commit/75de3441db9bf03d489609dcbb340a74de263508)




## Version 3.1-beta.3 

*Release Date: 2019-04-26*

EMQ X 3.1-beta.3 is now available. In this version we've introduced rule engine, improved plugin discovery mechanism, fixed some bugs, and so on. 

### EMQ X Core 

Enhancements: 

  * Improve plugin discovery mechanism 

Github PR: [ emqx/emqx#2339 ](https://github.com/emqx/emqx/pull/2339)




Bug fixes: 

  * Fix bug of clearing alarm repeatedly 

Github PR: [ emqx/emqx#2332 ](https://github.com/emqx/emqx/pull/2332)

  * Fix bug of parsing sticky package failure 

Github PR: [ emqx/emqx#2333 ](https://github.com/emqx/emqx/pull/2333)

  * Set DUP flag in PUBLISH packet correctly 

Github PR: [ emqx/emqx#2337 ](https://github.com/emqx/emqx/pull/2337)




### emqx-rule-engine (plugin) 

Enhancements: 

  * Implement prototype of rule engine 

Github Repository: [ emqx/emqx-rule-engine ](https://github.com/emqx/emqx-rule-engine)




### emqx-lua-hook (plugin) 

Enhancements: 

  * Add auth and acl hook 

Github PR: [ emqx/emqx-lua-hook#63 ](https://github.com/emqx/emqx-lua-hook/pull/63)




### emqx-auth-mysql (plugin) 

Bug fixes: 

  * Fix bug that ACL could not be loaded 

Github PR: [ emqx/emqx-auth-mysql#130 ](https://github.com/emqx/emqx-auth-mysql/pull/130) , [ emqx/emqx-auth-mysql#128 ](https://github.com/emqx/emqx-auth-mysql/pull/128)




## Version 3.1-beta.2 

*Release Date: 2019-03-16*

EMQ X 3.1-beta.2 is now available. In this version we've redesigned the  hooks  , supported  TLS/PSK  , fixed some issues about  gen_rpc  , and so on. 

### EMQ X Core 

Enhancements: 

  * Improve emqx hooks and credentials 

Github PR: [ emqx/emqx#2309 ](https://github.com/emqx/emqx/pull/2309)

  * Support TLS/DTLS PSK 

Github PR: [ emqx/emqx#2297 ](https://github.com/emqx/emqx/pull/2297)

  * Move request response out of emqx client 

Github PR: [ emqx/emqx#2293 ](https://github.com/emqx/emqx/pull/2293)




Bug fixes: 

  * Broker crash when forwarding message in cluster 

Github issues: [ emqx/emqx#2290 ](https://github.com/emqx/emqx/issues/2290)

Github PR: [ emqx/emqx#2320 ](https://github.com/emqx/emqx/pull/2320)

  * Unload emqx_alarm_handler before unloading plugins when shutting down 

Github PR: [ emqx/emqx#2316 ](https://github.com/emqx/emqx/pull/2316)

  * Fixed a bug related to emqx bridge 

Github issues: [ emqx/emqx#2312 ](https://github.com/emqx/emqx/issues/2312)

Github PR: [ emqx/emqx#2313 ](https://github.com/emqx/emqx/pull/2313)

  * Eliminate inflight full error 

Github PR: [ emqx/emqx#2281 ](https://github.com/emqx/emqx/pull/2281)




### emqx-management (plugin) 

Enhancements: 

  * Add default application secret configuration 

Github PR: [ emqx/emqx-management#58 ](https://github.com/emqx/emqx-management/pull/58)

  * Fix plugin reload error when plugin is not started 

Github PR: [ emqx/emqx-management#59 ](https://github.com/emqx/emqx-management/pull/59)

  * Move plugin-related HTTP APIs to each plugin 

Github PR: [ emqx/emqx-management#57 ](https://github.com/emqx/emqx-management/pull/57)

  * Fix io/max_fds undefined issue 

Github issues: [ emqx/emqx-management#2222 ](https://github.com/emqx/emqx-management/issues/2222)

Github PR: [ emqx/emqx-management#54 ](https://github.com/emqx/emqx-management/pull/54)




### emqx-auth-jwt (plugin) 

Enhancements: 

  * Improve the JWT Auth plugin 

Github PR: [ emqx/emqx-auth-jwt#63 ](https://github.com/emqx/emqx-auth-jwt/pull/63)




### emqx-auth-username (plugin) 

Enhancements: 

  * Add CURD HTTP API for managing usernames 

Github PR: [ emqx/emqx-auth-username#82 ](https://github.com/emqx/emqx-auth-username/pull/82)




### emqx-web-hook (plugin) 

Bug fixes: 

  * Fix bug when formatting message 

Github issues: [ emqx/emqx-web-hook#93 ](https://github.com/emqx/emqx-web-hook/issues/93)

Github PR: [ emqx/emqx-web-hook#96 ](https://github.com/emqx/emqx-web-hook/pull/96)




### minirest (deps) 

Bug fixes: 

  * Filter the API for plugins that not started 

Github PR: [ emqx/minirest#12 ](https://github.com/emqx/minirest/pull/12)




### gen_rpc (deps) 

Bug fixes: 

  * Fix raw socket flags for 'gen_tcp' 

Github PR: [ emqx/gen_rpc#5 ](https://github.com/emqx/gen_rpc/pull/5)




## Version 3.1-beta.1 

*Release Date: 2019-02-28*

The EMQ X 3.1-beta.1 is now available. This version focuses on feature improvements. We introduced new broker bridge, implemented the batch packets delivery, added supports for redis cluster, and so on. 

### EMQ X Core 

Enhancements: 

  * Introduce new bridge implement 

Github PR: [ emqx/emqx#2199 ](https://github.com/emqx/emqx/pull/2199)

  * Support batch delivery 

Github PR: [ emqx/emqx#2253 ](https://github.com/emqx/emqx/pull/2253)

  * Improve the  emqx_connection  module by using gen_statem behaviour 

Github PR: [ emqx/emqx#2235 ](https://github.com/emqx/emqx/pull/2235)

  * Add monitors and improve alarm handler 

Github PR: [ emqx/emqx#2266 ](https://github.com/emqx/emqx/pull/2266)




### emqx-auth-redis 

Enhancements: 

  * Support redis cluster 

Github PR: [ emqx/emqx-auth-redis#93 ](https://github.com/emqx/emqx-auth-redis/pull/93)




### emqx-dashboard 

Enhancements: 

  * Add test cases for  emqx_dashboard_cli  module 

Github PR: [ emqx/emqx-dashboard#34 ](https://github.com/emqx/emqx-dashboard/pull/34)




### emqx-auth-username 

Enhancements: 

  * Add new cli to update username 

Github PR: [ emqx/emqx-auth-username#74 ](https://github.com/emqx/emqx-auth-username/pull/74)




### emqx-auth-clientid 

Enhancements: 

  * Add new cli to update clientid 

Github PR: [ emqx/emqx-auth-clientid#59 ](https://github.com/emqx/emqx-auth-clientid/pull/59)




## Version 3.0.1 

*Release Date: 2019-01-25*

The EMQ X 3.0.1 is now available. Many improvements and bug fixes has been made. 

### EMQ X Core 

Enhancements: 

  * Add +L vm args for reducing some memory for emqx edge 

Github PR: [ emqx/emqx#2110 ](https://github.com/emqx/emqx/pull/2110)

  * Change logger level in a single command 

Github PR: [ emqx/emqx#2115 ](https://github.com/emqx/emqx/pull/2115)

  * Refactor the emqx bridge; Support bridge message persistence. 

Github PR: [ emqx/emqx#2160 ](https://github.com/emqx/emqx/pull/2160) , [ emqx/emqx#2117 ](https://github.com/emqx/emqx/pull/2117) , [ emqx/emqx#2113 ](https://github.com/emqx/emqx/pull/2113) , [ emqx/emqx#2108 ](https://github.com/emqx/emqx/pull/2108) , [ emqx/emqx#2053 ](https://github.com/emqx/emqx/pull/2053)

  * Optimize route matching 

Github PR: [ emqx/emqx#2124 ](https://github.com/emqx/emqx/pull/2124)

  * Improve the design of 'emqx_client' module 

Github PR: [ emqx/emqx#2137 ](https://github.com/emqx/emqx/pull/2137)

  * Improve the design of 'emqx_pool' module 

Github PR: [ emqx/emqx#2138 ](https://github.com/emqx/emqx/pull/2138)

  * Improve shared subscribe dispatch implementation 

Github PR: [ emqx/emqx#2144 ](https://github.com/emqx/emqx/pull/2144)

  * Re-generate the configuration when restarting emqx 

Github PR: [ emqx/emqx#2175 ](https://github.com/emqx/emqx/pull/2175)




Bug Fixes: 

  * Fix crash if peer closed the connection 

Github PR: [ emqx/emqx#2120 ](https://github.com/emqx/emqx/pull/2120)

  * Fix the bug that send will message unexpectedly 

Github PR: [ emqx/emqx#2156 ](https://github.com/emqx/emqx/pull/2156)




### emqx-lwm2m (plugin) 

Bug Fixes: 

  * Remove authentication for LwM2M 

GitHub PR: [ emqx/emqx-lwm2m#14 ](https://github.com/emqx/emqx-lwm2m/pull/14)




### emqx-auth-username (plugin) 

Enhancements: 

  * Support optional encryption modes 

GitHub PR: [ emqx/emqx-auth-usernmae#64 ](https://github.com/emqx/emqx-auth-username/pull/64)




### emqx-auth-clientid (plugin) 

Enhancements: 

  * Support optional encryption modes 

GitHub PR: [ emqx/emqx-auth-clientid#52 ](https://github.com/emqx/emqx-auth-username/pull/52)




### emqx-management (plugin) 

Enhancements: 

  * Add a new CLI 'plugins reload \<Name>'; Re-generate the configuration when reloading emqx plugin 

Github PR: [ emqx/emqx-management#30 ](https://github.com/emqx/emqx-management/pull/30)




## Version 3.0.0 

*Release Date: 2018-12-22*

The EMQ X 3.0.0 is now available. In this release, we have re-designed the ETS tables for subscripions, and enhanced the performance by refactoring some modules and tuning the erlang vm args. 

### EMQ X Core 

Enhancements: 

  * Move addtional vm args to a separate vm.args file 

Github PR: [ emqx/emqx#2033 ](https://github.com/emqx/emqx/pull/2033) , [ emqx/emqx#2057 ](https://github.com/emqx/emqx/pull/2057) , [ emqx/emqx#2070 ](https://github.com/emqx/emqx/pull/2070)

  * Add will topic validation and acl check 

Github PR: [ emqx/emqx#2075 ](https://github.com/emqx/emqx/pull/2075)

  * Add option to disconnect client in case of ACL denied 

Github PR: [ emqx/emqx#2059 ](https://github.com/emqx/emqx/pull/2059)

  * Implement a new session supervisor 

Github PR: [ emqx/emqx#2077 ](https://github.com/emqx/emqx/pull/2077)

  * Add 'active_n' option to optimize the CPU usage of  emqx_connection 

Github PR: [ emqx/emqx#2060 ](https://github.com/emqx/emqx/pull/2060)

  * Supports batch processing 'DOWN' events 

Github PR: [ emqx/emqx#2060 ](https://github.com/emqx/emqx/pull/2060)

  * Add sharding for subscription tables 

Github PR: [ emqx/emqx#2044 ](https://github.com/emqx/emqx/pull/2044)

  * Implement a new 'emqx_gc' module 

Github PR: [ emqx/emqx#2090 ](https://github.com/emqx/emqx/pull/2090)




Bug Fixes: 

  * Fix bug for  Topic Alias Maximum 

Github PR: [ emqx/emqx#2074 ](https://github.com/emqx/emqx/pull/2074)

  * Fix a bug that would not send a will message in some cases 

Github PR: [ emqx/emqx#2068 ](https://github.com/emqx/emqx/pull/2068)




### emqx-auth-ldap (plugin) 

Enhancements: 

  * Better design 

GitHub PR: [ emqx/emqx-auth-ldap#46 ](https://github.com/emqx/emqx-auth-ldap/pull/46)




### emqx-lua-hook (plugin) 

Bug Fixes: 

  * Make all test cases pass 

GitHub PR: [ emqx/emqx-lua-hook#45 ](https://github.com/emqx/emqx-lua-hook/pull/45)




### emqx-management (plugin) 

Enhancements: 

  * Add test cases for rest api and better design for the format of response 

GitHub PR: [ emqx/emqx-management#21 ](https://github.com/emqx/emqx-management/pull/21)




## Version 3.0-rc.5 

*Release Date: 2018-11-30*

The EMQ X 3.0-rc.5 is now available. The maintenance release fixes some bugs and starts supporting batch update of metrics. 

### EMQ X Core 

Enhancements: 

  * Reduce dependencies' size 

Github PR: [ emqx/emqx#1981 ](https://github.com/emqx/emqx/pull/1981)

  * Support batch update of metrics 

Github PR: [ emqx/emqx#2001 ](https://github.com/emqx/emqx/pull/2001)

  * Optimize read/write concurrency of  mnesia/ets  tables 

Github PR: [ emqx/emqx#2006 ](https://github.com/emqx/emqx/pull/2006)




Bug Fixes: 

  * Fix 'function_clause' in  emqx_router 

Github PR: [ emqx/emqx#1998 ](https://github.com/emqx/emqx/pull/1998)

  * Remove  simple log handler  at startup 

Github PR: [ emqx/emqx#2000 ](https://github.com/emqx/emqx/pull/2000)

  * Fix the atom leaks in  emqx_reason_codes  module 

Github PR: [ emqx/emqx#2008 ](https://github.com/emqx/emqx/pull/2008)




### emqx-passwd (plugin) 

Enhancements: 

  * Support  Rebar3 

GitHub PR: [ emqx/emqx-passwd#6 ](https://github.com/emqx/emqx-passwd/pull/6)




### emqx-web-hook (plugin) 

Enhancements: 

  * Support  Rebar3 

GitHub PR: [ emqx/emqx-web-hook#77 ](https://github.com/emqx/emqx-web-hook/pull/77)




Bug Fixes: 

  * username  and  clientid  in http request is empty in  emqx-web-hook 

GitHub PR: [ emqx/emqx-web-hook#77 ](https://github.com/emqx/emqx-web-hook/pull/77)




### emqx-dashboard (plugin) 

Bug Fixes: 

  * Firefox browser can not copy application info. 

GitHub PR: [ emqx/emqx-dashboard#12 ](https://github.com/emqx/emqx-dashboard/pull/12)




### emqx-management (plugin) 

Bug Fixes: 

  * Fix the crash caused by  clients  CLI. 

GitHub PR: [ emqx/emqx-management#16 ](https://github.com/emqx/emqx-management/pull/16)




## Version 3.0-rc.4 

*Release Date: 2018-11-24*

The EMQ X 3.0-rc.4 release improves logging, enhances support for  Rebar3  . 

### EMQ X Core 

Enhancements: 

  * Add  ignore_loop_deliver  flag for client with  MQTT v3.1.1  to avoid loop delivery 

Github PR: [ emqx/emqx#1964 ](https://github.com/emqx/emqx/pull/1964)

  * Support using  username  to replace  client_id  , disabled by default 

Github PR: [ emqx/emqx#1961 ](https://github.com/emqx/emqx/pull/1961)

  * Enable  emqx.log  by default 

Github PR: [ emqx/emqx#1979 ](https://github.com/emqx/emqx/pull/1979)

  * Add  CLI  for log level 

Github PR: [ emqx/emqx#1977 ](https://github.com/emqx/emqx/pull/1977)

  * Improve  CLI  for log tracer 

Github PR: [ emqx/emqx#1973 ](https://github.com/emqx/emqx/pull/1973)

  * Optimize log performance 

Github PR: [ emqx/emqx#1960 ](https://github.com/emqx/emqx/pull/1960)




Bug Fixes: 

  * Fix type validation for  User-Property 

Github PR: [ emqx/emqx#1969 ](https://github.com/emqx/emqx/pull/1969)

  * Fix wrong description for  max_topic_alias 

Github PR: [ emqx/emqx#1962 ](https://github.com/emqx/emqx/pull/1962)

  * Update proc meta-data for empty  client_id 

Github PR: [ emqx/emqx#1980 ](https://github.com/emqx/emqx/pull/1980)




### emqx-coap (plugin) 

Enhancements: 

  * Support  Rebar3 

GitHub PR: [ emqx/emqx-coap#89 ](https://github.com/emqx/emqx-coap/pull/89)




Bug fixes: 

  * Fix bad using of  sendfun 

GitHub PR: [ emqx/emqx-coap#89 ](https://github.com/emqx/emqx-coap/pull/89)




### emqx-management (plugin) 

Bug fixes: 

  * Fix the unstable rest api for lookup connection in cluster mode 

GitHub PR: [ emqx/emqx-management#11 ](https://github.com/emqx/emqx-management/pull/11)




### ekka (dependency) 

Bug Fixes: 

  * Fix bug in distributed lock 

GitHub PR: [ emqx/ekka#39 ](https://github.com/emqx/ekka/pull/39)




### minirest (dependency) 

Enhancements: 

  * Support  Rebar3 

GitHub PR: [ emqx/minirest#6 ](https://github.com/emqx/minirest/pull/6)




### cuttlefish (dependency) 

Bug fixes: 

  * Change default logger to  std_error 

GitHub PR: [ emqx/cuttlefish#4 ](https://github.com/emqx/cuttlefish/pull/4)




### emqx-rel (build-project) 

Enhancements: 

  * Build with  cuttlefish 

GitHub PR: [ emqx/emqx-rel#253 ](https://github.com/emqx/emqx-rel/pull/253)

  * delay_publish  plugin is disabled by default 

GitHub PR: [ emqx/emqx-rel#251 ](https://github.com/emqx/emqx-rel/pull/251)




## Version 3.0-rc.3 

*Release Date: 2018-11-10*

The EMQ X 3.0-rc.3 release rewrites  emqx_mqueue  module, supports  MQTT-SN  ,  CoAP  and  STOMP  protocols. 

### EMQ X Core 

Enhancements: 

  * Replace macro  QOS$i  to  QOS_$i 

Github PR: [ emqx/emqx#1948 ](https://github.com/emqx/emqx/pull/1948)

  * Fix config descriptions of  ACL cache 

Github PR: [ emqx/emqx#1950 ](https://github.com/emqx/emqx/pull/1950)

  * Rewrite  emqx_mqueue  module 

Github PR: [ emqx/emqx#1926 ](https://github.com/emqx/emqx/pull/1926)

  * Change  lager  to  logger 

Github PR: [ emqx/emqx#1898 ](https://github.com/emqx/emqx/pull/1898)




Bug Fixes: 

  * Fix 'badarg' bug with duplicate subscriptions 

Github PR: [ emqx/emqx#1943 ](https://github.com/emqx/emqx/pull/1943)

  * Fix 'badarg' in  io_lib:format/2  when 'from' field is tuple 

Github PR: [ emqx/emqx#1954 ](https://github.com/emqx/emqx/pull/1954)

  * MQTT bridge  via  TLS 

Github PR: [ emqx/emqx#1949 ](https://github.com/emqx/emqx/pull/1949)




### emqx-stomp (plugin) 

Enhancements: 

  * Improve support for  receipt  frame, and add test cases 

GitHub PR: [ emqx/emqx-stomp#53 ](https://github.com/emqx/emqx-stomp/pull/53)




### emqx-sn (plugin) 

Enhancements: 

  * Improve support for  MQTT-SN  protocol 

GitHub PR: [ emqx/emqx-sn#90 ](https://github.com/emqx/emqx-sn/pull/90)




### emqx-lua-hook (plugin) 

Bug Fixes: 

  * Fix errors when load/unload lua hooks 

GitHub PR: [ emqx/emqx-lua-hook#41 ](https://github.com/emqx/emqx-lua-hook/pull/41)




### emqx-statsd (plugin) 

Enhancements: 

  * Add metrics 

GitHub PR: [ emqx/emqx-statsd#4 ](https://github.com/emqx/emqx-statsd/pull/4)




### emqx-dashboard (plugin) 

Enhancements: 

  * Add  qos2/forward  metric 

GitHub PR: [ emqx/emqx-dashboard#7 ](https://github.com/emqx/emqx-dashboard/pull/7)




### emqx-auth-pgsql (plugin) 

Enhancements: 

  * Improve concurrency performance in  emqx-auth-pgsql 

GitHub PR: [ emqx/emqx-auth-pgsql#94 ](https://github.com/emqx/emqx-auth-pgsql/pull/94)




## Version 3.0-rc.2 

*Release Date: 2018-10-27*

The EMQ X 3.0-rc.2 release improved the  Will Message  publishing mechanism, and add support for using  ssl  certificate as  MQTT  username. 

### EMQ X Core 

Enhancements: 

  * Improve publish mechanism of  Will Message 

Github PR: [ emqx/emqx#1889 ](https://github.com/emqx/emqx/pull/1889)

  * Support for using  ssl  certificate as  MQTT  username 

Github PR: [ emqx/emqx#1913 ](https://github.com/emqx/emqx/pull/1913)

  * Improve test coverage for modules 

Github PR: [ emqx/emqx#1921 ](https://github.com/emqx/emqx/pull/1921)




Bug Fixes: 

  * Fix 'bad argument' error when  emqx_broker:subscribed  is called 

Github PR: [ emqx/emqx#1921 ](https://github.com/emqx/emqx/pull/1921)




## Version 3.0-rc.1 

*Release Date: 2018-10-20*

The EMQ X 3.0-rc.1 release is mainly for bug fixes and new features improvements for MQTT 5.0. 

### EMQ X Core 

Enhancements: 

  * Add  request  & response  support for  CONNECT  & CONNACK 

Github PR: [ emqx/emqx#1819 ](https://github.com/emqx/emqx/pull/1819)

  * Add warning logs for unauthorized subscribe 

Gihub PR: 

[ emqx/emqx#1878 ](https://github.com/emqx/emqx/pull/1878)

  * Improve coverage for  emqx_hooks  , and add test case for  emqx_mod_sup 

Gihub PR: 

[ emqx/emqx#1892 ](https://github.com/emqx/emqx/pull/1892)




Bug Fixes: 

  * Fix the bad link to ACL doc 

Github PR: [ emqx/emqx#1899 ](https://github.com/emqx/emqx/pull/1899)

  * Fix bug in validating publish packet 

Github PR: [ emqx/emqx#1888 ](https://github.com/emqx/emqx/pull/1888)

  * Fix bugs that not deliver  Reason Code  to client 

Github PR: [ emqx/emqx#1819 ](https://github.com/emqx/emqx/pull/1819)

  * Fix compatibility problems in  emqx_client  module 

Github PR: [ emqx/emqx#1819 ](https://github.com/emqx/emqx/pull/1819)




### emqx-lwm2m 

  * Update LwM2M plugin for EMQ X 3.0 

Github PR: [ emqx/emqx-lwm2m#3 ](https://github.com/emqx/emqx-lwm2m/pull/3)




## Version 3.0-beta.4 

*Release Date: 2018-09-30*

The EMQ X 3.0-beta.4 release is mainly for bug fixes and feature improvements on MQTT 5.0. 

### EMQ X Core 

Enhancements: 

  * Add  max_heap_size  for process 

GitHub PR: [ emqx/emqx#1855 ](https://github.com/emqx/emqx/pull/1855)

  * Improve handling of  Topic Alias Maximum  and  Receive Maximum  properties 

Github PR: [ emqx/emqx#1873 ](https://github.com/emqx/emqx/pull/1873)

  * Add  Mountpoint  to zone 

Github PR: [ emqx/emqx#1869 ](https://github.com/emqx/emqx/pull/1869)

  * Improve travis build to support  rebar3 xref 

Github PR: [ emqx/emqx#1861 ](https://github.com/emqx/emqx/pull/1861)

  * Upgrade dependency  esockd  to v5.4.2 

Github PR: [ emqx/emqx#1875 ](https://github.com/emqx/emqx/pull/1875)




Bug Fixes: 

  * Fix sticky strategy when two or more shared subscriber groups 

GitHub PR: [ emqx/emqx#1871 ](https://github.com/emqx/emqx/pull/1871)

  * Fix errors when running  make app.config 

GitHub PR: [ emqx/emqx#1868 ](https://github.com/emqx/emqx/pull/1868)

  * Fix incorrect args 

GitHub PR: [ emqx/emqx#1866 ](https://github.com/emqx/emqx/pull/1866)




### emqx-passwd (plugin) 

Enhancements: 

  * Upgrade dependency  erlang-bcrypt  to v0.5.1 and expose  check_pass  for the use of various auth plugins 

GitHub PR: [ emqx/emqx-passwd#3 ](https://github.com/emqx/emqx-passwd/pull/3)




### emqx-delayed-publish (plugin) 

Bug Fixes: 

  * Fix incorrect matching 

GitHub PR: [ emqx/emqx-delayed-publish#5 ](https://github.com/emqx/emqx-delayed-publish/pull/5)




### erlang-bcrypt (dependency) 

Enhancements: 

  * Add $2b, $2x and $2y prefixes support 

GitHub PR: [ emqx/erlang-bcrypt#1 ](https://github.com/emqx/erlang-bcrypt/pull/1)




### esockd (dependency) 

Enhancements: 

  * Add examples for DTLS PSK 

GitHub PR: [ emqx/esockd#88 ](https://github.com/emqx/esockd/pull/88)

  * Improve start of SSL 

Github PR: [ emqx/esockd#90 ](https://github.com/emqx/esockd/pull/90)




Bug Fixes: 

  * Fix DTLS start failure 

GitHub PR: [ emqx/esockd#89 ](https://github.com/emqx/esockd/pull/89)




## Version 3.0-beta.3 

*Release Date: 2018-09-23*

The EMQ X 3.0-beta.3 release is mainly for bug fixes and feature improvements on MQTT 5.0. 

### EMQ X Core 

Enhancements: 

  * Improve the  force_gc_policy  config 

GitHub issues: [ emqx/emqx#1851 ](https://github.com/emqx/emqx/pull/1851)

  * Improve design of bridges 

GitHub issues: [ emqx/emqx#1849 ](https://github.com/emqx/emqx/pull/1849)

  * Add force shutdown policy 

GitHub issues: [ emqx/emqx#1836 ](https://github.com/emqx/emqx/pull/1836)

  * Add new shared subscription dispatch strategy 

GitHub issues: [ emqx/emqx#1823 ](https://github.com/emqx/emqx/pull/1823)

  * Improve the design of  esockd_connection_sup  module 

GitHub issues: [ emqx/emqx#86 ](https://github.com/emqx/esockd/pull/86)

  * Configurable websocket path 

GitHub issues: [ emqx/emqx#1809 ](https://github.com/emqx/emqx/pull/1809) , [ emqx/emqx#1814 ](https://github.com/emqx/emqx/pull/1814)

  * Improve handling of  Message Expiry Interval  property 

GitHub issues: [ emqx/emqx#1813 ](https://github.com/emqx/emqx/pull/1813)

  * Support more gc enforcement policies 

GitHub issues: [ emqx/emqx#1808 ](https://github.com/emqx/emqx/pull/1808)

  * Rebar3 and erlang.mk dual support 

GitHub issues: [ emqx/emqx#1806 ](https://github.com/emqx/emqx/pull/1806)




Bug Fixes: 

  * Fix incorrect value of  Maximum QoS  property 

GitHub issues: [ emqx/emqx#1848 ](https://github.com/emqx/emqx/issues/1848) , [ emqx/emqx#1857 ](https://github.com/emqx/emqx/pull/1857)

  * Fix the handling for  Session Expiry Interval  property 

GitHub issues: [ emqx/emqx#1833 ](https://github.com/emqx/emqx/issues/1833) , [ emqx/emqx#1834 ](https://github.com/emqx/emqx/issues/1834) , [ emqx/emqx#1845 ](https://github.com/emqx/emqx/pull/1845)

  * Fix an issue about  Publish Limit  config 

GitHub issues: [ emqx/emqx#1847 ](https://github.com/emqx/emqx/issues/1847) , [ emqx/emqx#1856 ](https://github.com/emqx/emqx/pull/1856)

  * Fix message delivery to remote connections 

GitHub issues: [ emqx/emqx#1846 ](https://github.com/emqx/emqx/pull/1846)

  * Fix an issue in travis build 

GitHub issues: [ emqx/emqx#1818 ](https://github.com/emqx/emqx/pull/1818)

  * Fix an issue when handling MQTT packages 

GitHub issues: [ emqx/emqx#1811 ](https://github.com/emqx/emqx/issues/1811) , [ emqx/emqx#1817 ](https://github.com/emqx/emqx/pull/1817)




### emqx-ratainer (plugin) 

Enhancements: 

  * Support message level TTL for retained message 

GitHub issues: [ emqx/emqx-retainer#52 ](https://github.com/emqx/emqx-retainer/issues/52) , [ emqx/emqx-retainer#60 ](https://github.com/emqx/emqx-retainer/pull/60)




### emqx-dashboard (plugin) 

Bug Fixes: 

  * Fix metrics field 

GitHub issues: [ emqx/emqx-dashboard#5 ](https://github.com/emqx/emqx-dashboard/pull/5)




### emqx-management (plugin) 

Bug Fixes: 

  * Fix subscription error 

GitHub issues: [ emqx/emqx-management#7 ](https://github.com/emqx/emqx-management/pull/7)

  * Improve CLI for bridges 

GitHub commit: [ emqx/emqx-management#a8d0b397 ](https://github.com/emqx/emqx-management/commit/a8d0b3978ee3d51119d0fb22a12286a83d30c5ff)




### emqx-web-hook (plugin) 

Bug Fixes: 

  * Fix load plugin error 

GitHub commit: [ emqx/emqx-web-hook#331ca26 ](https://github.com/emqx/emqx-web-hook/commit/331ca26550931d691c98173501ca0fb4780d7a9a)




### emqx-coap (plugin) 

Enhancements: 

  * Introduce emqx-coap into EMQ X 3.0 

GitHub issues: [ emqx/emqx-coap#86 ](https://github.com/emqx/emqx-coap/pull/86) , [ emqx/gen_coap#8 ](https://github.com/emqx/gen_coap/pull/8)




### emqx-docker (docker file) 

Enhancements: 

  * Optimize docker file 

GitHub issues: [ emqx/emqx-docker#71 ](https://github.com/emqx/emqx-docker/pull/71)




## Version 3.0-beta.2 

*Release Date: 2018-09-10*

The EMQ X 3.0-beta.2 release is mainly for bug fixes and new features support for MQTT 5.0. 

### EMQ X Core 

Enhancements: 

  * Support subscription options of MQTT 5.0 

GitHub issues: [ emqx/emqx#1788 ](https://github.com/emqx/emqx/pull/1788) , [ emqx/emqx-retainer#58 ](https://github.com/emqx/emqx-retainer/pull/58) , [ emqx/emqx#1803 ](https://github.com/emqx/emqx/pull/1803)

  * Add validations for 'Topic-Alias' of MQTT 5.0 

GitHub issues: [ emqx/emqx#1789 ](https://github.com/emqx/emqx/pull/1789) , [ emqx/emqx#1802 ](https://github.com/emqx/emqx/pull/1802)

  * Improve the design of hooks 

GitHub issue: [ emqx/emqx#1790 ](https://github.com/emqx/emqx/pull/1790)

  * Rename 'emqx_mqtt_properties' module to 'emqx_mqtt_props' 

GitHub issue: [ emqx/emqx#1791 ](https://github.com/emqx/emqx/pull/1791)

  * Update emqx_zone 

GitHub issue: [ emqx/emqx#1795 ](https://github.com/emqx/emqx/pull/1795)




Bug Fixes: 

  * Fix issues about 'Will Delay Interval' property 

GitHub issues: [ emqx/emqx#1800 ](https://github.com/emqx/emqx/pull/1800) , [ emqx/emqx-delayed-publish#3 ](https://github.com/emqx/emqx-delayed-publish/pull/3)

  * Fix an issue about 'Reserved' flag 

GitHub issue: [ emqx/emqx#1783 ](https://github.com/emqx/emqx/pull/1783)

  * Generate a config file for unit test 

GitHub issue: [ emqx/emqx#1794 ](https://github.com/emqx/emqx/pull/1794)




### emqx-management (plugin) 

Enhancements: 

  * Add restful APIs for banned function 

GitHub issue: [ emqx/emqx-management#6 ](https://github.com/emqx/emqx-management/pull/6)




### emqx-delayed-publish (plugin) 

Enhancements: 

  * Refactor the code 

GitHub issue: [ emqx/emqx-delayed-publish#4 ](https://github.com/emqx/emqx-delayed-publish/pull/4)




### minirest (dependency) 

Enhancements: 

  * Pass both query and body params to the callback functions 

GitHub issue: [ emqx/minirest#4 ](https://github.com/emqx/minirest/pull/4)




### emqx-rel (build-project) 

Enhancements: 

  * Check OTP version while compiling. 

GitHub issue: [ emqx/emqx-rel#217 ](https://github.com/emqx/emqx-rel/pull/217)




## Version 3.0-beta.1 

*Release Date: 2018-09-02*

*Release Name: Promises of Tomorrow*

### Introduction 

3.0-beta.1 version is now officially released. It is backward compatible with MQTT 3 (3.1 & 3.1.1), and it also supports new features of MQTT 5.0 specification. 

It also comes with some important features. Scalability and extensibility are improved significantly as well after refactoring some core components. 

### MQTT 5.0 Protocol specification support 

  * New packet type 

In MQTT 5.0 there is a new packet type AUTH for authentication exchange. 

  * Session expiry 

Clean session flag in MQTT 3 is now split to Clean Start Flag and a Session Expiry Interval. 

  * Message expiry 

Allow an expiry interval to be set when a message is published. 

  * Reason code on all ACKs 

All responding packet includes a reason code. The communication partner can know if a request is successful or failed with what reason. 

  * Reason string on all ACKs 

An optional reason string to reason code is allowed. 

  * Server disconnect 

Now server can disconnect a connection. 

  * Payload format and content type 

User can specify the payload format and a MIME style content type when publishing. 

  * Request/Response 

Add a few properties, formalized request and response communication pattern. 

  * Shared subscriptions 

EMQ X 2.x supports shared subscription on single-node as an unstandardized feature. Now in EMQ X 3.0, the shared subscription is cluster-wide. 

  * Subscription ID 

With a subscription ID the client is able to know from which subscription the message comes. 

  * Topic alias 

Topic can have an integer alias, which reduces the communication overhead for the long topic names. 

  * User properties 

User properties can be added in most packets. 

  * Maximum packet size 

Broker specified max packet size was already implemented in EMQ X 2.x. When an oversized message is received, it will be dropped, and broker will disconnect without informing about the reason. Now with MQTT 5.0 specification, client and broker can specify maximum messsage size limitation through CONNECT/CONNECT ACK packets. 

  * Optional server feature availability (TODO) 

Allowed features of the broker can be defined and the client can be informed of those features. 

  * Subscription options 

MQTT 5.0 provides subscription options primarily to allow for message bridge applications. For example, the option for handling nolocal and retained messages. 

  * Will delay 

MQTT 5.0 allows to specify a delay between end of connection and sending of the will message，so it can avoid to send out the will message during temporary network problems. 

  * Server keep alive 

MQTT 5.0 allows server to specify a keepalive value it wishes the client to use. 

  * Assigned ClientID 

In MQTT 5.0, if ClientID is assigned by the server, then the server should return the assigned ClientID to client. 

  * Server reference 

MQTT 5.0 allows broker to specify an alternative broker for client to use, which is uesed for server redirection. 




### Evolved Clustering Architecture 

The clustering architecture is evolved. Now a single cluster is able to serve ten-millions of concurrent connections. 
    
    
    ----------             ----------
    |  EMQX  |<--- MQTT--->|  EMQX  |
    |--------|             |--------|
    |  Ekka  |<----RPC---->|  Ekka  |
    |--------|             |--------|
    | Mnesia |<--Cluster-->| Mnesia |
    |--------|             |--------|
    | Kernel |<----TCP---->| Kernel |
    ----------             ----------

  * Ekka is introduced to auto-cluster EMQ X, and to auto-heal the cluster after net-split, following clustering methods are now supported: 
    * manual: nodes joining a cluster manually; 
    * static: auto-clustering from a pre-defined node list; 
    * mcast: auto-clustering using IP multicast; 
    * dns: auto-clustering using DNS A-records; 
    * etcd: auto-clustering using etcd; 
    * k8s: auto-clustering using kubernetes. 
  * A scalable RPC is introduced to mitigate network congestion among nodes to reduce the risk of net-split. 



### Rate Limiting 

The rate limiting is introduced to make the broker more resilient. User can configure MQTT TCP or SSL listener configuration. 

  * Concurrent connection numbers: max_clients 
  * Connection rate limitation: max_conn_rate 
  * Message delivery bytes limitation: rate_limit 
  * Message delivery number rate limitation: max_publish_rate 



### Other Feature improvements and Bug Fixes 

  * Upgraded esockd; 
  * Switched to cowboy HTTP stack for higher HTTP connection performance; 
  * Refactored the ACL caching mechanism; 
  * Added local and remote MQTT bridge; 
  * Introduced concept of "zone", that different zones can have different configurations; 
  * Refactored session module, and reduced data copy among nodes, which led to higher inter-nodes communication efficiency; 
  * Improved OpenLDAP Access Control; 
  * Added delayed publish; 
  * Supported new statistic and metrics to Prometheus; 
  * Improved the hooks. 



## Version 2.3.11 

*Release Date: 2018-07-23*

### Bugfix and Enhancements 

Fix the getting config REST API which throws exceptions. 

Support to restart listeners when emqttd is running. 

Specify a fixed tag for the dependency libraries. 

### emq-auth-jwt 

Fix token verification with jwerl 1.0.0 

### emq-auth-mongo 

Support $all variable in ACL query. (emq-auth-mongo#123) 

Support both clientid and username variables in all queries. (emq-auth-mongo#123) 

## Version 2.3.10 

*Release Date: 2018-06-27*

### Bugfix and Enhancements 

Upgrade the esockd library to v5.2.2 

### emq-auth-http 

Ignore auth on ignore in body, allows for chaining methods 

## Version 2.3.9 

*Release Date: 2018-05-20*

### Bugfix and Enhancements 

Bugfix: check params for REST publish API (#1599) 

Upgrade the mongodb library to v3.0.5 

### esockd 

Bugfix: proxy protocol - set socket to binary mode (#78) 

## Version 2.3.8 

*Release Date: 2018-05-11*

### Bugfix and Enhancements 

Bugfix: unregister users CLI when unload emq_auth_username (#1588) 

Bugfix: Should be an info level when change CleanSession (#1590) 

Bugfix: emqttd_ctl crashed when emq_auth_usename doesn't exist (#1588) 

### emq-auth-mongo 

Improve: Support authentication database (authSource) (#116) 

## Version 2.3.7 

*Release Date: 2018-04-22*

### Bugfix and Enhancements 

Bugfix: fixed spec of function setstats/3 (#1575) 

Bugfix: clean dead persistent session on connect (#1575) 

Bugfix: dup flag not set when re-deliver (#1575) 

Bugfix: Upgrade the lager_console_backend config (#1575) 

Improve: Support set k8s namespace (#1575) 

Upgrade the ekka library to v0.2.3 (#1575) 

Improve: move PIPE_DIR dir from /tmp/${WHOAMI}_erl_pipes/$NAME/ to /$RUNNER_DATA_DIR/${WHOAMI}_erl_pipes/$NAME/ (emq-relx#188) 

### emq-auth-http 

Improve: Retry 3 times when httpc:request occurred socket_closed_remotely error (emq-auth-http#70) 

## Version 2.3.6 

*Release Date: 2018-03-25*

### Bugfix and Enhancements 

Security: LWT message checking the ACL (#1524) 

Bugfix: Retain msgs should not be sent to existing subscriptions (#1529) 

### emq-auth-jwt 

Validate JWT token using a expired field (#29) 

## Version 2.3.5 

*Release Date: 2018-03-03*

### Bugfix and Enhancements 

Feature: Add etc/ssl_dist.conf file for erlang SSL distribution (emq-relx#178) 

Feature: Add node.ssl_dist_optfile option and etc/ssl_dist.conf file (#1512) 

Feature: Support Erlang Distribution over TLS (#1512) 

Improve: Tune off the 'tune_buffer' option for external MQTT connections (#1512) 

### emq-sn 

Clean registered topics if mqtt-sn client send a 2nd CONNECT in connected state (#76) 

Upgrade the esockd library to v5.2.1 (#76) 

### emq-auth-http 

Remove 'password' param from ACL and superuser requests (#66) 

## Version 2.3.4 

*Release Date: 2018-01-29*

### Bugfix and Enhancements 

Feature: Forward real client IP using a reverse proxy for websocket (#1335) 

Feature: EMQ node.name with link local ipv6 address not responding to ping (#1460) 

Feature: Add PROTO_DIST_ARG flag to support clustering via IPv6 address. (#1460) 

Bugfix: retain bit is not set when publishing to clients (when it should be set). (#1461) 

Bugfix: Can't search topic on web dashboard (#1473) 

### emq-sn 

Bugfix: CONNACK is not always sent to the client (emq-sn#67) 

Bugfix: Setting the port to ::1:2000 causes error (emq-sn#66) 

## Version 2.3.3 

*Release Date: 2018-01-08*

### Bugfix and Enhancements 

Add a full documentation for  emq.conf  and plugins. 

Repair a dead link in README - missing emq-lwm2m. (#1430) 

Subscriber with wildcard topic does not receive retained messages with sub topic has $ sign (#1398) 

Web Interface with NGINX Reverse Proxy not working. (#953) 

### emq-dashboard 

Add  dashboard.default_user.login  ,  dashboard.default_user.password  options to support configuring default admin. 

### emq-modules 

The emq-modules rewrite config is not right. (#35) 

### emq-docker 

Upgrade alpine to 3.7 (#31) 

### emq-packages 

Support ARM Platform (#12) 

## Version 2.3.2 

*Release Date: 2017-12-26*

### Bugfix and Enhancements 

Support X.509 certificate based authentication (#1388) 

Add proxy_protocol, proxy_protocol_timeout options for ws/wss listener. 

Cluster discovery etcd nodes key must be created manually. (#1402) 

Will read an incorrect password at the last line of emq_auth_username.conf (#1372) 

How can i use SSL/TLS certificate based client authentication? (#794) 

Upgrade the esockd library to v5.2. 

### esockd 

Improve the parser of proxy protocol v2. 

Add 'send_timeout', 'send_timeout_close' options. 

Rename esockd_transport:port_command/2 function to async_send/2. 

Add test case for esockd_transport:async_send/2 function. 

Add esockd_transport:peer_cert_subject/1, peer_cert_common_name/1 functions. 

### emq-auth-mysql 

Update depends on emqtt/mysql-otp. 

Fixed the issue that Cannot connect to MySQL 5.7 (#67). 

### emq-relx 

Fix mergeconf/3 appending line break error. (#152) 

### emq-sn 

Fix crash in emq_sn_gateway:transform() function which handles SUBACK. (#57) 

Define macro SN_RC_MQTT_FAILURE. (#59) 

### emq-web-hook 

Filter auth_failure client for disconnected hook. (#30) 

## Version 2.3.1 

*Release Date: 2017-12-03*

### Bugfix and Enhancements 

Remove the unnecessary transactions to optimize session management. 

Should not exit arbitrarily when clientid conflicts in mnesia. 

Change the default value of 'mqtt.session.enable_stats' to 'on'. 

The DUP flag should be set to 0 for all QoS0 messages. (emqttd#1319) 

Fix the 'no function clause' exception. (emqttd#1293) 

The retained flags should be propagated for bridge. (emqttd#1293) 

The management API should listen on 0.0.0.0:8080. (emqttd#1353) 

Fast close the invalid websocket in init/1 function. 

erlang:demonitor/1 the reference when erasing a monitor. (emqttd#1340) 

### emq-retainer 

Don't clean the retain flag after the retained message is stored. 

Add three CLIs for the retainer plugin. (emq-retainer#38) 

### emq-dashboard 

Refactor(priv/www): improve the  routing  page. (emq-dashboard#185) 

### emq-modules 

Turn off the  subscription  module by default. (emq-modules#26) 

### emq-sn 

Add an integration test case for sleeping device. 

Do not send will topic if client is kicked out. 

Prevent crash information in log when emq_sn_gateway getting timeout, since it is a possible procedure. 

### emq-relx 

Support node cookie value with  =  characters. (emq-relx#146) 

### mochiweb 

Improve Req:get(peername) funciton to support  x-forwarded-for  and  x-remote-port  . (emqtt/mochiweb#9) 

## Version 2.3.0 "Passenger's Log" 

*Release Date: 2017-11-20*

EMQ 2.3.0 is available now! EMQ R2.3.0 improved the PubSub design to avoid race-condition issue and optimized the message routing efficiency. The self-signed certificates for SSL released with EMQ has been updated. This release also comes with a new dashboard theme and improvement of API design. 

### Bugfix and Enhancements 

Fixed the issue that Retained message is not sent for Subscribe to existing topic. (emqttd#1314) 

Fixed the issue that The DUP flag MUST be set to 0 for all QoS0 messages.(emqttd#1319) 

Improve the pubsub design and fix the race-condition issue. (emqttd#PR1342) 

Crash on macOS High Sierra (emqttd#1297) 

### emq-dashboard Plugin (emq-dashboard#PR174) 

Upgraded the 'subscriptions' RESTful API. 

Improvement of the auth failure log. (emq-dashboard#59) 

### emq-coap Plugin (emq-coap#PR61) 

Replaced coap_client with er_coap_client. 

Fixed: correct the output format of coap_discover() to enable ".well-known/core". 

Refactor the coap_discover method. 

### emq-relx 

Upgraded the  bin/nodetool  script to fix the  rpcterms  command. 

### emq-web-hook Plugin 

Fixed the emq_web_hook plugin getting username from client.connected hook. (emq-web-hook#19) 

### emq-auth-jwt Plugin(emq-auth-jwt#PR15) 

Added test cases for emq_auth_jwt. 

Fixed jwt:decode/2 functions's return type. 

### emq-auth-mongo Plugin(emq-auth-mongo#PR92) 

Updated the default MongoDB server configuration. 

## Version 2.3-rc.2 

*Release Date: 2017-10-22*

### Bugfix 

Change the default logging level of  trace  CLI. (emqttd#1306) 

### emq-dashboard Plugin (emq-dashboard#164) 

Fix the 'Status' filters of plugins's management. 

Fix the URL Redirection when deleting an user. 

Compatible with IE,Safari,360 Browsers. 

## Version 2.3-rc.1 

*Release Date: 2017-10-12*

### Bugfix 

Fixed the issue that invalid clients can publish will message. (emqttd#1230) 

Fixed Dashboard showing no stats data (emqttd#1263) 

Fixed a rare occurred building failure (emqttd#1284) 

Support Persistence Logs for longer time (emqttd#1275) 

Fix for users APIs (emqttd#1289) 

Changed passwd_hash/2 function's return type (emqttd#1289) 

### emq-dashboard Plugin (emq-dashboard#154) 

Improved the Dashboard Interface of Monitoring/Management/Tools. 

Allow switching dashboard themes. 

Supoort both EN and CN languages. 

## Version 2.3-beta.4 

*Release Date: 2017-09-13*

### Highlights 

Released a new sexy dashboard. 

Add more RESTful APIs for manangement and monitoring. 

Configuring the broker through CLI or API without having to restart. 

### Bugfix 

Job for emqttd.service failed because the control process exited with error code. (emqttd#1238) 

Travis-CI Build Failing (emqttd#1221) 

Https listener of Dashboard plugin won't work (emqttd#1220) 

Service not starting on Debian 8 Jessie (emqttd#1228) 

### emq-dashboard 

  1. Support switching to other clustered node. 
  2. Configure and reboot the plugins on the dashboard. 
  3. A login page to replace the basic authentication popup window. 



### emq-coap 

1.Try to clarify the relationship between coap and mqtt in EMQ. (emq-coap#54). 

2.Fix crashes in coap concurrent test(gen-coap#3). 

## Version 2.3-beta.3 

*Release Date: 2017-08-21*

### Enhancements 

Add HTTP API for hot configuration. 

### Bugfix 

  1. Parse 'auth.mysql.password_hash' error when hot configuration reload (emq-auth-mysql#68) 
  2. Set 'auth.pgsql.server' error when hot configuration reload (emq-auth-pgsql#67) 
  3. Set 'auth.redis.server' and 'auth.redis.password_hash' error when hot configuration reload (emq-auth-redis#47) 
  4. Fixed the issue that when deleting retained message subscribed clients are not notified (emqttd#1207) 
  5. Support more parameters for hot configuration reload: 


  * mqtt.websocket_protocol_header = on 
  * mqtt.mqueue.low_watermark = 20% 
  * mqtt.mqueue.high_watermark = 60% 
  * mqtt.client.idle_timeout = 30s 
  * mqtt.client.enable_stats = off 



## Version 2.3-beta.2 

*Release Date: 2017-08-12*

EMQ R2.3-beta.2, a development release, is available now! This release introduces new HTTP Managment API, and supports Hot configuration of some parameters and plugins. 

The plugins which support Hot configuration: 

  * emq-stomp 
  * emq-coap 
  * emq-sn 
  * emq-lwm2m 
  * emq-dashboard 
  * emq-retainer 
  * emq-recon 
  * emq-web-hook 
  * emq-auth-jwt 
  * emq-auth-http 
  * emq-auth-mongo 
  * emq-auth-mysql 
  * emq-auth-pgsql 
  * emq-auth-redis 



### Enhancements 

  1. Introduce new HTTP management API. 
  2. Add ClientId parameter for HTTP Publish API. 
  3. Allow configuring keepalive backoff. 
  4. Remove the fullsweep_after option to lower CPU usage. 
  5. Authorize HTTP Publish API with clientId. 



### emq-sn Plugin (emq-sn#49) 

  1. Support CONNECT message in connected/wait_for_will_topic/wait_for_will_msg states. 
  2. Clean registered topic for a restarted client. 
  3. Bug fix of not clearing buffered PUBLISH messages received during asleep state as those messages are sent to client when client wakes up. 



### emq-auth-ldap Plugin (emq-auth-ldap#21) 

Improve the design LDAP authentication. 

### emq-coap Plugin (emq-coap#51) 

Support CoAP PubSub Specification ( [ https://www.ietf.org/id/draft-ietf-core-coap-pubsub-02.txt ](https://www.ietf.org/id/draft-ietf-core-coap-pubsub-02.txt) ) 

## Version 2.3-beta.1 

*Release Date: 2017-07-24*

EMQ R2.3-beta.1 is available now! This release supports automatic cluster node discovery and network partition autoheal. It supports automatically forming clusters of Erlang nodes using different strategies, such as IP Multicast, Etcd and Kubernetes. 

### Node Discovery and Autocluster 

EMQ R2.3 supports node discovery and autocluster with various strategies: 

Strategy |  Description                     
---------|----------------------------------
static   |  Autocluster by static node list 
mcast    |  Autocluster by UDP Multicast    
dns      |  Autocluster by DNS A Record     
etcd     |  Autocluster using etcd          
k8s      |  Autocluster on Kubernetes       



### Network Partition and Autoheal 

Enable autoheal of Network Partition by default: 
    
    
    cluster.autoheal = on

When network partition occurs, the following steps are performed to heal the cluster if autoheal is enabled: 

  1. Node reports the partitions to a leader node which has the oldest guid. 
  2. Leader node create a global netsplit view and choose one node in the majority as coordinator. 
  3. Leader node requests the coordinator to autoheal the network partition. 
  4. Coordinator node reboots all the nodes in the minority side. 



### Node down and Autoclean 

A down node will be removed from the cluster if autoclean is enabled: 
    
    
    cluster.autoclean = 5m

### LWM2M Protocol Support 

EMQ-LWM2M is a gateway plugin for EMQ，which implements most LWM2M features. MQTT client is able to access LWM2M device through emq-lwm2m plugin, by sending a command and reading its response. 

Lightweight M2M (LWM2M) is a set of protocols defined by the Open Mobile Alliance (OMA) for machine-to-machine (M2M) or Internet of Things (IoT) device management and communications 

### JWT Authentication 

EMQ R2.3 supports JWT(JSON Web Token) Authentication with [ emq-auth-jwt ](https://github.com/emqtt/emq-auth-jwt) plugin. 

### Retainer Plugin 

Retainer Plugin support 'disc_only' mode to store MQTT retained messages. 

### Debian 9 Package 

EMQ R2.3 released binary package for Debian 9. 

### Erlang/OTP R20 

EMQ R2.3 is compatible with Erlang/OTP R20, and all the binary packages are built on Erlang/OTP R20. 

## Version 2.2 "Nostalgia" 

*Release Date: 2017-07-08*

*Release Name: Nostalgia*

EMQ 2.2.0 is available now! EMQ R2.2 supports CoAP(RFC 7252), MQTT-SN protocols completely, and it is extensible with Web Hook, Lua Hook and Elixir Hook. 

Feature: Add 'listeners restart/stop' CLI command (emqttd#1135) 

Bugfix: Exit Code from emqttd_ctl (emqttd#1133) 

Bugfix: Fix spec errors found by dialyzer (emqttd#1136) 

Bugfix: Catch exceptions thrown from rpc:call/4 (emq-dashboard#128) 

Bugfix: Topic has been decoded by gen-coap, no conversion needed (emq-coap#43) 

## Version 2.2-rc.2 

*Release Date: 2017-07-03*

::: warning Warning
2.2-rc.2 requires Erlang/OTP R19.3+ to build. 
:::

### Bugfix and Enhancements 

Compatible with Erlang/OTP R20 (emq-relx#77) 

CoAP gateway plugin supports coap-style publish & subscribe pattern. (emq_coap#33) 

MQTT-SN gateway plugin supports sleeping device (emq_sn#32) 

Upgrade esockd and mochiweb libraries to support restarting a listener 

## Version 2.2-rc.1 

*Release Date: 2017-06-14*

### Bugfix and Enhancements 

Add a new listener for HTTP REST API (emqttd#1094) 

Fix the race condition issue caused by unregister_session/1 (emqttd#1096) 

Fix the issue that we cannot remove a down node from the cluster (emqttd#1100) 

Passed org.eclipse.paho.mqtt_sn.testing/interoperability tests (emq_sn#29) 

Fixed the issue that send http request and return non-200 status code, but AUTH/ACL result is denied (emq-auth-http#33) 

Fixed the issue that fail to stop listener (emq_stomp#24) 

Support using systemctl to manage emqttd service on CentOS 

## Version 2.2-beta.3 

*Release Date: 2017-05-27*

### Bugfix and Enhancements 

Call emit_stats when force GC (emqttd#1071) 

Update the default value of 'mqtt.mqueue.max_length' to 1000 (emqttd#1074) 

Update emq-auth-mongo READEME (emq-auth-mongo#66) 

Update default password field (emq-auth-mongo#67) 

Upgrade the mongodb library to v3.0.3 

Remove ‘check password===undefined && userName!== undefined’ (emq-dashboard#120) 

### emq_auth_redis Plugin 

Support 'HGET mqtt_user:%u password' for authentication query 

### emq_auth_mongo Plugin 

Support mongodb Cluster, Replica Set 

### Documentation 

Add 'Build on Windows' chapter 

## Version 2.2-beta.2 

*Release Date: 2017-05-20*

### Bugfix and Enhancements 

Add a 'websocket_protocol_header' option to handle WebSocket connection from WeChat (emqttd#1060) 

Assign username and password to MQTT-SN's CONNECT message (emqttd#1041) 

Allow for Content-Type:application/json in HTTP Publish API (emqttd#1045) 

emqttd_http.erl:data conversion (emqttd#1059) 

Seperate emq_sn from emqttd (emq-sn#24) 

Check St0's type, making it easier to debug crash problems (emq-lua-hook#6) 

Fix error: load xxx.lua (emq-lua-hook#8) 

Leave luerl alone as a rebar project (emq-lue-hook#9) 

Display websocket data in reverse order (emq-dashboard#118) 

priv/www/assets/js/dashboard.js:Fixed a typo (emq-dashboard#118) 

### Update README 

Update README of emq-auth-pgsql: add the 'ssl_opts' configuration (emq-auth-pgsql#56) 

Update README of emq-auth-mysql: fix the 'passwd_hash' typo (emq-auth-mysql#54) 

Update README of emq-auth-mongo: change 'aclquery' to 'acl_query' (emq-auth-mongo#63) 

### Elixir Plugin 

Add a new plugin [ emq-elixir-plugin ](https://github.com/emqtt/emq-elixir-plugin) to support Elixir language. 

## Version 2.2-beta.1 

*Release Date: 2017-05-05*

EMQ 2.2-beta.1 is now available. Many new features including Web Hook, Lua Hook and Proxy Protocol have been released in this version. 

### MQTT Listeners 

Support to configure multiple MQTT TCP/SSL listeners for one EMQ node. For example: 
    
    
    -------
    -- External TCP 1883 --> |     |
    | EMQ | -- Internal TCP 2883 --> Service
    -- External SSL 8883-->  |     |
    -------

Configure a listener in etc/emq.conf: 
    
    
    listener.tcp.${name}= 127.0.0.1:2883
    
    listener.tcp.${name}.acceptors = 16
    
    listener.tcp.${name}.max_clients = 102400

### Proxy Protocol V1/2 

The EMQ cluster is usually deployed behind a Load Balancer, such as HAProxy or NGINX: 
    
    
    -----
    |   |
    | L | --TCP 1883--> EMQ
    --SSL 8883--> |   |                |
    | B | --TCP 1883--> EMQ
    |   |
    -----

The LB can pass the source IP, port of the TCP connection on to EMQ cluster by Proxy Protocol. 

Enable Proxy Protocol support for MQTT Listener: 
    
    
    ## Proxy Protocol V1/2
    ## listener.tcp.${name}.proxy_protocol = on
    ## listener.tcp.${name}.proxy_protocol_timeout = 3s

### Web Hook Plugin 

The Web Hook plugin [ emq-web-hook ](https://github.com/emqtt/emq-web-hook) can trigger a webhook callback when a MQTT client connected to or disconnected from the broker, a MQTT message is published or acked. 

### Lua Hook Plugin 

The Lua Hook plugin [ emq-lua-hook ](https://github.com/emqtt/emq-lua-hook) make it possible to extend the broker and write business logic with Lua script. 

### Improve the Auth/ACL Chain 

We improved the Auth/ACL chain design in 2.2 release. The Auth request will be forwarded to next auth module if it is ignored by the current auth module: 
    
    
    --------------           -------------           --------------
    Client --> | Redis Auth | -ignore-> | HTTP Auth | -ignore-> | MySQL Auth |
    --------------           -------------           --------------
          |                       |                       |
         \|/                     \|/                     \|/
    allow | deny            allow | deny            allow | deny

### Support bcrypt password hash 

Enable the  bcrypt  password hash in auth module, for example: 
    
    
    auth.redis.password_hash = bcrypt

### API Breaking Change 

etc/emq.conf: 'mqtt.queue. *' changed to 'mqtt.mqueue.* ' 

### emq-dashboard 

Support 'Unsubscribe' action on WebSocket Page. 

## Version 2.1.2 

*Release Date: 2017-04-21*

Fix  emqttd_ctl sessions list  CLI 

Newline character in emq.conf causing error;(emqttd#1000) 

Fix crash caused by duplicated PUBREC packet (emqttd#1004) 

Unload the 'session.created' and 'session.teminated' hooks (emq-plugin-template) 

## Version 2.1.1 

*Release Date: 2017-04-14*

Localhost:8083/status returns 404 when AWS LB check the health of EMQ (emqttd#984) 

Https listener not working in 2.1.0 as in 2.0.7 (emq-dashboard#105) 

Fix mqtt-sn Gateway not working (emq-sn#12) 

Upgrade emq-sn Plugin (emq-sn#11) 

Upgrade emq-coap Plugin (emq-coap#21) 

## Version 2.1.0 

*Release Date: 2017-04-07*

The stable release of 2.1 version. 

Trouble with auth.mysql.acl_query (emq-auth-mysql#38) 

Filter the empty fields in ACL table (emq-auth-mysql#39) 

## Version 2.1.0-rc.2 

*Release Date: 2017-03-31*

Support pbkdf2 hash (emq-auth-mongo#46) 

Kickout the conflict WebSocket connection (emqttd#963) 

Correct licence in app.src (emqttd#958) 

SSL options to connect to pgsql (emq-auth-pgsql#41) 

## Version 2.1.0-rc.1 

*Release Date: 2017-03-24*

EMQ fails to start if run under a different linux user than that which first ran it (emqttd#842) 

Depend on emqtt/pbkdf2 to fix the building errors of Travis CI (emqttd#957) 

Depend on goldrush and emqtt/pbkdf2 to resolve the building errors (emqttd#956) 

Fix 'rebar command not found' (emq-relx#33) 

Compile error in v2.1.0-beta.2 (emq-relx#32) 

Support salt with passwords (emq-auth-mongo#11) 

Change the default storage_type to 'ram' (emq-retainer#13) 

## Version 2.1.0-beta.2 

*Release Date: 2017-03-13*

Cannot find AwaitingAck (emqttd#597) 

EMQ V2.1 crash when public with QoS = 2 (emqttd#919) 

Support pbkdf2 hash (emqttd#940) 

Add src/emqttd.app.src to be compatible with rebar3 (emqttd#920) 

Add more test cases (emqttd#944) 

CRASH REPORT Process \<0.1498.0> with 0 neighbours crashed with reason: {ssl_error,{tls_alert,"certificate unknown"}} in esockd_connection:upgrade (emqttd#915) 

auth.redis.password_hash = plain by default (emq-auth-redis#20) 

## Version 2.1.0-beta.1 

*Release Date: 2017-02-24*

EMQ v2.1.0-beta.1 is now available. 

::: warning Warning
EMQ 2.1+ Requires Erlang/OTP R19+ to build. 
:::

Since 2.1.0 release, we will tag EMQ versions accoding to the [ Semantic Versioning 2.0.0 ](http://semver.org) principles. And we will release EMQ versions monthly, odd number releases for bugfix and optimization, and even number releases for bugfix and new features. 

### Tuning GC 

  1. All the WebSocket, Client, Session processes will hiberante and GC after a period of idle time. 
  2. Add 'mqtt.conn.force_gc_count' configuration to force the Client, Session processes to GC when high message throughput. 
  3. Tune the 'fullsweep_after' option of WebSocket, Client, Session processes. 



### Hooks API 

Hooks module now support to register the same function with different tags. 

### Bugfix 

emqttd#916: Add 'mqtt_msg_from()' type 

emq-auth-http#15: ACL endpoint isnt called 

## Version 2.1-beta 

## Version 2.1-beta 

*Release Date: 2017-02-18*

EMQ v2.1-beta is now available. We improved the design of Session/Inflight and use one timer to redeliver the inflight QoS1/2 messages, and improved the GC mechanism of MQTT connection process to reduce CPU usage at the high rate of messages. 

### Per Client, Session Statistics 

Support Per Client, Session Statistics. Enable by configuration in etc/emq.conf: 
    
    
    mqtt.client.enable_stats = 60s
    
    mqtt.session.enable_stats = 60s

### Add 'missed' Metrics 

The 'missed' metrics will be increased when EMQ broker received PUBACK, PUBREC, PUBREL, PUBCOMP packets from clients, but missing in inflight window: 
    
    
    packets/puback/missed
    
    packets/pubrec/missed
    
    packets/pubrel/missed
    
    packets/pubcomp/missed

### Integrate Syslog 

Output EMQ log to syslog: 
    
    
    ## Syslog. Enum: on, off
    log.syslog = on
    
    ##  syslog level. Enum: debug, info, notice, warning, error, critical, alert, emergency
    log.syslog.level = error

### Upgrade QoS 

Support to upgrade QoS accoding to the subscription: 
    
    
    mqtt.session.upgrade_qos = on

### Add 'acl reload' CLI 

Reload acl.conf without restarting emqttd service (#885) 

### etc/emq.conf Changes 

  1. Rename mqtt.client_idle_timeout to mqtt.client.idle_timeout 
  2. Add mqtt.client.enable_stats 
  3. Add mqtt.session.upgrade_qos 
  4. Delete mqtt.session.collect_interval 
  5. Add mqtt.session.enable_stats 
  6. Rename mqtt.session.expired_after to mqtt.session.expiry_interval 



### Merge modules to emq_modules 

Merge the emq_mod_presence, emq_mod_subscription, emq_mod_rewrite into emq_modules 

Rename emq_mod_retainer to emq_retainer project 

### Dashboard Plugin 

Overview page: Add 'missed' metrics Client page: Add 'SendMsg', 'RecvMsg' Fields Session page: Add DeliverMsg, EnqueueMsg Fields 

### recon Plugin 

Change the datatype of 'recon.gc_interval' to duration 

### reloader Plugin 

Change the datatype of 'reloader.interval' to duration 

## Version 2.0.7 

*Release Date: 2017-01-20*

The Last Maintenance Release for EMQ 2.0, and support to build RPM/DEB Packages. 

Create the emq-package project: [ https://github.com/emqtt/emq-package ](https://github.com/emqtt/emq-package)

emq-auth-http#9: Update the priv/emq_auth_http.schema,  cuttlefish:unset()  if no super_req/acl_req config exists 

emq-auth-mongo#31:  cuttlefish:unset()  if no ACL/super config exists 

emq-dashboard#91: Fix the exception caused by binary payload 

emq-relx#21: Improve the  binemqttd.cmd  batch script for windows platform 

emqttd#873: Documentation: installing-from-source 

emqttd#870: Documentation: The word in Documents is wrong 

emqttd#864: Hook 'client.unsubscribe' need to handle 'stop' 

emqttd#856: Support variables in etc/emq.conf: {{ runner_etc_dir }}, {{ runner_etc_dir }}, {{ runner_data_dir }} 

## Version 2.0.6 

*Release Date: 2017-01-08*

Upgrade the [ esockd ](https://github.com/emqtt/esockd) library to v4.1.1 

esockd#41: Fast close the TCP socket if ssl:ssl_accept failed 

emq-relx#15: The EMQ 2.0 broker cannot run on Windows. 

emq-auth-mongo#31: Mongodb ACL Cannot work? 

## Version 2.0.5 

*Release Date: 2016-12-24*

emq-auth-http#9: Disable ACL support 

emq-auth-mongo#29: Disable ACL support 

emq-auth-mongo#30: {datatype, flag} 

## Version 2.0.4 

*Release Date: 2016-12-16*

emqttd#822: Test cases for SSL connections 

emqttd#818: trap_exit to link WebSocket process 

emqttd#799: Can't publish via HTTPS 

## Version 2.0.3 

*Release Date: 2016-12-12*

emqttd#796: Unable to forbidden tcp lisener 

emqttd#814: Cannot remove a 'DOWN' node from the cluster 

emqttd#813: Change parameters order 

emqttd#795: Fix metrics of websocket connections 

emq-dashboard#88: Rename the default topic from “/World” to “world” 

emq-dashboard#86: Lookup all online clients 

emq-dashboard#85: Comment the default listener port 

emq-mod-retainer#3: Retained messages get lost after EMQTT broker restart. 

## Version 2.0.2 

*Release Date: 2016-12-05*

emqttd#787: Stop plugins before the broker stopped, clean routes when a node down 

emqttd#790: Unable to start emqttd service if username/password contains special characters 

emq-auth-clientid#4: Improve the configuration of emq_auth_clientid.conf to resolve emqttd#790 

emq-auth-username#4: Improve the configuration of emq_auth_username.conf to resolve emqttd#790 

## Version 2.0.1 

*Release Date: 2016-11-30*

emqttd#781: Update README for EMQ 2.0 

emq_dashboard#84: Show the Cluster Status of Node 

emq_dashboard#79: disc_copies to store mqtt_admin table 

emq_auth_clientid: disc_copies to store mqtt_auth_clientid table 

emq_auth_username: disc_copies to store mqtt_auth_username table 

emq_mod_subscription#3: Remove emq_mod_subscription table and  module.subscription.backend  config 

emq_plugin_template#5: Unregister Auth/ACL modules when the plugin unloaded 

## Version 2.0 "West of West Lake" 

*Release Date: 2016-11-24*

*Release Name: West of West Lake*

The *EMQ* Version 2.0, named "West of West Lake", has been released with a lot of improvements and enhancements, and is ready to deploy in production now. 

  1. First of all, the *EMQ* broker now supports  Shared Subscription  and  Local Subscription  . 
  2. Supports CoAP(RFC 7252) and MQTT-SN protocol/gateway. 
  3. Adopt a more user-friendly  k = v  syntax for the new configuration file. 
  4. Add more hooks and new plugins, integrate with HTTP, LDAP, Redis, MySQL, PostgreSQL and MongoDB. 
  5. Cross-platform Builds and Deployment. Run the broker on Linux, Unix, Windows, Raspberry Pi and ARM platform. 



### Shared Subscription 

Shared Subscription supports Load balancing to distribute MQTT messages between multiple subscribers in the same group: 
    
    
    ---------
    |       | --Msg1--> Subscriber1
    Publisher--Msg1,Msg2,Msg3-->|  EMQ  | --Msg2--> Subscriber2
    |       | --Msg3--> Subscriber3
    ---------

Create a shared subscription with  $queue/  or  $share/\<group>/  prefix: 

                
                
> Prefix        
                
                |  Examples                             
----------------|---------------------------------------
$queue/         |  mosquitto_sub -t '$queue/topic       
$share/\<group>/ |  mosquitto_sub -t '$share/group/topic 



### Local Subscription 

The  Local Subscription  will not create global routes on clustered nodes, and only dispatch MQTT messages on local node. 

Usage: subscribe a topic with  $local/  prefix. 

### erlang.mk and relx 

The *EMQ* 2.0 adopts [ erlang.mk ](https://erlang.mk) and [ relx ](https://github.com/erlware/relx) tools to build the whole projects on Linux, Unix and Windows. 

### CoAP Support 

The *EMQ* 2.0 supports CoAP(RFC7252) protocol/gateway now, and supports communication between CoAP, MQTT-SN and MQTT clients. 

CoAP Protocol Plugin: [ https://github.com/emqtt/emqttd_coap ](https://github.com/emqtt/emqttd_coap)

### MQTT-SN Support 

The *EMQ* 2.0 now supports MQTT-SN protocol/gateway. 

MQTT-SN Plugin: [ https://github.com/emqtt/emq_sn ](https://github.com/emqtt/emq_sn)

### New Configuration File 

The release integrated with  cuttlefish  library, and adopted a more user-friendly  k = v  syntax for the new configuration file: 
    
    
    ## Node name
    node.name = emqttd@127.0.0.1
    ...
    ## Max ClientId Length Allowed.
    mqtt.max_clientid_len = 1024
    ...

The new configuration files will be preprocessed and translated to an Erlang  app.config  before the EMQ broker started: 
    
    
    ----------------------                                          2.0/schema/*.schema      -------------------
    | etc/emq.conf       |                   -----------------              \|/              | data/app.config |
    |       +            | --> mergeconf --> | data/app.conf | -->  cuttlefish generate  --> |                 |
    | etc/plugins/*.conf |                   -----------------                               | data/vm.args    |
    ----------------------                                                                   -------------------

### OS Environment Variables 

EMQ_NODE_NAME   |  Erlang node name                      
----------------|----------------------------------------
EMQ_NODE_COOKIE |  Cookie for distributed erlang node    
EMQ_MAX_PORTS   |  Maximum number of opened sockets      
EMQ_TCP_PORT    |  MQTT TCP Listener Port, Default: 1883 
EMQ_SSL_PORT    |  MQTT SSL Listener Port, Default: 8883 
EMQ_HTTP_PORT   |  HTTP/WebSocket Port, Default: 8083    
EMQ_HTTPS_PORT  |  HTTPS/WebSocket Port, Default: 8084   



### Docker Image 

We released an official Docker Image for *EMQ* 2.0. The open source project for Dockerfile: [ https://github.com/emqtt/emq_docker ](https://github.com/emqtt/emq_docker) . 

### Full Support for Windows 

The *EMQ* 2.0 fully supports Windows platform. You can run 'emqttd_ctl' command and cluster two nodes on Windows now. 

### Bugfix and Enhancements 

#764: add mqtt.cache_acl option 

#667: Configuring emqttd from environment variables 

#722:  mqtt/superuser  calls two times  emqtt_auth_http 

#754: "-heart" option for EMQ 2.0 

#741: emq_auth_redis cannot use hostname as server address 

### Plugins 

Plugin                                                                  |  Description                   
------------------------------------------------------------------------|--------------------------------
[ emq_dashboard ](https://github.com/emqtt/emqttd_dashboard)            |  Web Dashboard                 
[ emq_auth_clientid ](https://github.com/emqtt/emq_auth_clientid)       |  ClientId Auth Plugin          
[ emq_auth_username ](https://github.com/emqtt/emq_auth_username)       |  Username/Password Auth Plugin 
[ emq_auth_ldap ](https://github.com/emqtt/emq_auth_ldap)               |  LDAP Auth                     
[ emq_auth_http ](https://github.com/emqtt/emq_auth_http)               |  HTTP Auth/ACL Plugin          
[ emq_auth_mysql ](https://github.com/emqtt/emq_auth_mysql)             |  MySQL Auth/ACL Plugin         
[ emq_auth_pgsql ](https://github.com/emqtt/emq_auth_pgsql)             |  PostgreSQL Auth/ACL Plugin    
[ emq_auth_redis ](https://github.com/emqtt/emq_auth_redis)             |  Redis Auth/ACL Plugin         
[ emq_auth_mongo ](https://github.com/emqtt/emq_auth_mongo)             |  MongoDB Auth/ACL Plugin       
[ emq_mod_presence ](https://github.com/emqtt/emq_mod_presence)         |  Presence Module               
[ emq_mod_retainer ](https://github.com/emqtt/emq_mod_retainer)         |  Retainer Module               
[ emq_mod_rewrite ](https://github.com/emqtt/emq_mod_rewrite)           |  Topic Rewrite Module          
[ emq_mod_subscription ](https://github.com/emqtt/emq_mod_subscription) |  Subscription Module           
[ emq_coap ](https://github.com/emqtt/emq_coap)                         |  CoAP Protocol Plugin          
[ emq_sn ](https://github.com/emqtt/emq_sn)                             |  MQTT-SN Protocol Plugin       
[ emq_stomp ](https://github.com/emqtt/emq_stomp)                       |  STOMP Protocol Plugin         
[ emq_sockjs ](https://github.com/emqtt/emq_sockjs)                     |  STOMP over SockJS Plugin      
[ emq_recon ](https://github.com/emqtt/emq_recon)                       |  Recon Plugin                  
[ emq_reloader ](https://github.com/emqtt/emq_reloader)                 |  Reloader Plugin               
[ emq_plugin_template ](https://github.com/emqtt/emq_plugin_template)   |  Template Plugin               



## Version 2.0-rc.3 

*Release Date: 2016-11-01*

  1. Change the three modules(Presence, Retainer, Subscription) to standalone plugins: 

[ emq_mod_retainer ](https://github.com/emqtt/emq_mod_retainer)         |  Retained Message Storage                                                      
------------------------------------------------------------------------|--------------------------------------------------------------------------------
[ emq_mod_presence ](https://github.com/emqtt/emq_mod_presence)         |  Publish presence message to $SYS topics when client connected or disconnected 
[ emq_mod_subscription ](https://github.com/emqtt/emq_mod_subscription) |  Subscribe topics automatically when client connected                          



  2. Update the SSL certificates under the etc/certs/ folder. 
  3. Bugfix: Fixed a typo (#716) 
  4. Bugfix: emqttd_http can not use emq_auth_http? #739 
  5. Bugfix: emq_auth_redis cannot use hostname as server address (#741) 
  6. Upgrade Redis, MySQL, Postgre and MongoDB plugins to support hostname. 



## Version 2.0-rc.2 

*Release Date: 2016-10-19*

  1. A more user-friendly configuration for the EMQ broker. Integrate with  cuttlefish  library and adopt  K = V  syntax: 
    
        node.name = emqttd@127.0.0.1
    
    ...
    
    mqtt.listener.tcp = 1883
    
    ...

  2. Support OS Environments: 
    
        EMQ_NODE_NAME
    EMQ_NODE_COOKIE
    EMQ_MAX_PORTS
    EMQ_TCP_PORT
    EMQ_SSL_PORT
    EMQ_HTTP_PORT
    EMQ_HTTPS_PORT

  3. Refactor all the modules and plugins, and adopt new configuration syntax. 




TODO: issues closed. 

## Version 2.0-rc.1 

*Release Date: 2016-10-03*

  1. mqtt/superuser  POST called two times in  emqtt_auth_http  (#696) 

  2. Close MQTT TCP connection if authentication failed (#707) 

  3. Improve the plugin management. Developer don't need to add plugin's config to rel/sys.config 

  4. Add  BUILD_DEPS  in the plugin's Makefile: 
    
        BUILD_DEPS = emqttd
    dep_emqttd = git https://github.com/emqtt/emqttd emq20

  5. Improve the design of Redis ACL. 




## Version 2.0-beta.3 

*Release Date: 2016-09-18*

### New Features 

Shared Suscriptions (#639, #416): 
    
    
    mosquitto_sub -t '$queue/topic'
    mosquitto_sub -t '$share/group/topic'

Local Subscriptions that will not create global routes: 
    
    
    mosquitto_sub -t '$local/topic'

### Bugfix 

Error on Loading  emqttd_auth_http  (#691) 

Remove 'emqttd' application from dependencies (emqttd_coap PR#3) 

## Version 2.0-beta.2 

*Release Date: 2016-09-10*

### CoAP Support 

Release an experimental CoAP Gateway: [ https://github.com/emqtt/emqttd_coap ](https://github.com/emqtt/emqttd_coap)

### API Breaking Changes 

'$u', '$c' variables in emqttd.conf and modules/acl.conf changed to '%u', '%c' 

Improve the design of mqtt retained message, replace emqttd_retainer with emqttd_mod_retainer. 

Add 'session.subscribed', 'session.unsubscribed' hooks, remove 'client.subscribe.after' hook 

Tab 'retained_message' -> 'mqtt_retained' 

### Bugfix 

[2.0 beta1] FORMAT ERROR: "~s PUBLISH to ~s: ~p" (PR #671) 

Fixing issues in cluster mode. (PR #681) 

Fixing issues with unsubscribe hook (PR #673) 

## Version 2.0-beta.1 

*Release Date: 2016-08-30*

*Release Name: West of West Lake*

### EMQ - Shortened Project Name 

Adopt a shortened project name: EMQ(Erlang/Enterprise/Elastic MQTT Broker), E means Erlang/OTP, Enterprise and Elastic. 

### Improve the Release Management 

In order to iterate the project fast, we will adopt a new release management strategy since 2.0. There will be two or three 'Preview Release' named beta1, beta2 or beta3, and then one or two 'Release Candidate' named rc1, rc2 before a Major version is production ready. 

### Seperate Rel from Application 

We split the emqttd 1.x project into two projects since 2.0-beta1 release to resolve the plugins' dependency issue. 

A new project named [ emqttd-relx ](https://github.com/emqtt/emqttd-relx) is created and responsible for buiding the emqttd application and the plugins: 
    
    
    git clone https://github.com/emqtt/emqttd-relx.git
    
    cd emqttd-relx && make
    
    cd _rel/emqttd && ./bin/emqttd console

### erlang.mk and relx 

The rebar which is used in 1.x release is replaced by [ erlang.mk ](https://erlang.mk) and [ relx ](https://github.com/erlware/relx) tools since 2.0-beta1 release. 

You can check the 'Makefile' and 'relx.config' in the release project of the borker: [ emqttd-relx ](https://github.com/emqtt/emqttd-relx) . 

### Improve Git Branch Management 

stable     |  1.x Stable Branch       
-----------|--------------------------
master     |  2.x Master Branch       
emq10      |  1.x Developement Branch 
emq20      |  2.x Development Branch  
emq30      |  3.x Development Branch  
issue#{id} |  BugFix Branch           



### New Config Syntax 

Since 2.0-beta1 release the configuration file of the broker and plugins adopt a new syntax like rebar.config and relx.config: 

etc/emqttd.conf for example: 
    
    
    %% Max ClientId Length Allowed.
    {mqtt_max_clientid_len, 512}.
    
    %% Max Packet Size Allowed, 64K by default.
    {mqtt_max_packet_size, 65536}.
    
    %% Client Idle Timeout.
    {mqtt_client_idle_timeout, 30}. % Second

### MQTT-SN Protocol Plugin 

The MQTT-SN Protocol Plugin [ emqttd_sn ](https://github.com/emqtt/emqttd_sn) has been ready in 2.0-beta1 release. The default UDP port of MQTT-SN is 1884. 

Load the plugin: 
    
    
    ./bin/emqttd_ctl plugins load emqttd_sn

### Improve the PubSub Design 

![image](./_static/images/publish.png)

### Improve the Plugin Management 

The plugin of EMQ 2.0 broker is a normal erlang application which depends on and extends 'emqttd'. You can create a standalone plugin application project, and add it to [ emqttd-relx ](https://github.com/emqtt/emqttd-relx) Makefile as a DEP. 

All the plugins' config files will be copied to emqttd/etc/plugins/ folder when making emqttd brinary packages in [ emqttd-relx ](https://github.com/emqtt/emqttd-relx) project: 
    
    
    ▾ emqttd/
      ▾ etc/
        ▸ modules/
        ▾ plugins/
            emqtt_coap.conf
            emqttd.conf
            emqttd_auth_http.conf
            emqttd_auth_mongo.conf
            emqttd_auth_mysql.conf
            emqttd_auth_pgsql.conf
            emqttd_auth_redis.conf
            emqttd_coap.conf
            emqttd_dashboard.conf
            emqttd_plugin_template.conf
            emqttd_recon.conf
            emqttd_reloader.conf
            emqttd_sn.conf
            emqttd_stomp.conf

### EMQ 2.0 Documentation 

[ https://docs.emqx.io/broker/v2/en/index.html ](https://docs.emqx.io/broker/v2/en/index.html)

## Version 1.1.3 

*Release Date: 2016-08-19*

Support './bin/emqttd_ctl users list' CLI (#621) 

Cannot publish payloads with a size of the order 64K using WebSockets (#643) 

Optimize the procedures that retrieve the Broker version and Borker description in the tick timer (PR#627) 

Fix SSL certfile, keyfile config (#651) 

## Version 1.1.2 

## Version 1.1.2 

*Release Date: 2016-06-30*

Upgrade mysql-otp driver to 1.2.0 (#564, #523, #586, #596) 

Fix WebSocket Client Leak (PR #612) 

java.io.EOFException using paho java client (#551) 

Send message from paho java client to javascript client (#552) 

Compatible with the Qos0 PUBREL packet (#575) 

Empty clientId with non-clean session accepted (#599) 

Update docs to fix typos (#601, #607) 

## Version 1.1.1 

*Release Date: 2016-06-04*

Compatible with the Qos0 PUBREL packet (#575) 

phpMqtt Client Compatibility (#572) 

java.io.EOFException using paho java client (#551) 

## Version 1.1 

*Release Date: 2016-06-01*

### Highlights 

Upgrade eSockd library to 4.0 and Support IPv6 

Support to listen on specific IP Address: 
    
    
    {mqtt, {"192.168.1.20", 1883}, [
        ...
    ]},

Add MongoDB, HTTP Authentication/ACL Plugins 

Upgrade MySQL, PostgreSQL, Redis Plugins to support superuser authentication and avoid SQL Injection 

### Enhancements 

Allow human-friendly IP addresses (PR#395) 

File operation error: emfile (#445) 

emqttd_plugin_mongo not found in emqttd (#489) 

emqttd_plugin_mongo Error While Loading in emqttd (#505) 

Feature request: HTTP Authentication (#541) 

Compatible with the Qos0 PUBREL packet (#575) 

### Bugfix 

Bugfix: function_clause exception occurs when registering a duplicated authentication module (#542) 

Bugfix: ./emqttd_top msg_q result: {"init terminating in do_boot",{undef,[{etop,start,[],[]},{init,start_it,1,[]},{init,start_em,1,[]}]}} (#557) 

### Tests 

111 common test cases. 

### Dashboard Plugin 

WebSocket Page: Support 'Clean Session', Qos, Retained parameters (emqttd_dashboard#52) 

Upgrade eSockd library to 4.0, Show OTP Release on Overview Page (emqttd_dashboard#61) 

Changing dashboard credentials for username authentication (emqttd_dashboard#56) 

Add './bin/emqttd_ctl admins' CLI, support to add/delete admins 

### HTTP Auth Plugin 

Authentication/ACL by HTTP API: [ https://github.com/emqtt/emqttd_auth_http ](https://github.com/emqtt/emqttd_auth_http)

### MongoDB Plugin 

Upgrade Erlang MongoDB driver to v1.0.0 

Support superuser authentication 

Support ACL (emqttd_plugin_mongo#3) 

### MySQL Plugin 

Support superuser authentication 

Use parameterized query to avoid SQL Injection 

### Postgre Plugin 

Support superuser authentication 

Use parameterized query to avoid SQL Injection 

### Redis Plugin 

Support superuser authentication 

Support ClientId authentication by '%c' variable 

### Reloader Plugin 

Reload modified modules during development automatically. 

## Version 1.0.3 

*Release Date: 2016-05-23*

eSockd 3.2 

MochiWeb 4.0.1 

## Version 1.0.2 

*Release Date: 2016-05-04*

Issue#534 - './bin/emqttd_ctl vm' - add 'port/count', 'port/limit' statistics 

Issue#535 - emqttd_client should be terminated properly even if exception happened when sending data 

PR#519 - The erlang '-name' requires the fully qualified host name 

emqttd_reloader plugin - help reload modified modules during development. 

## Version 1.0.1 

*Release Date: 2016-04-16*

PR#515 - Fix '$queue' pubsub, add 'pubsub_queue' test and update docs 

## Version 1.0 (The Seven Mile Journey) 

*Release Date: 2016-04-13*

*Release Name: The Seven Mile Journey*

We finally released Version 1.0 (The Seven Mile Journey) with full documentation after two years' development and more than fifty iterations. 

The emqttd 1.0 implements a fully-featured, scalable, distributed and extensible open-source MQTT broker for IoT, M2M and Mobile applications: 

  1. Full MQTT V3.1/3.1.1 Protocol Specifications Support 
  2. Massively scalable - Scaling to 1 million connections on a single server 
  3. Distributed - Route MQTT Messages among clustered or bridged broker nodes 
  4. Extensible - LDAP, MySQL, PostgreSQL, Redis Authentication/ACL Plugins 



### Bugfix and Enhancements 

Possible race condition using emqttd_cm (#486) 

Improve the design of retained message expiration (#503) 

Do not expire the retained messages from $SYS/# topics (#500) 

### Documentation 

[ https://docs.emqx.io/broker/v2/en/index.html ](https://docs.emqx.io/broker/v2/en/index.html)

[ http://docs.emqtt.com/ ](http://docs.emqtt.com/)

### Thanks 

Thank Ericsson for the Great Erlang/OTP Platform ( [ http://erlang.org/ ](http://erlang.org/) )! 

Contributors on GitHub: @callbay @lsxredrain @hejin1026 @desoulter @turtleDeng @Hades32 @huangdan @phanimahesh @dvliman @Prots @joaohf 

Partners: EACG ( [ http://eacg.de/ ](http://eacg.de/) ) 

Favorite Band: The Seven Mile Journey ( [ http://www.thesevenmilejourney.dk/ ](http://www.thesevenmilejourney.dk/) ) 

## Version 0.17.1-beta 

*Release Date: 2016-03-22*

### Enhancements 

Time unit of session 'expired_after' changed to minute. (#479) 

### Dashboard 

Code Review and improve the design of Dashboard. 

## Version 0.17.0-beta 

*Release Date: 2016-03-15*

### Highlights 

Installation and Configuration Guide released on [ http://docs.emqtt.com ](http://docs.emqtt.com)

Improve and Consolidate the design of Hook, Server, PubSub and Router 

Upgrade the [Web Dashboard](https://github.com/emqtt/emqttd_dashboard) to support pagination 

Bridge emqttd broker to another emqttd broker & emqttd to mosquitto bridge (#438) 

### Enhancements 

emqttd_ctl: better error message (#450) 

./bin/emqttd_ctl: add 'routes' command: 
    
    
    routes list             # List all routes
    routes show \<Topic>     # Show a route

Add 'backend_subscription' table and support static subscriptions (emqttd_backend) 

Add 'retained_message' table and refactor emqttd_retainer module (emqttd_backend) 

A New Hook and Callback Design (emqttd_hook) 

Add PubSub, Hooks APIs to emqttd module (emqttd) 

Move start_listeners/0, stop_listeners/0 APIs to emqttd_app module (emqttd_app) 

### Tests 

Add 100+ common test cases. 

### Plugins 

Upgrade Dashboard, Redis, Stomp and Template Plugins 

## Version 0.16.0-beta 

*Release Date: 2016-02-16*

### Highlights 

Licensed under the Apache License, Version 2.0 Now. 

Improve the design of cluster, support to join or leave the cluster (#449): 
    
    
    $ ./bin/emqttd_ctl cluster
    cluster join \<Node>                     #Join the cluster
    cluster leave                           #Leave the cluster
    cluster remove \<Node>                   #Remove the node from cluster
    cluster status                          #Cluster status

Improve the design of Trie and Route, only the wildcard topics stored in Trie. 

Common Test to replace EUnit. 

### Enhancements 

mqtt_message record: add 'sender' field (#440) 

refactor the emqttd, emqttd_time, emqttd_opts, emqttd_node modules. 

### Bugfix 

noproc error when call to gen_server2:call(false, {add_route,Topic,\<0.685.0>}, infinity) (#446) 

### Plugins 

Changed the license of all plugins. 

## Version 0.15.0-beta 

*Release Date: 2016-01-31*

### Highlights 

Optimize for Push Application, 500K+ Subscribers to a Topic. 

Optimization for Route ETS insertion (#427) 

Priority Message Queue for Persistent Session (#432) 

Add Redis, MongoDB Plugins (#417) 

### Enhancements 

Username/Password Authentication: Support to configure default users (#428) 

Improve CLI Commands: pubsub, bridges, trace (#429) 

emqttd_mod_subscription: fix client_connected/3 

emqttd_auth_mod: add passwd_hash/2 function 

priority_queue: add plen/2, out/2 functions 

### Bugfix 

Fix dequeue/1 of emqttd_bridge... 

Add emqttd:seed_now/0 function 

### Plugins 

emqttd_plubin_mysql: Changed mysql driver to mysql-otp 

emqttd_plugin_pgsql: Integrate with ecpool 

emqttd_plugin_redis: First release 

emqttd_plugin_mongo: First release 

## Version 0.14.1-beta 

*Release Date: 2015-12-28*

Bugfix: emqttd_ws_client.erl: Unexpected Info: {'EXIT',\<0.27792.18>,{shutdown,destroy}} (#413) 

Improve: fix spec errors found by dialyzer 

## Version 0.14.0-beta 

*Release Date: 2015-12-18*

### Highlights 

Scaling to 1.3 Million Concurrent MQTT Connections on a 12 Core, 32G CentOS server. 

New PubSub, Router Design (#402). Prepare for scaling to 10 millions on one cluster. 

### Enhancements 

Improve the gproc_pool usage with a general emqttd_pool_sup 

Improve the design of emqttd_pubsub, add a new emqttd_router module 

Improve the design of the whole supervisor tree 

Route aging mechanism to remove the topics that have no subscriptions 

Improve the dashboard, mysql, pgsql, stomp, sockjs plugins 

Add 'topics', 'subscriptions' admin commands 

Avoid using mnesia table index and mnesia:index_read API to lower CPU usage 

Subscribe timeout exception (#366) 

Long Delay on Multiple Topic Subscription (#365) 

Subscriptions persistence (#344) 

emqttd_ctl: 'subscriptions' command to force clients to subscribe some topics (#361) 

### Bugfix 

emqttd_sm: spec of lookup_session/1 is not right BUG (#411) 

Observer application should be removed from reltool.config for 'wx' app is not available (#410) 

### Benchmark 

1.3 million concurrent MQTT connections on a 12 Core, 32G CentOS Server, consume about 15G Memory and 200% CPU. 

## Version 0.13.1-beta 

*Release Date: 2015-11-28*

Bugfix: Plugin pathes error under windows (#387) 

Improve: Too many error logs "[error] Session ..... Unexpected EXIT: client_pid=\<0.14137.35>, exit_pid=\<0.30829.22>, reason=nop..." (#383) 

Improve: Define QOS0/1/2, Pooler Error (PR#382) 

Improve: High CPU load when 400K unstable mobile connections (#377) 

BugFix: emqttd_plugin_pgsql - error using same query with latest update plugin (pgsql#5) 

## Version 0.13.0-beta 

*Release Date: 2015-11-08*

### Highlights 

Rate Limiting based on [Token Bucket](https://en.wikipedia.org/wiki/Token_bucket) and [Leaky Bucket](https://en.wikipedia.org/wiki/Leaky_bucket#The_Leaky_Bucket_Algorithm_as_a_Meter) Algorithm 

Upgrade eSockd and MochiWeb libraries to support Parameterized Connection Module 

Improve emqttd_client to support fully asynchronous socket networking 

### Enhancements 

Protocol Compliant - Session Present Flag (#163) 

Compilation fails if repo is cloned with a different name (#348) 

emqttd_client: replace gen_tcp:send with port_command (#358) 

TCP sndbuf, recbuf, buffer tuning (#359) 

emqttd_client.erl to handle 'inet_async', 'inet_reply' properly (#360) 

Refator the [client/session management design](https://github.com/emqtt/emqttd/blob/master/doc/design/ClientSession.md) 

### Bugfix 

Cannot kick transient client out when clientId collision (#357) 

Fix the order of emqttd_app:start_server/1 (#367) 

emqttd_session:subscribe/2 will crash (#374) 

### Benchmark 

[benchmark for 0.13.0 release](https://github.com/emqtt/emqttd/wiki/benchmark-for-0.13.0-release) 

3.1G memory and 50+ CPU/core: 
    
    
    Connections: 250K
    Subscribers: 250K
    Topics:      50K
    Qos1 Messages/Sec In:  4K
    Qos1 Messages/Sec Out: 20K
    Traffic In(bps):  12M+
    Traffic Out(bps): 56M+

## Version 0.12.3-beta 

*Release Date: 2015-10-22*

Bugfix: emqttd_sysmon crasher for 'undefined' process_info (#350) 

Bugfix: emqttd_client: catch parser exception (#353) 

## Version 0.12.2-beta 

*Release Date: 2015-10-16*

Bugfix: Retained messages should not be expired if 'broker.retained.expired_after = 0' (#346) 

## Version 0.12.1-beta 

*Release Date: 2015-10-15*

Highlight: Release for Bugfix and Code Refactor. 

Feature: Retained message expiration (#182) 

Improve: '$SYS/#' publish will not match '#' or '+/#' (#68) 

Improve: Add more metrics and ignore '$SYS/#' publish (#266) 

Improve: emqttd_sm should be optimized for clustered nodes may be crashed (#282) 

Improve: Refactor emqttd_sysmon and suppress 'monitor' messages (#328) 

Task: benchmark for 0.12.0 release (#225) 

Benchmark: About 900K concurrent connections established on a 20Core, 32G CentOS server. 

## Version 0.12.0-beta 

*Release Date: 2015-10-08*

### Highlights 

Enhance the **emqttd_ctl** module to allow plugins to register new commands (#256) 

Add [emqttd_recon plugin](https://github.com/emqtt/emqttd_recon) to debug/optimize the broker (#235) 

Add **'./bin/emqttd_ctl broker pubsub'** command to check the status of core pubsub processes 

Add **'./bin/emqttd_top'** command(like etop) to show the top 'msg_q', 'reductions', 'memory' or 'runtime' processes 

'rel/files/emqttd.config.production' for production deployment(default) 

'rel/files/emqttd.config.development' for development deployment 

### Enhancements 

Qos1/2 messages will not be dropped under unstable mobile network (#264) 

**emqttd_session:subscribe/2, emqttd_session:unsubscribe/2** APIs should be asynchronous (#292) 

**etc/emqttd.config** : 'idle_timeout' option to close the idle client(socket connected but no 'CONNECT' frame received) 

**etc/emqttd.config** : 'unack_retry_interval' option for redelivering Qos1/2 messages 

How to monitor large 'message_queue_len' (#283) 

### Bugfix 

Behaviour emqttd_auth_mod is missing init callback (#318) 

### Benchmark 

Write a new [benchmark tool](https://github.com/emqtt/emqtt_benchmark) to benchmark this release 

Hw requirements - 5K users, 25-50 msgs/sec, QoS=1 (#209) 

Supported Number of Connections Greatly Reduced When Clients are Subscribing (#324) 

## Version 0.11.0-beta 

*Release Date: 2015-09-25*

Highlight: Rebar to manage plugin dependencies. 

Highlight: [Stomp](https://github.com/emqtt/emqttd_stomp) and [SockJS](https://github.com/emqtt/emqttd_sockjs) Plugins! 

Improve: add rel/files/emqttd.config.development|production. 

Improve: rel/reltool.config.script to release deps of plugin. 

Improve: persist mnesia schema on slave nodes. 

Improve: use timer:seconds/1 api. 

Improve: The binary release will be compiled with R18.1 now. 

Bugfix: issue#306 - emqttd_cm should unregister the duplicated client 

Bugfix: issue#310 - usage of emqttd_ctl error: 'session list' should be 'sessions list' 

Bugfix: issue#311 - './bin/emqttd_ctl sessions list' error 

Bugfix: issue#312 - unsubcribe will lead to crash if emqttd_plugin_template plugin loaded 

## Version 0.10.4-beta 

*Release Date: 2015-09-18*

Optimize session management and upgrade eSockd library to 2.7.1 

[Benchmark for 0.10.4 release](https://github.com/emqtt/emqttd/wiki/benchmark-for-0.10.4-release) 

Improve: issue#294 - [error] failed to start connection on 0.0.0.0:1883 - enotconn 

Improve: issue#297 - How do I allow user with some pattern to access topic with some pattern? 

Bugfix: issue#291 - "./bin/emqttd attach ..." cannot work 

Bugfix: issue#284 - Should not use erlang:list_to_atom/1 in emqttd_vm.erl 

## Version 0.10.3-beta 

*Release Date: 2015-08-30*

Bugfix: issue#271 - add emqttd_ws_client:subscribe/2 function 

Bugfix: issue#269 - bin/emqttd Syntax error on ubuntu 

Improve: issue#265 - client under unstable mobile network generate a lot of logs 

## Version 0.10.2-beta 

*Release Date: 2015-08-26*

Improve: issue#257 - After the node name changed, the broker cannot restart for mnesia schema error. 

## Version 0.10.1-beta 

*Release Date: 2015-08-25*

Bugfix: issue#259 - when clustered the emqttd_dashboard port is close, and the 'emqttd' application cannot stop normally. 

Feature: issue#262 - Add ' [ http://host:8083/mqtt/status ](http://host:8083/mqtt/status) ' Page for health check 

## Version 0.10.0-beta 

*Release Date: 2015-08-20*

[Web Dashboard](https://github.com/emqtt/emqttd_dashboard) and [MySQL](https://github.com/emqtt/emqttd_plugin_mysql), [PostgreSQL](https://github.com/emqtt/emqttd_plugin_pgsql) Authentication/ACL Plugins! 

Highlight: Web Dashboard to monitor Statistics, Metrics, Clients, Sessions and Topics of the broker. 

Highlight: JSON/HTTP API to query all clients connected to broker. 

Highlight: A new [Plugin Design](https://github.com/emqtt/emqttd/wiki/Plugin%20Design) and a [Template project](https://github.com/emqtt/emqttd_plugin_template) for plugin development. 

Highlight: Authentication/ACL with MySQL, PostreSQl databases (#194, #172) 

Feature: Session Statistics including inflight_queue, message_queue, message_dropped, awaiting_rel, awaiting_ack, awaiting_comp (#213) 

Feature: Cookie based authentication for MQTT over websocket connections (#231) 

Feature: Get all clients connected to the broker (#228, #230, #148, #129) 

Feature: "./bin/emqttd_ctl clients show ClientId" to query client status (#226) 

Feature: "./bin/emqttd_ctl clients kick ClientId" to kick out a client 

Feature: "./bin/emqttd_ctl sessions list" to show all sessions 

Feature: "./bin/emqttd_ctl sessions show ClientId" to show a session 

Feature: Erlang VM metrics monitor with Web Dashboard (#59) 

Improve: Too many "inflight queue is full!" log when session is overloaded (#247) 

Improve: There are two many "MQueue(~s) drop ~s" logs if the message queue of session is small (#244) 

Improve: gen_server2(from RabbitMQ) to improve emqttd_session, emqttd_pubsub 

Improve: Makefile to build plugins 

Bugfix: emqttd_broker:unhook/2 cannot work (#238) 

Bugfix: emqttd plugin cannot include_lib("emqttd/include/emqttd.hrl") (#233) 

Bugfix: Too many 'Session ~s cannot find PUBACK' logs (#212) 

Bugfix: emqttd_pooler cannot work 

## Version 0.9.3-alpha 

*Release Date: 2015-07-25*

Wiki: [Bridge](https://github.com/emqtt/emqttd/wiki/Bridge) 

Improve: emqttd_protocol.hrl to define 'QOS_I' 

Improve: emqttd_pubsub to add subscribe/2 API 

Improve: ./bin/emqttd_ctl to support new bridges command 

Bugfix: issue #206 - Cannot bridge two nodes 

## Version 0.9.2-alpha 

*Release Date: 2015-07-18*

Improve: issue #196 - Add New Hook 'client.subscribe.after' 

## Version 0.9.1-alpha 

*Release Date: 2015-07-10*

Bugfix: issue #189 - MQTT over WebSocket(SSL) cannot work? 

Bugfix: issue #193 - 'client.ack' hook should be renamed to 'message.acked', and called by emqttd_broker:foreach_hooks 

## Version 0.9.0-alpha 

*Release Date: 2015-07-09*

[Session, Queue, Inflight Window, Hooks, Global MessageId and More Protocol Compliant](https://github.com/emqtt/emqttd/releases/tag/0.9.0-alpha) Now! 

Feature: Session/Queue/Inflight Window Design (#145). 

Feature: Support to resume a persistent session on other clustered node. 

Feature: Support alarm management. 

Feature: emqttd_guid to generate global unique message id. 

Feature: Hooks for message pub/ack. 

Feature: Protocol compliant - message ordering, timeout and retry. 

Improve: Every client will start_link a session process, whether or not the client is persistent. 

Improve: etc/emqttd.config to support more session, queue configuration. 

Improve: issue #179 - Max offline message queue {max_queue, 100} meaning. 

Improve: issue #180 - Should change project structure for other projects maybe depend on 'emqttd'. Merge emqtt, emqttd apps. 

Improve: issue #185 - PacketId and MessageId: the broker should generate global unique message id. 

Improve: issue #187 - etc/emqttd.config to support https listener 

Improve: issue #186 - emqttd_cm to store client details 

Improve: issue #174 - add 'from' field to mqtt_message record. 

Improve: issue #170 - $SYS Topics should support alarms. 

Improve: issue #169 - Add More [Hooks](https://github.com/emqtt/emqttd/wiki/Hooks-Design) 

Improve: issue #167 - Inflight window to assure message ordering. 

Improve: issue #166 - Message delivery timeout and retry. 

Improve: issue #143 - Qos1, Qos2 PubSub message timeout. 

Improve: issue #122 - Labeling message with unique id. emqttd_guid module to generate global unique msgid. 

Improve: emqttd_bridge to support pending message queue, and fix the wrong Qos design. 

Improve: mqtt_message record to add 'msgid', 'from' and 'sys' fields. 

Change: Add emqttd_mqueue, emqttd_guid, emqttd_alarm modules. 

Bugfix: issue #184 - emqttd_stats:setstats is not right. 

Bugfix: Closed issues #181, #119. 

Tests: fix the parser, acl test cases. 

## Version 0.8.6-beta 

*Release Date: 2015-06-17*

Bugfix: issue #175 - publish Will message when websocket is closed without 'DISCONNECT' packet 

## Version 0.8.5-beta 

*Release Date: 2015-06-10*

Bugfix: issue #53 - client will receive duplicate messages when overlapping subscription 

## Version 0.8.4-beta 

*Release Date: 2015-06-08*

Bugfix: issue #165 - duplicated message when publish 'retained' message to persistent client 

## Version 0.8.3-beta 

*Release Date: 2015-06-05*

Bugfix: issue #158 - should queue:in new message after old one dropped 

Bugfix: issue #155 - emqtt_parser.erl: parse_topics/3 should reverse topics 

Bugfix: issue #149 - Forget to merge plugins/emqttd_auth_mysql from 'dev' branch to 'master' in 0.8.x release 

## Version 0.8.2-alpha 

*Release Date: 2015-06-01*

Bugfix: issue #147 - WebSocket client cannot subscribe queue '$Q/queue/${clientId}' 

Bugfix: issue #146 - emqttd_auth_ldap: fill(Username, UserDn) is not right 

## Version 0.8.1-alpha 

*Release Date: 2015-05-28*

Client [Presence](https://github.com/emqtt/emqttd/wiki/Presence) Support and [$SYS Topics](https://github.com/emqtt/emqttd/wiki/$SYS-Topics) Redesigned! 

Bugfix: issue #138 - when client disconnected normally, broker will not publish disconnected $SYS message 

Bugfix: fix websocket url in emqttd/priv/www/websocket.html 

Improve: etc/emqttd.config to allow websocket connections from any hosts 

Improve: rel/reltool.config to exclude unnecessary apps. 

## Version 0.8.0-alpha 

*Release Date: 2015-05-25*

[Hooks](https://github.com/emqtt/emqttd/wiki/Hooks%20Design), Modules and [Plugins](https://github.com/emqtt/emqttd/wiki/Plugin%20Design) to extend the broker Now! 

Plugin: emqttd_auth_mysql - MySQL authentication plugin (issues #116, #120) 

Plugin: emqttd_auth_ldap - LDAP authentication plugin 

Feature: emqttd_broker to support Hooks API 

Feature: issue #111 - Support 'Forced Subscriptions' by emqttd_mod_autosub module 

Feature: issue #126 - Support 'Rewrite rules' by emqttd_mod_rewrite module 

Improve: Support hooks, modules to extend the broker 

Improve: issue #76 - dialyzer check 

Improve: 'Get Started', 'User Guide', 'Developer Guide' Wiki 

Improve: emqtt_topic to add join/1, feed_var/3, is_queue/1 

Improve: emqttd_pooler to execute common tasks 

Improve: add emqttd_sm_sup module, and use 'hash' gproc_pool to manage sessions 

Tests: add more test cases for 'emqttd' app 

## Version 0.7.1-alpha 

*Release Date: 2015-05-04*

Add doc/design/* and merge doc/* to github Wiki 

Bugfix: issue #121 - emqttd cluster issuse 

Bugfix: issue #123 - emqttd:unload_all_plugins/0 cannot unload any plugin 

Bugfix: fix errors found by dialyzer 

## Version 0.7.0-alpha 

*Release Date: 2015-05-02*

[MQTT over WebSocket(SSL)](https://github.com/emqtt/emqttd/wiki/MQTT-Over-WebSocket) Now! 

[Plugin Achitecture](https://github.com/emqtt/emqttd/wiki/Plugin%20Design) based on OTP application 

[Trace MQTT Packets or Messages](https://github.com/emqtt/emqttd/wiki/Trace%20Design) to log files 

Feature: issue #40, #115 - WebSocket/SSL Support 

Feature: issue #49, #105 - Plugin Architecture Support 

Feature: issue #93 - Trace API Design 

Improve: issue #109 - emqttd_broker should add subscribe, notify API 

Improve: update README.md to add 'Goals', 'Contributors' chapters 

Change: rename etc/app.config to etc/emqttd.config 

Change: etc/emqttd.config changed 

Bugfix: critical issue #54 - error when resume session! 

Bugfix: issue #118 - error report when UNSUBSCRIBE with no topics 

Bugfix: issue #117 - sys_interval = 0 config cannot work 

Bugfix: issue #112 - Makefile to support build plugins 

Bugfix: issue #96 - "make clean" cannot work 

## Version 0.6.2-alpha 

*Release Date: 2015-04-24*

Bugfix: critical issue #54, #104, #106 - error when resume session 

Improve: add emqttd_cm_sup module, and use 'hash' gproc_pool to register/unregister client ids 

Improve: kick old client out when session is duplicated. 

Improve: move mnesia dir config from etc/app.config to etc/vm.args 

## Version 0.6.1-alpha 

*Release Date: 2015-04-20*

Integrate with [gproc library](https://github.com/uwiger/gproc) to support pool 

Feature: issues#91 - should use worker_pool to handle some async work? 

Feature: issues#95 - Topic filters in ACL rule should support 'eq' tag 

Improve: issues#84 - emqttd_pubsub is redesigned again to protect mnesia transaction 

Improve: issues#74 - ACL Support and update [ACL Design Wiki](https://github.com/emqtt/emqttd/wiki/ACL-Design) 

## Version 0.6.0-alpha 

*Release Date: 2015-04-17*

ACL Support Now: [ACL-Design Wiki](https://github.com/emqtt/emqttd/wiki/ACL-Design) 

Authentication with username, clientid Now: [Authentication Wiki](https://github.com/emqtt/emqttd/wiki/Authentication) 

Seperate common MQTT library to 'emqtt' application 

Redesign message pubsub, route and retain modules 

Redesign mnesia database cluster 

Feature: issues#47 - authentication, authorization support 

Feature: issues#92 - merge emqttd_acl and emqttd_auth to emqttd_access_control 

Feature: emqttd_acl_mod, emqttd_auth_mod behaviour to extend ACL, authentication 

Feature: issues#85 - lager:info to log subscribe, unsubscribe actions 

Feature: issues#77 - authentication with clientid, ipaddress 

Improve: issues#90 - fix lager_file_backend log format, and rotate 10 log files 

Improve: issues#88 - use '-mneisa_create', '-mnesia_replicate' attributes to init mneisa 

Improve: issues#87 - record mqtt_user and mqtt_client is duplicated 

Improve: issues#81 - redesign nodes cluster to support disc_copies mnesia tables 

Improve: issues#80 - redesign emqttd_cm to handle more concurrent connections 

Improve: issues#70 - how to handle connection flood? Now could support 2K+ CONNECT/sec 

Change: redesign mnesia tables: message, topic, subscriber, trie, trie_node 

Bugfix: issues#83 - emqttd_broker stats cannot work 

Bugfix: issues#75 - careless about function name when emqttd_pubsub handle getstats message 

## Version 0.5.5-beta 

*Release Date: 2015-04-09*

Bugfix: issue #75 - careless about function name when emqttd_pubsub handle getstats message. 

Bugfix: issue #79 - cannot find topic_subscriber table after cluster with other nodes. 

## Version 0.5.4-alpha 

*Release Date: 2015-03-22*

Benchmark this release on a ubuntu/14.04 server with 8 cores, 32G memory from QingCloud.com: 
    
    
    200K Connections,
    30K Messages/Sec,
    20Mbps In/Out Traffic,
    200K Topics,
    200K Subscribers,
    
    Consumed 7G memory, 40% CPU/core

Benchmark code: [ https://github.com/emqtt/emqttd_benchmark ](https://github.com/emqtt/emqttd_benchmark)

Change: rewrite emqttd_pubsub to handle more concurrent subscribe requests. 

Change: ./bin/emqttd_ctl add 'stats', 'metrics' commands. 

Bugfix: issue #71, #72 

## Version 0.5.3-alpha 

*Release Date: 2015-03-19*

Bugfix: issues#72 - emqttd_cm, emqtt_sm ets:match_delete/2 with wrong pattern 

## Version 0.5.2-alpha 

*Release Date: 2015-03-18*

Change: upgrade esockd to 2.1.0-alpha, do not tune socket buffer for mqtt connection. 

## Version 0.5.1-alpha 

*Release Date: 2015-03-13*

Change: upgrade esockd to v1.2.0-beta, rename 'acceptor_pool' to 'acceptors' 

## Version 0.5.0-alpha 

*Release Date: 2015-03-12*

RENAME 'emqtt' to 'emqttd'! 

Support [Broker Bridge](https://github.com/emqtt/emqttd/wiki/Bridge-Design) Now! 

Change: rename project from 'emqtt' to 'emqttd' 

Change: lager:debug to dump RECV/SENT packets 

Feature: emqttd_bridge, emqttd_bridge_sup to support broker bridge 

Feature: emqtt_event to publish client connected/disconnected message to $SYS topics 

Feature: ./bin/emqttd_ctl add more commands: listeners, broker, bridges, start_bridge, stop_bridge... 

Feature: issue#57 - support to configure max packet size 

Feature: issue#68 - if sys_interval = 0, emqttd_broker will not publish messages to $SYS/brokers/# 

Bugfix: issue#67 - subscribe '#' to receive all messages 

Bugfix: issue#64 - emqtt_app start/2: should wait_for_databases 

Test: emqttd_topic_tests add more '_match_test' 

## Version 0.4.0-alpha 

*Release Date: 2015-03-10*

Support [$SYS Topics of Broker](https://github.com/emqtt/emqttd/wiki/$SYS-Topics-of-Broker) Now! 

Feature: emqtt_broker to publish version, uptime, datetime to $SYS/brokers/# topics 

Feature: emqtt_broker to publish count of clients, sessions, suscribers to $SYS/brokers/# topics 

Feature: emqtt_metrics to publish bytes, packets, messages metrics to $SYS/brokers/# topics 

Feature: add include/emqtt_systop.hrl 

Change: emqtt_cm to count current clients 

Change: emqtt_sm to count current sessions 

Change: emqtt_pubsub to count current topics and suscribers 

Change: emqtt_pubsub to add create/1 API 

Change: emqtt_pubsub dispatch/2 to return number of subscribers 

Change: emqtt_pubsub to count 'dropped' messages 

Change: emqtt_opts to add merge/2 function 

Test: add emqtt_serialiser_tests.erl 

## Version 0.3.4-beta 

*Release Date: 2015-03-08*

Bugfix: emqtt_serialiser.erl cannot serialise UNSUBACK packets 

## Version 0.3.3-beta 

*Release Date: 2015-03-07*

Bugfix: emqtt_serialiser.erl cannot serialise PINGRESP issue#60 

## Version 0.3.2-beta 

*Release Date: 2015-03-05*

Improve: merge emqttc serialiser, parser, packet 

Add: emqtt_opts to merge socket options 

## Version 0.3.1-beta 

*Release Date: 2015-03-02*

Feature: SSL Socket Support 

Feature: issue#44 HTTP API should add Qos parameter 

Bugfix: issue#52 emqtt_session crash 

Bugfix: issue#53 sslsocket keepalive error 

Upgrade: esockd to v0.2.0 

Upgrade: mochiweb to v3.0.0 

## Version 0.3.0-beta 

*Release Date: 2015-01-19*

Feature: HTTP POST API to support 'qos', 'retain' parameters 

Feature: $SYS system topics support 

Change: Rewrite emqtt_topic.erl, use '', '#', '+' to replace <<"">>, <<"#">>, <<"+">>

Change: fix emqtt_pubsub.erl to match '#', '+' 

Tests: emqtt_topic_tests.erl add more test cases 

## Version 0.3.0-alpha 

*Release Date: 2015-01-08*

NOTICE: Full MQTT 3.1.1 support now! 

Feature: Passed org.eclipse.paho.mqtt.testing/interoperability tests 

Feature: Qos0, Qos1 and Qos2 publish and suscribe 

Feature: session(clean_sess=false) management and offline messages 

Feature: redeliver awaiting puback/pubrec messages(doc: Chapter 4.4) 

Feature: retain messages, add emqtt_server module 

Feature: MQTT 3.1.1 null client_id support 

Bugfix: keepalive timeout to send will message 

Improve: overlapping subscription support 

Improve: add emqtt_packet:dump to dump packets 

Test: passed org.eclipse.paho.mqtt.testing/interoperability 

Test: simple cluster test 

Closed Issues: #22, #24, #27, #28, #29, #30, #31, #32, #33, #34, #36, #37, #38, #39, #41, #42, #43 

## Version 0.2.1-beta 

*Release Date: 2015-01-08*

pull request 26: Use binaries for topic paths and fix wildcard topics 

emqtt_pubsub.erl: fix wildcard topic match bug caused by binary topic in 0.2.0 

Makefile: deps -> get-deps 

rebar.config: fix mochiweb git url 

tag emqtt release accoding to [Semantic Versioning](http://semver.org/) 

max clientId length is 1024 now. 

## Version 0.2.0 

*Release Date: 2014-12-07*

rewrite the project, integrate with esockd, mochiweb 

support MQTT 3.1.1 

support HTTP to publish message 

## Version 0.1.5 

*Release Date: 2013-01-05*

Bugfix: remove QOS_1 match when handle PUBREL request 

Bugfix: reverse word in emqtt_topic:words/1 function 

## Version 0.1.4 

*Release Date: 2013-01-04*

Bugfix: fix "mosquitto_sub -q 2 ......" bug 

Bugfix: fix keep alive bug 

## Version 0.1.3 

*Release Date: 2013-01-04*

Feature: Support QOS2 PUBREC, PUBREL, PUBCOMP messages 

Bugfix: fix emqtt_frame to encode/decoe PUBREC/PUBREL messages 

## Version 0.1.2 

*Release Date: 2012-12-27*

Feature: release support like riak 

Bugfix: use ?INFO/?ERROR to print log in tcp_listener.erl 

## Version 0.1.1 

*Release Date: 2012-09-24*

Feature: use rebar to generate release 

Feature: support retained messages 

Bugfix: send will msg when network error 

## Version 0.1.0 

*Release Date: 2012-09-21*

The first public release. 
