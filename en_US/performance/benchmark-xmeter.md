# Performance Testing with XMeter Cloud

This chapter will introduce how to use XMeter Cloud to perform performance testing on EMQX and obtain more comprehensive performance data, such as the average and P99 end-to-end latency of messages.

XMeter Cloud is a large-scale public cloud testing service for the IoT domain from EMQ. It is built on the open-source Apache JMeter project, and supports millions of simulated device connections and message throughput testing.

## Test Tools

You will need the following software or tools.

1. [EMQX](https://emqx.io/), an open-source, highly scalable MQTT Broker, designed specifically for IoT and real-time communication applications.
2. [XMeter Cloud](https://www.emqx.com/en/products/xmeter), a fully managed MQTT load testing cloud service, built on the Apache open source project JMeter, which can quickly run various MQTT load and scenario tests.
3. [collectd](https://github.com/collectd/collectd), a daemon running on the system that collects information such as CPU, memory, disk usage, and network traffic, and send these collected data to the designated storage.
4. [InfluxDB](https://www.influxdata.com/), an open-source time-series database for storing and analyzing time-series data.
5. [Grafana](https://grafana.com/grafana/), an open-source data visualization and monitoring tool that converts data from a variety of data sources into aesthetically pleasing charts, graphs, and warnings.

## Setting up the test environment

Firstly, you need to deploy two servers, one of which is used to run EMQX and collectd, and the other is used to run InfluxDB and Grafana. The hardware specifications of the former need to be adjusted according to the actual test scale, and the latter generally uses a 4-core machine. The server where EMQX is located must have a public IP so that XMeter Cloud can access and test. The public cloud is usually a good choice.

If you do not want the network latency of the wide area network to affect the final performance test results, you can create a peering connection between EMQX and XMeter. XMeter Cloud only supports peering with HUAWEI Cloud Platform now, you can contact the technical team of XMeter Cloud to help you do this. 

collectd is responsible for collecting system metrics such as CPU usage of the server where EMQX is located and then sends these metrics to InfluxDB deployed on the other server, which stores the data. Finally, Grafana uses InfluxDB as a data source to display these metrics as charts.

![img](./assets/test-architecture.png)

Next,  need to first complete the installation and configuration of these software on these two cloud servers, and then initiate the MQTT test from XMeter Cloud.

In this process, you will use some files, such as the template files for Grafana Dashboard and the test scripts used in XMeter Cloud, which you can find in the mqtt-test-kit directory under [bootcamp](https://github.com/emqx/bootcamp).

### 1. Install and configure EMQX

Download and install EMQX on Server 1:

```
wget https://www.emqx.com/en/downloads/broker/5.3.0/emqx-5.3.0-el7-amd64.rpm
sudo yum install emqx-5.3.0-el7-amd64.rpm -y
```

After the installation is complete, run the following command to start EMQX:

```
sudo systemctl start emqx
```

> The actual command may vary depending on the OS version and installation method. Please refer to commands provided on the [download](https://www.emqx.io/downloads) page.

### 2. Install and configure collectd

Install collectd on Server 1:

```
yum install collectd -y
```

You need to use the CPU, Load, Interface, Memory plugins, which are used to collect the system metrics of CPU usage, CPU load, network traffic, and memory usage, respectively. These plugins are enabled by default, and you can find the following configuration in the `/etc/collectd.conf`:

```
LoadPlugin cpu
...
LoadPlugin interface
...
LoadPlugin load
...
LoadPlugin memory
```

The CPU plugin for collectd reports CPU usage per core by default and uses CPU Jiffies. You need to add the following configuration in the config file to make it report the average percentage of all cores directly:

```
<Plugin cpu>
  ReportByCpu false
  ReportByState true
  ValuesPercentage true
</Plugin>
```

Next, you need to configure collectd's Network plugin to allow collectd to send the collected performance metrics to InfluxDB on another server. You need to add the following configuration to `/etc/collectd.conf` to enable the Network plugin and send the performance metrics to the specified host and port. `{Host}` needs to be replaced with the actual LAN IP of Server 2.:

```
LoadPlugin network
<Plugin network>
  Server "{Host}" "25826"
</Plugin>
```

After completing the above configuration, run the following command to start collectd:

```
systemctl start collectd
```

### 3. Install and configure InfluxDB

Install InfluxDB 1.8 on Server 2:

```
wget https://dl.influxdata.com/influxdb/releases/influxdb-1.8.10.x86_64.rpm
sudo yum localinstall influxdb-1.8.10.x86_64.rpm -y
```

Please don't install InfluxDB 2.7 or later versions. These versions no longer directly support backup write protocols such as collected and Prometheus. You must use Telegraf to convert these protocols into Line Protocol before writing them into InfluxDB. So for simplicity, please directly install InfluxDB 1.8 which supports the collectd write protocol.

After installation, you need to modify InfluxDB'configuration so that it can receive performance metrics sent by collectd and store them in the database. Open the InfluxDB configuration file /etc/influxdb/influxdb.conf, and change the configuration items in the collectd section to the following content.

```
[[collectd]]
  enabled = true
  bind-address = ":25826"
  database = "collectd"
  batch-size = 5000
  batch-pending = 10
  batch-timeout = "10s"
  read-buffer = 0
  typesdb = "/usr/share/collectd/types.db"
  security-level = "none"
  parse-multivalue-plugin = "split"
```

The above configuration means that InfluxDB will listen to the collectd data on port 25826 and write it into a database named collectd, which is automatically created by InfluxDB.

`typesdb` is required, it points to a `types.db` file that defines the collectd data source specification, which InfluxDB needs to understand the collectd data. You can get this file by installing collectd on your machine. `/usr/share/collectd/types.db` is the default path to the types.db file when you install collectd by yum, or you can get `types.db` from [here](https://github.com/emqx/bootcamp/blob/main/mqtt-test-kit/types.db).

Setting `security-level` to `none` means that collectd data will not be signed and encrypted, which is consistent with the collectd configuration.

Setting `parse-multivalue-plugin` to `split` means that InfluxDB will store data with multiple values as multiple data points.

Next, start InfluxDB:

```
sudo systemctl start influxdb
```

You can verify whether the collectd data has been correctly written into InfluxDB with the following command:


```
$ influx
Connected to http://localhost:8086 version 1.8.10
InfluxDB shell version: 1.8.10
> use collectd
Using database collectd
> select * from cpu_value limit 8
name: cpu_value
time                host     type    type_instance value
----                ----     ----    ------------- -----
1692954741571911752 ecs-afc3 percent user          0.049981257028614265
1692954741571917449 ecs-afc3 percent system        0.024990628514307132
1692954741571923666 ecs-afc3 percent wait          0.024990628514307132
1692954741571932372 ecs-afc3 percent nice          0
1692954741571943586 ecs-afc3 percent interrupt     0
1692954741571947059 ecs-afc3 percent softirq       0
1692954741571947389 ecs-afc3 percent steal         0
1692954741571949536 ecs-afc3 percent idle          99.90003748594276
```

### 4. Install and configure Grafana

Install Grafana on Server 2:

```
sudo yum install -y https://dl.grafana.com/oss/release/grafana-10.0.0-1.x86_64.rpm
```

Start Grafana:

```
systemctl start grafana-server
```

Next, you need to import a Dashboard into Grafana. This Dashboard will provide four monitoring panels for CPU usage, CPU load, memory usage, and network traffic. Click [here](https://github.com/emqx/bootcamp/blob/main/mqtt-test-kit/Grafana-Dashboard.json) to download the Dashboard template file.

Before importing the Dashboard, you need to make some modifications to `Grafana-Dashboard.json`. This is because each query in the Dashboard has an added host field to distinguish when there are multiple host data sources.

Search for `host::tag` in `Grafana-Dashboard.json`, and you will find the following content:

```
...
{
  "condition": "AND",
  "key": "host::tag",
  "operator": "=",
  "value": "ecs-afc3"
}
...
```

Just globally replace the host name `ecs-afc3` with your own host name. You can run the following command to view the hostname:

```
cat /proc/sys/kernel/hostname
```

Then, open a browser and type `<http://<hostname>>:3000` in the address bar to access Grafana, replacing `<hostname>` with the actual server address.

The default username and password for Grafana is admin. Grafana will ask us to change the default password when you log in for the first time. After logging in, you need to first add InfluxDB as the data source, click `Add your first data source` on the home page:

![img](./assets/add-your-first-data-source.png)

Find the InfluxDB data source, click to add this data source, and go to the configuration page:

![img](./assets/add-influxdb.png)

Here you only need to pay attention to three configuration items:

1. URL, InfluxDB's HTTP service listens on port 8086 by default, and InfluxDB and Grafana are on the same server, you can configure it as `http://localhost:8086`
2. Database, the database from which Grafana will read collectd data, so please configure it as `collected`.
3. HTTP Method specifies the HTTP method that Grafana will use to query InfluxDB for data, you can configure it as GET.

Click the `Save & test` button when you're done, and if the configuration is correct, you'll see a prompt `datasource is working. 7 measurements found`:

![img](./assets/save-and-test-influxdb.png)

Click the plus sign in the upper right corner and select `Import dashboard`:

![img](./assets/click-import-dashboard.png)

Import the modified `Grafana-Dashboard.json` file and select the InfluxDB data source you just added:

![img](./assets/import-dashboard.png)

Click `Import` button to complete the import, you will see the following four monitoring charts, which show the current server CPU usage, memory usage, network send/receive traffic, and CPU load changes, respectively:

![img](./assets/grafana-dashboard-example.png)

### 5. System Tuning

Depending on the actual scale of the test, you may also need to adjust Linux kernel parameters and EMQX parameters. For example, when the number of MQTT client connections exceeds 65535, you might need to adjust parameters such as `fs.file-max` to increase the maximum number of file handles that EMQX can open. And when the message throughput is large, you might also need to adjust the size settings of the send and receive buffers for better performance. You can refer to the [Performance Tuning](./tune.md) documentation, this section will not be expanded further.

### 6. Creating Tests in XMeter Cloud

After registering and logging in to [XMeter Cloud](https://www.emqx.com/en/products/xmeter) and going to the home page, you will see the **Standard** and the **Professional** available for selection. The specific differences between these two plans can be found in the [Product Plans](https://docs.emqx.com/en/xmeter-cloud/latest/price/plan.html) section of the XMeter Cloud documentation.

Taking the **Professional** as an example. After switching to the **Professional**, you will see two testing options: **MQTT Standard Scenario Test** and **Custom Scenario Test**.

In the **MQTT Standard Scenario Test**, XMeter Cloud has preset some standard scenarios, such as one-to-one, fan-out. You only need to simply configure parameters such as the number of publishing and subscribing clients, the message publishing rate to initiate the test.

In the **Custom Scenario Test**, You can implement tests for more complex scenarios, but you need to write JMeter test scripts yourself and upload them to XMeter Cloud. As a reference, you can download sample scripts [here](https://github.com/emqx/bootcamp/tree/main/mqtt-test-kit/scripts). The `Fan-In.jmx`, `Fan-Out.jmx`, `Symmetric.jmx`, `Symmetric-Bridge.jmx` correspond to fan-in, fan-out, symmetric, and bridge scenarios, respectively.

Each script provides custom variables for you to modify parameters such as QoS level, Payload size, message publishing rate. Taking the `Symmetric.jmx` script as an example, before submitting the test, you need to configure the following parameters:

![img](./assets/config-test-in-xmeter.png)

- **Name**: By default, XMeter Cloud will concatenate the test scenario name with the current time as the test name. You can change it to any name you prefer, as long as it does not confuse you among multiple tests.
- **Duration**: Set the duration of this test.
- **Total VU Number**: Set the number of virtual users per thread group, which is the number of MQTT clients, the thread groups depend on the actual content of the script. The `Symmetric.jmx` script includes a thread group Pub for publishing messages and a thread group Sub for receiving messages. So if the number of virtual users for both Pub and Sub thread groups is set to 1000, then the total number is 2000.
- **Stress Region**: Set the VPC where the test machine will be created and the load will be initiated. This needs to be configured only when using peer connections.
- **Ramp-Up Period**: Set how much time it needs to reach the total number of virtual users when running the test script. If the **Total VU Number** is set to 2000 and the **Ramp-Up Period** is set to 20 seconds, then XMeter Cloud will initiate connections at a rate of 100 connections per second.
- **Loop Mode**: `loop forever` means that the duration of the test run will be completely determined by the **Duration**; `use the loop count in the scenario` means that the test may end earlier than the time specified by **Duration**, but it will not exceed **Duration** at the longest.
- **XMeter Runtime Variables**: The variables defined in our test script are listed here,  which allow us to fine-tune our test cases by modifying them, such as changing the QoS level of the message. The following are the custom variables provided by the Symmetric.jmx script:
  - **server**: The address of the MQTT server, which needs to be configured as the server's intranet address after creating the peer connection.
  - **host**: The listening port of the MQTT server.
  - **qos**: The QoS level used when the message is published. The maximum QoS for subscribers is fixed at 2, ensuring that QoS degradation does not occur.
  - **payload_size**: The Payload size of the message in bytes.
  - **target_throughput**: The target throughput, which refers to the total publishing rate of the messages. When you set the number of virtual users in the publisher thread group to 1000 and **target_throughput** to 10000, then each publisher will publish the message at 10 msgs/s.
  - **publisher_number**, etc.: In XMeter Cloud, these variables are overridden by the previous configurations, such as **Total VU Number** and **Ramp-Up Period**. So there is no need to care about them. They are only effective when you launch the test directly using JMeter.

After completing the above configurations, you can click Next to submit the test. During the running of the test, you can observe the real-time changes of throughput and response time in XMeter Cloud, and observe the CPU and other system resources usage of the server where EMQX is located in Grafana:

![img](./assets/test-report-in-xmeter.png)
