# Usage FAQs

## What happens when my license expires?

If you are an EMQX Enterprise user, when your license reaches its expiration date, a warning starts to appear each time the node is started to remind you of the expiration.  Depending on your license type, additional restrictions may apply:

- **For licenses issued for "small" customers or trial licenses:** No new MQTT connections are allowed, even if the total number of connections is less than the limit specified in the license.  Existing connections won't be disconnected, but they won't be able to reconnect if they drop.
- **For licenses not issued for "small" customers or trial licenses**: New MQTT connections are still permitted, as long as the total count remains below the maximum limit.

If you are unsure which type of license you have, please confirm with your account manager.

## How do I update my license?

You can use the following command to update your EMQX Enterprise License:

```bash
./bin/emqx ctl 

    license info             # Show license info 
    license update <License> # Update license given as a string
```

You can also update your license through the Dashboard. For how to apply for a license and update it through the Dashboard, see [Work with EMQX Enterprise License](../deploy/license.md).

## Why can't I receive retained messages when using shared subscriptions?

According to the MQTT protocol, when a client uses a shared subscription, the server is not allowed to send retained messages to that client.

## Why do messages sometimes get lost when using shared subscriptions?

When a shared subscriber's connection is disconnected but the session remains active, the server continues to deliver messages to the subscriber, which are temporarily stored in the session. As a result, other active shared subscribers may appear as if they have not consumed all the messages. In addition, if the shared subscriber chooses to create a new session when reconnecting, the messages cached in the old session will be permanently lost.

If it is confirmed that the above situation does not exist, yet the issue of message loss persists, you can use the [Log Trace](../observability/tracer.md) to conduct further investigation.

## How to troubleshoot the cause of SSL/TLS connection failure?

Usually when the SSL/TLS connection handshake fails, EMQX will output the corresponding failure reason in the [Log](../observability/log.md). The following are some common keywords in the log and their corresponding meanings:

- certificate_expired

   The `certificate_expired` keyword appears in the log, indicating that the certificate has expired, please renew it in time.

- no_suitable_cipher

   The `no_suitable_cipher` keyword appears in the log, indicating that a suitable cipher suite was not found during the handshake process. The possible reasons are that the certificate type does not match the cipher suite, the cipher suite supported by both the server and the client was not found, and so on.

- handshake_failure

   The `handshake_failure` keyword appears in the log. There are many reasons, which may be analyzed in conjunction with the error reported by the client. For example, the client may find that the connected server address does not match the domain name in the server certificate.

- unknown_ca

   The `unknown_ca` keyword appears in the log, which means that the certificate verification fails. Common reasons are that the intermediate CA certificate is omitted, the Root CA certificate is not specified, or the wrong Root CA certificate is specified. In the two-way authentication, we can judge whether the certificate configuration of the server or the client is wrong according to other information in the log. If there is a problem with the server certificate, the error log is usually:

   ```
   {ssl_error,{tls_alert,{unknown_ca,"TLS server: In state certify received CLIENT ALERT: Fatal - Unknown CA\n"}}}
   ```

   `CLIENT ALERT` means this is a warning message from the client. The server certificate fails the client's check.

   If there is a problem with the client certificate, the error log is usually:

   ```
   {ssl_error,{tls_alert,{unknown_ca,"TLS server: In state certify at ssl_handshake.erl:1887 generated SERVER ALERT: Fatal - Unknown CA\n"}}}
   ```

   `SERVER ALERT` means that the server finds that the client certificate cannot pass the authentication when checking the client certificate, and the client will receive this warning message from the server.

- protocol_version

   The `protocol_version` keyword appears in the log, indicating a mismatch between the TLS protocol versions supported by the client and server.

## How to troubleshoot abnormal disconnections on MQTT clients?

You can execute `emqx ctl listeners` in the Shell, which will return statistics of disconnections on each listener, including the reason for the disconnection and the number of disconnections. For example:

```
$ . /bin/emqx ctl listeners
tcp:default
  listen_on : 0.0.0.0:1883
  acceptors : 16
  proxy_protocol : false
  running : true
  current_conn : 9
  max_conns : infinity
  shutdown_count : [{keepalive_timeout,1},{idle_timeout,2}]
```

Here are some common disconnection reasons:

- `keepalive_timeout`: The client heartbeat timed out and the connection was closed by EMQX.
- `tcp_closed`: The client directly closed the TCP connection without sending a DISCONNECT packet.
- `frame_too_large`: The size of the packet sent by the client exceeds the maximum limit and the connection was closed by EMQX.
- `protocol_error`: EMQX closed the connection due to non-compliant behavior, e.g., the client sent multiple CONNECT packets within the same connection.
- `idle_timeout`: EMQX closed the connection because it did not receive a CONNECT packet from the client within 15 seconds of the TCP connection being established.

You can also use the [Log Trace](../observability/tracer.md) to trace all the logs related to the Client ID, IP, and topic you specify, and then you can analyze why the client disconnected based on these logs.

## When I was executing the stress test, the connection number and throughput were lower than expected. How can I tune the system to make full use of it?

When executing a stress test, besides ensuring the necessary hardware resources, it is also necessary to tune the OS and the Erlang VM to make maximum use of the resources. The most common tuning is to modify the global limitation of file handles, the user limitation of file handles, the TCP backlog and buffer, the limitation of process number of Erlang VM, and so on. You will also need to tune the client machine to ensure it has the ability and resources to handle all the subs and pubs.

Different use cases require different tuning. Refer to [Performance Tuning](../performance/tune.md) for tuning the system for general purposes.

## When I encounter problems related to client connection, publishing, and subscription, such as failure to connect, abnormal disconnection, etc., how should I troubleshoot?

EMQX's debug logs already capture all the behaviors and phenomena. By viewing the debug logs, we can determine when the client initiated the connection, the parameters specified during the connection, the success of rejection of the connection, and the reasons for rejection, among other details. However, the extensive information logged in debug mode can consume additional resources and make it challenging to analyze individual clients or topics.

To address this, EMQX provides a [Log Trace](../observability/tracer.md) feature. We can specify the clients or topics we want to trace, and EMQX will output all the debug logs related to those clients or topics to the designated log file. This facilitates self-analysis and seeking assistance from the community.

It's important to note that if the client cannot establish a connection with EMQX due to network issues, the log tracing feature will not be useful since EMQX does not receive any messages in such cases. This situation often arises from network configuration problems like firewalls or security groups, resulting in closed server ports. This is particularly common when deploying EMQX on cloud instances. Therefore, in addition to log tracing, troubleshooting network-related issues involves checking port occupation, listening status, and network configurations.

## Why are there client IDs like "CENSYS" or other unfamiliar clients?

CENSYS is an internet scanning and reconnaissance tool that performs regular scans of the IPv4 address space to identify default ports for various protocols such as HTTP, SSH, MQTT, and etc. Therefore, if you notice MQTT clients with a client ID of "CENSYS" or other unfamiliar clients accessing your MQTT broker, it indicates a relatively lower level of security protection. To address this issue effectively, consider implementing the following measures:

1. Avoid using default configurations, such as the AppID and AppSecret used for verifying HTTP API access permissions in EMQX.
2. Enable authentication mechanisms like password-based authentication or JWT authentication to prevent unauthorized access where only knowledge of an IP address is sufficient for login.
3. Enable TLS mutual authentication to allow access only to clients with valid certificates.
4. Enable proper authorization mechanisms to restrict access to sensitive data for unauthorized devices.
5. Configure your firewall to close unnecessary ports as much as possible.

## How to reset the Dashboard login password if I forget it?

The default username and password of the Dashboard are admin and public respectively. For security reasons, the Dashboard will force you to change your password when you log in for the first time. If you forget the password you set previously, you can use the following command to set a new password without providing the old password:

```
emqx ctl admin <Username> <New Password>
```
