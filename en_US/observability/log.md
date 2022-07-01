# Log

Logs provide a reliable source of information for troubleshooting and optimizing system performance. Logs can record business problems, such as login errors and abnormal access. It can also record information about events that occur in the operating system or network, including performance and fault detection.

EMQX activates the overload protection mechanism by default when the log data is too much or the log writing is too slow, which can ensure the normal business is not affected by the log.

## Log level

The logging of EMQX Broker is divided into 8 levels ([RFC 5424](https://www.ietf.org/rfc/rfc5424.txt)), which are shown from low to high as follows:

```bash
debug < info < notice < warning < error < critical < alert < emergency
```

The default logging level of EMQX is warning.

Logging configuration updates are performed in two ways.

1. Configure the configuration items under `log` in `emqx.conf`, which will take effect after restarting the node.
2. Configure it in the dashboard under `Configuration/Log`, save it and it will take effect without restarting the node.

EMQX supports outputting logs to console or log file, outputting console and file do not affect each other, and can be set at the same time.

## Console log Handler

When using the foreground start ( `. /bin/emqx console` or `/bin/emqx/foreground`), the console output of `warning` level logs is enabled and the log file output is disabled. Output to the console can make it easier for development debugging. Its configuration path is `log.console_handler`, and all configuration items are described as follows:

```
log.console_handler {
    ## Enable this log handler.   
    enable = false
    ## The log level for the current log handler.
    level = warning
    ## The time offset to be used when formatting the timestamp.
    ## Can be one of:
    ##   - system: the time offset used by the local system
    ##   - utc: the UTC time offset
    ##   - +-[hh]:[mm]: user specified time offset, such as "-02:00" or "+00:00"
    time_offset = system
    ## Set the maximum length of a single log message. If this length is exceeded, the log message will be truncated.
    chars_limit = unlimited
    ## Choose log formatter. `text` for free text, and `json` for structured logging.
    formatter = text
    ## Print logs in a single line if set to true. Otherwise, log messages may span multiple lines.
    single_line = true
    ## As long as the number of buffered log events is lower than this value,
    ## all log events are handled asynchronously. This means that the client process sending the log event,
    ## by calling a log function in the Logger API, does not wait for a response from the handler
    ## but continues executing immediately after the event is sent.
    ## It is not affected by the time it takes the handler to print the event to the log device.
    ## If the message queue grows larger than this value,
    ## the handler starts handling log events synchronously instead,
    ## meaning that the client process sending the event must wait for a response.
    ## When the handler reduces the message queue to a level below the sync_mode_qlen threshold,
    ## asynchronous operation is resumed.    
    sync_mode_qlen = 100
    ## When the number of buffered log events is larger than this value, the new log events are dropped.
    ## When drop mode is activated or deactivated, a message is printed in the logs.
    drop_mode_qlen = 3000
    ## If the number of buffered log events grows larger than this threshold, a flush (delete) operation takes place.
    ## To flush events, the handler discards the buffered log messages without logging.    
    flush_qlen = 8000
    overload_kill {
      ## Enable log handler overload kill feature.
      enable = true
      ## Maximum memory size that the log handler process is allowed to use.
      mem_size = 30MB
      ## Maximum allowed queue length.
      qlen = 20000
      ## If the handler is terminated, it restarts automatically after a delay specified in milliseconds. The value `infinity` prevents restarts.
      restart_after = 5s
     }
    burst_limit {
      ## Enable log burst control feature.
      enable = true
      ## Maximum number of log events to handle within a `window_time` interval. After the limit is reached, successive events are dropped until the end of the `window_time`.
      max_count = 10000
      window_time  =  1s
     }   
    ## Maximum depth for Erlang term log formatting and Erlang process message queue inspection.
    max_depth  =  100
   }
```

## File Log Handler

The default log file directory of EMQX is `./log` (for zip package installation) or `/var/log/emqx` (for binary package installation). The configuration of the file  log handler is mostly the same as that of the console handler, except for the additional file write control configuration, which is listed below.

```
 log.file_handlers.default {  
      ## Enable this file logging handler
      enable = true
      ## Logging level
      level = warning
      ## File path
      file = "log/emqx.log"     
      rotation {        
        ## Enable log rotation feature.               
        enable = true
        ## The maximum number of log files to rotate.
        count = 10
        }
      ## This parameter controls log file rotation. `infinity` means the log file will grow indefinitely, 
      ## otherwise the log file will be rotated when it reaches `max_size` (in bytes).
      max_size = 50MB 
 }
```

When file logging is enabled (log.to = file or both), the following files will appear in the log directory:

- **emqx.log.N:** log file prefixed with emqx.log, that contains all the log messages of EMQX Broker, such as `emqx.log.1`,` emqx.log.2` ...
- **emqx.log.siz and emqx.log.idx:** System files used to record log rotation information.
- **run_erl.log:** The system file used to record startup information when starting EMQX Broker in the background with `emqx start`.
- **erlang.log.N:** log file prefixed with erlang.log, which is a copy file of the console log when EMQX Broker is started in the background with `emqx start`, such as `erlang.log.1`,` erlang.log.2` ...

## Output log file for log level

If you want to write logs greater than or equal to a certain level to a separate file, you can configure `emqx.conf`  as :

```
log.file_handlers.my_info_log {  
      enable = true
      level = info
      file = "log/info.log"     
      rotation {        
        enable = true
        count = 10
        }
      max_size = 50MB      
 }
```

## Log format

The format of the log message (the fields are separated by spaces):

**date time level client_info module_info msg**

- **date:** Local data. The format is: YYYY-MM-DD
- **time:** Local time, accurate to milliseconds. The format is: hh:mm:ss.ms
- **level:** log level, wrapped in brackets. The format is:[Level]
- **client_info:** optional field, only exists if this log message is related to a client. The format is: ClientId@Peername or ClientId or Peername
- **msg:** log message content. The format is arbitrary and can contain spaces.

### Log message example 1:

```bash
2022-06-30T16:07:47.689512+08:00 [debug] clientid: test, line: 792, mfa: emqx_connection:handle_incoming/2, msg: mqtt_packet_received, packet: PINGREQ(Q0, R0, D0), payload: [], peername: 127.0.0.1:64391, tag: MQTT
```

The fields in this log message are:

- **datetime:** `2022-06-30T15:59:19.438914+08:00`
- **level:** `[debug]`
- **flat log-content:** `clientid: test, line: 792, mfa: emqx_connection:handle_incoming/2, msg: mqtt_packet_received, packet: PINGREQ(Q0, R0, D0), payload: [], peername: 127.0.0.1:64391, tag: MQTT`

This log indicates that EMQX received a `PINGREQ(Q0,R0,D0)` packet at `2022-06-30T16:07:47.689512+08:00` with clientid `test`. The IP of the client is `127.0.0.1:64391`.

### Log message example 2:

```bash
2022-06-30T16:25:32.446873+08:00 [debug] line: 150, mfa: emqx_retainer_mnesia:store_retained/2, msg: message_retained, topic: $SYS/brokers/emqx@127.0.0.1/sysdescr
```

The fields in this log message are:

- **date-time:** `2022-06-30T16:25:32.446873+08:00`
- **level:** `[debug]`
- **flat log-content:** `line: 150, mfa: emqx_retainer_mnesia:store_retained/2, msg: message_retained, topic: $SYS/brokers/emqx@127.0.0.1/sysdescr`

