# macOS

This section guides you on installing and starting EMQX on macOS with a zip file.

## Install EMQX

The section below explains how to install EMQX 4.3.19 on macOS. If you want to install a different version or in a different system, please visit the [EMQX Deployment page](https://www.emqx.com/en/try?product=enterprise).

1. Download [emqx-ee-macos-4.3.19-amd64.zip](https://www.emqx.com/en/downloads/enterprise/4.3.19/emqx-ee-macos-4.3.19-amd64.zip)

```bash
wget https://www.emqx.com/en/downloads/enterprise/4.3.19/emqx-ee-macos-4.3.19-amd64.zip
```

2. Install EMQX

```bash
unzip emqx-ee-macos-4.3.19-amd64.zip
```

## Start EMQX

EMQX offers 3 different options to start EMQX:

- To start EMQX directly, run:

  ```bash
  $ emqx start
  EMQX 4.3.19 is started successfully!
  
  $ emqx_ctl status
  Node 'emqx@127.0.0.1' 4.3.19 is started
  ```

- To start EMQX with systemctl, run:

  ```bash
  sudo systemctl start emqx
  ```

- To start EMQX as a service, run:

  ```bash
  sudo service emqx start
  ```
