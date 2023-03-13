# macOS

This section will guide you on how to install and start EMQX on macOS with a zip file.

Supported versions:

- macOS 12
- macOS 11

{% emqxce %}

The section below will take macOS 12 as an example to illustrate how to download the latest version of EMQX. If you want to install a different version or in a different system, please visit the [EMQX Deployment page](https://www.emqx.io/downloads?os=macOS). 

## Install EMQX

1. Download [emqx-5.0.20-macos12-arm64.zip](https://www.emqx.com/en/downloads/broker/5.0.20/emqx-5.0.20-macos12-arm64.zip). 

   

   ```bash
   wget https://www.emqx.com/en/downloads/broker/5.0.20/emqx-5.0.20-macos11-amd64.zip
   ```

2. Install EMQX. 

   ```bash
   mkdir -p emqx && unzip emqx-5.0.20-macos11-amd64.zip -d emqx
   ```

## Start EMQX

EMQX offers 3 different options to start EMQX:

- To start EMQX directly, run:

  ```bash
  $ emqx start
  EMQX 5.0.20 is started successfully!
  
  $ emqx_ctl status
  Node 'emqx@127.0.0.1' 5.0.20 is started
  ```

- To start EMQX with systemctl, run:

  ```bash
  sudo systemctl start emqx
  ```

- To start EMQX as a service, run:

  ```bash
  sudo service emqx start
  ```

## Uninstall EMQX

To uninstall EMQX, run:

```
sudo apt remove --purge emqx
```


{% endemqxce %}

{% emqxee %}

The section below will take macOS 12 as an example to illustrate how to download the latest version of EMQX. If you want to install a different version or in a different system, please visit the [EMQX Deployment page](https://www.emqx.com/en/try?product=enterprise). 

## Install EMQX

1.  Download [emqx-enterprise-5.0.1-macos12-arm64.zip](https://www.emqx.com/en/downloads/enterprise/5.0.1/emqx-enterprise-5.0.1-macos12-arm64.zip). 

   ```bash
   wget https://www.emqx.com/en/downloads/enterprise/5.0.1/emqx-enterprise-5.0.1-macos12-arm64.zip
   ```

2. Install EMQX.

   ```bash
   mkdir -p emqx && unzip emqx-enterprise-5.0.1-macos12-arm64.zip -d emqx
   ```

## Start EMQX

EMQX offers 3 different options to start EMQX:

- To start EMQX directly, run:

  ```bash
  $ emqx start
  EMQX 5.0.1 is started successfully!
  
  $ emqx_ctl status
  Node 'emqx@127.0.0.1' 5.0.1 is started
  ```

- To start EMQX with systemctl, run:

  ```bash
  sudo systemctl start emqx
  ```

- To start EMQX as a service, run:

  ```bash
  sudo service emqx start
  ```

## Uninstall EMQX

To uninstall EMQX, run:

  ```shell
sudo apt remove --purge emqx
  ```

{% endemqxee %}