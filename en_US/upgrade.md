# Upgrade 

## Upgrade to 3.1 

::: tip Tip
The 3.1 version newly designed the project architecture, configuration method and plug-in management method. 2.x and 1.x version upgrades require reconfiguration deployment. 
:::

Upgrade steps: 

  1. Download and install emqx-3.1 to the new directory, for example /opt/emqx-3.1/. 
  2. Refer to the old version of etc/vm.args, etc/emqttd.config or etc/emq.conf to configure version 3.1 of etc/emqx.conf. 
  3. Reconfigure etc/plugins/{plugin}.conf. 
  4. Edit the data/loaded_plugins, and add the plugins loaded in old installation. 
  5. Stop the old emqttd, and start the 3.1 installation. 



## Upgrade to 2.0.3 

Upgrade steps: 

  1. Download and extract the 2.0.3 version to the new installation directory, for example /opt/emqttd-2.0.3/. 
  2. The old version of the 'etc/' configuration file, the 'data/' data file is overwritten to the new version of the directory. 
  3. Stop the old emqttd, and start the 2.0.3 installation. 



## Upgrade to 2.0 

::: tip Tip
Cannot upgrade 1.x releases to 2.0 smoothly. 
:::

Upgrade steps: 

  1. Download and install emqttd-2.0 to the new directory, for example: 
    
         Old installation: /opt/emqttd-1.1.3/
    
         New installation: /opt/emqttd-2.0/

  2. Configure the etc/emq.conf for the 2.0 installation. 

  3. Configure etc/plugins/{plugin}.conf for the 2.0 new installation if you loaded plugins. 

  4. Edit the data/loaded_plugins, and add the plugins loaded in old installation. 

  5. Stop the old emqttd, and start the 2.0 installation. 




## Upgrade to 1.1.2 

::: tip Tip
1.0+ releases can be upgraded to 1.1.2 smoothly 
:::

Steps: 

  1. Download and install emqttd-1.1.2 to the new directory, for example: 
    
         Old installation: /opt/emqttd_1_0_0/
    
         New installation: /opt/emqttd_1_1_2/

  2. Copy the 'etc/' and 'data/' from the old installation: 
    
         cp -R /opt/emqttd_1_0_0/etc/* /opt/emqttd_1_1_2/etc/
    
         cp -R /opt/emqttd_1_0_0/data/* /opt/emqttd_1_1_2/data/

  3. Copy the plugins/{plugin}/etc/* from the old installation if you loaded plugins. 

  4. Stop the old emqttd, and start the new one. 



