# EMQ X MQTT 微信小程序接入


微信小程序支持通过 WebSocket 进行即时通信，EMQ X 的 MQTT Over WebSocket 能够完全兼容使用在微信小程序上。

::: tip
由于微信小程序的规范限制，EMQ X 使用微信小程序接入时需要注意以下几点：

- 必须使用已经通过[域名备案](https://baike.baidu.com/item/%E5%9F%9F%E5%90%8D%E5%A4%87%E6%A1%88)的**域名**接入
- 域名需要在[小程序管理后台](https://mp.weixin.qq.com/wxamp/devprofile/get_profile)域名/IP 白名单中(开发 -> 开发设置 -> 服务器域名 -> socket 合法域名)
- 仅支持 WebSocket/TLS 协议，需要为域名分配受信任 CA 颁发的证书
- 由于微信小程序 BUG，安卓**真机必须**使用 TLS/443 端口，否则会连接失败（即连接地址不能带端口）
:::

## 参考资料

- EMQ X [使用 WebSocket 连接 MQTT 服务器](https://www.emqx.cn/blog/connect-to-mqtt-broker-with-websocket)
- CSDN [Nginx 反向代理 WebSocket](https://www.xncoding.com/2018/03/12/fullstack/nginx-websocket.html)
- 微信小程序 MQTT 接入[Demo](https://github.com/iAoe444/WeChatMiniEsp8266)


## 详细步骤

1、注册微信小程序帐号，并下载[微信开发者工具](https://developers.weixin.qq.com/miniprogram/dev/devtools/download.html)。由于微信小程序安全要求比较高，在与后台服务器之间的通讯必须使用 https 或 wss 协议，因此要在微信小程序后台设置域名服务器。

登录小程序后台后，找到 **开发设置 -> 服务器域名** 进行服务器配置，在 socket 合法域名处输入格式如 wss://xxx.emqx.io，此处不带端口号。


2、服务器端要申请并安装证书，华为云等云提供商均可以[申请免费证书](https://www.huaweicloud.com/product/scm.html)。

这里必须注意的是，证书申请绑定时，必须与所使用的服务器域名一致，此处建议使用 Nginx 来做反向代理并终结证书，相关配置如下：

```bash
server {
    listen  443 ssl;        
    server_name xxx.emqx.io; 
    ssl_certificate   cert/***.pem;
    ssl_certificate_key  cert/***.key;
    ssl_session_timeout  5m;      
    ssl_ciphers ECDHE-RSA-AES128-GCM-SHA256:ECDHE:ECDH:AES:HIGH:!NULL:!aNULL:!MD5:!ADH:!RC4;
    ssl_protocols TLSv1 TLSv1.1 TLSv1.2;
    ssl_prefer_server_ciphers on;

    # 添加反向代理
    location /mqtt {
      proxy_pass http://127.0.0.1:8083/mqtt;
      proxy_set_header Host $host;
      proxy_set_header X-Real-IP $remote_addr;
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
      # client_max_body_size 35m;
      proxy_http_version 1.1;
      proxy_set_header Upgrade $http_upgrade;
      proxy_set_header Connection "upgrade";    
    }

}
```

3、开源社区提供了小程序 MQTT 接入的 Demo：[https://github.com/iAoe444/WeChatMiniEsp8266](https://github.com/iAoe444/WeChatMiniEsp8266) 下载解压到一个文件夹后，用微信小程序开发者工具，打开 `index.js` 文件，**将 MQTT 地址、用户名和密码改为实际参数即可**。

按照以上 3 步的安装配置，你的微信小程序已经能够成功连接到 EMQ X 服务器了。
