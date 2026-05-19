# EMQX + GPT-Realtimeでリアルタイム音声エージェントを構築する

本ガイドでは、GPT-RealtimeモデルとEMQXを組み合わせてリアルタイム音声エージェントアプリケーションを素早く構築する方法を説明します。

## 一時的なAPIキーを取得する

ネイティブWebRTCを使ってブラウザからGPT-Realtimeに接続するには、まず一時的（エフェメラル）なAPIキーを取得する必要があります。このキーはOpenAIのREST APIを通じて生成できます。

```bash
export OPENAI_API_KEY="sk-xxxxxx"
curl -s -X POST https://api.openai.com/v1/realtime/client_secrets \
  -H "Authorization: Bearer $OPENAI_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{"session": {"type": "realtime", "model": "gpt-realtime"}}' | jq .value
```

## リアルタイム音声チャットを実装する

以下の例は、ネイティブWebRTCを使用してGPT-Realtimeモデルに接続し、リアルタイム音声チャットを実装する方法を示しています。

```javascript
// 取得したエフェメラルキーをここに設定
const EPHEMERAL_KEY = "ek_xxxxxx";

// ピアコネクションを作成
const pc = new RTCPeerConnection();

// モデルからのリモート音声を再生する設定
audioElement.current = document.createElement("audio");
audioElement.current.autoplay = true;
pc.ontrack = (e) => (audioElement.current.srcObject = e.streams[0]);

// ブラウザのマイク入力用にローカル音声トラックを追加
const ms = await navigator.mediaDevices.getUserMedia({
    audio: true,
});
pc.addTrack(ms.getTracks()[0]);

// イベント送受信用のデータチャネルを作成
const dc = pc.createDataChannel("oai-events");

// セッションをSession Description Protocol（SDP）で開始
const offer = await pc.createOffer();
await pc.setLocalDescription(offer);

const sdpResponse = await fetch("https://api.openai.com/v1/realtime/calls", {
    method: "POST",
    body: offer.sdp,
    headers: {
        Authorization: `Bearer ${EPHEMERAL_KEY}`,
        "Content-Type": "application/sdp",
    },
});

const answer = {
    type: "answer",
    sdp: await sdpResponse.text(),
};
await pc.setRemoteDescription(answer);

// サーバーからのイベントをリッスン
dc.addEventListener("message", (e) => {
    const event = JSON.parse(e.data);
    console.log("Received event:", event);
});
```

このコードはWebRTCの音声チャネルを作成するだけでなく、GPT-Realtimeモデルとのイベント送受信用にデータチャネルも作成しています。受信したすべてのイベントはコンソールにログ出力されます。テスト中に音声が出ないなどの問題が発生した場合は、詳細なエラー情報をコンソールで確認してください。

## MCPを使ったデバイス制御

1. EMQXを起動し、MCPブリッジプラグインをインストールおよび設定します。

2. スマートライトをシミュレートするMCPサーバーを起動します。詳細な手順は[EMQX MCPブリッジを使ってIoTデバイスにアクセスする](../../mcp-bridge/quick-start.md)を参照してください。

   なお、EMQXはパブリックネットワーク環境にデプロイされている必要があり、MCPブリッジプラグインは有効なSSL証明書で設定されている必要があります。これにより、GPT-RealtimeはHTTPS経由でMCPサービスにアクセスできます。

3. フロントエンドコードをMCPツール対応に変更します。

   MCPツールを有効にするには、GPT-Realtimeイベントを処理する`handle_event()`関数を追加します。

   ```javascript
   // サーバーからのイベントをリッスン
   dc.addEventListener("message", (e) => {
       const event = JSON.parse(e.data);
       handle_event(event);
   });
   ```

   この関数内で`session.created`イベントを処理し、セッション作成時に`session.update`イベントを送信してMCPツールを有効化します。MCPサーバーのアドレスは`https://your-emqx-host:port/mcp`に設定してください。

   ```javascript
   function handle_event(event) {
       if (event.type === "session.created") {
           // クライアントイベントを送信
           const session_update_event = {
               type: "session.update",
               session: {
                   type: "realtime",
                   model: "gpt-realtime",
                   // "text"に設定することも可能
                   output_modalities: ["audio"],
                   tools: [
                       {
                           type: "mcp",
                           server_label: "mqtt_mcp_bridge",
                           server_description: "EMQX MCP over MQTT Bridge",
                           server_url: "https://your-emqx-host:port/mcp",
                           require_approval: "never",
                       }
                   ],
                   tool_choice: "auto",
                   // 直接セッションフィールドを設定可能。プロンプトフィールドより優先されます:
                   instructions: "I have a smart light and its client ID is abc123"
               }
           };
           dc.send(JSON.stringify(session_update_event));
       } else if (event.type === "response.done") {
           console.log("Received response done:", event);
       } else {
           console.log("Received event:", event);
       }
   }
   ```

これでブラウザのフロントエンドページにアクセスし、GPT-Realtimeと音声会話を行うと、モデルがMCPツールを通じてIoTデバイスにアクセス・制御できるようになります。

::: tip
GPT-RealtimeはMCPサーバーにHTTPS経由でのみアクセス可能です。以下を必ず満たしてください。

- MCPプラグインが有効な自己署名でないSSL証明書で設定されていること
- URLがIPアドレスではなくドメイン名を使用し、パブリックにアクセス可能であること
:::

::: tip

GPT-RealtimeはMCPサーバーにアクセスする際にStreamable HTTPを必要とするため、EMQX MCPブリッジプラグインの`/mcp`エンドポイントを使用し、`/sse`エンドポイントは使用しないでください。

:::

## モデルへのメッセージ送信

前述のコードでは、システムインストラクションを使い、あらかじめデバイスのクライアントIDをモデルに通知していました。

```javascript
const session_update_event = {
    type: "session.update",
    session: {
        ...
        instructions: "I have a smart light and its client ID is abc123"
    }
};
```

GPT-Realtimeは会話中にWebRTCデータチャネルを通じてメッセージを送信し、コンテキスト情報を追加することもサポートしています。

```javascript
// クライアントイベントを送信
const event = {
    type: "conversation.item.create",
    item: {
        type: "message",
        role: "user",
        content: [
            {
                type: "input_text",
                text: "I have a smart light and its client ID is abc123",
            },
        ],
    },
};
dc.send(JSON.stringify(event));
```
