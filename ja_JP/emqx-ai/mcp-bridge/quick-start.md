# EMQX MCP ブリッジを使用して IoT デバイスにアクセスする

本ガイドでは、EMQX MCP ブリッジを使用して MCP 対応モデルや AI エージェントと EMQX を統合し、IoT デバイスへのアクセスおよび制御を可能にする方法について説明します。

## 前提条件

EMQX サーバーがバージョン 5.7.0 以降でインストールおよび起動されていること。

## MCP ブリッジプラグインのインストールと設定

1. 以下のリンクから MCP ブリッジプラグインの最新バージョンをダウンロードします。  
   https://github.com/emqx/emqx_mcp_bridge/releases

2. 「プラグインのインストール」の手順に従い、EMQX サーバーにプラグインをインストールします。

3. プラグインの設定を行います。

   ブラウザで http://localhost:18083/#/plugins/ にアクセスし、MCP ブリッジプラグインをクリックして設定ページを開きます。ここで、リスニングアドレスや証明書などの設定を変更できます。**保存**をクリックすると設定が自動的に適用され、プラグインの手動再起動は不要です。

   リスニングアドレスを `https://your-hostname:9909/mcp` に設定した場合、MCP プラグインは指定ポート上に以下の 2 つの HTTP エンドポイントを起動します。

   - `/sse`：SSE プロトコルを使用する MCP 接続用  
   - `/mcp`：ストリーム可能な HTTP プロトコルを使用する MCP 接続用

   SSE プロトコルのみをサポートしたい場合は、リスニングアドレスを `https://your-hostname:9909/sse` に設定できます。

   また、一部のモデルや AI エージェントは MCP サーバーへの HTTPS アクセスを要求する場合があります。その場合は、MCP ブリッジプラグインに有効かつ信頼された SSL 証明書を設定し、URL が公開アクセス可能であることを確認してください。

   **ターゲット MQTT クライアント ID 取得方法**を **ツールパラメータ** に設定します。これにより、MCP クライアントはツール呼び出し時にデバイスの MQTT クライアント ID をパラメータとして渡せるようになり、接続確立時に HTTP ヘッダーで固定のクライアント ID を指定する必要がなくなります。

   ![MCP ブリッジプラグイン設定](./assets/mcp-bridge-config.png)

## MCP over MQTT SDK を使ったデバイスのシミュレーション

まず、[MCP SDK のインストール](../sdks/mcp-sdk-python.md) ガイドに従い、Python 用 MCP SDK をインストールします。

```bash
uv init smart_light
cd smart_light
uv add git+https://github.com/emqx/mcp-python-sdk --branch main
uv add "mcp[cli]"
source .venv/bin/activate
```

プロジェクトに `smart_light.py` ファイルを作成し、以下の内容を記述します。

```bash
# smart_light.py
import os
from mcp.server.fastmcp import FastMCP

status = "off"

# サーバーを作成
mcp = FastMCP(
    "devices/light",
    log_level="DEBUG",
    mqtt_server_description="ライトデバイスを制御するシンプルな FastMCP サーバーです。ライトのオン・オフや明るさの変更が可能です。",
    mqtt_client_id = os.getenv("MQTT_CLIENT_ID"),
    mqtt_options={
        "username": "aaa",
        "host": "localhost",
        "port": 1883,
    },
)

@mcp.tool()
def change_brightness(level: int) -> str:
    """ライトの明るさを変更します。レベルは 0 から 100 の範囲で指定してください。"""
    if 0 <= level <= 100:
        return f"明るさを {level} に変更しました"
    return "無効な明るさレベルです。0 から 100 の範囲で指定してください。"

@mcp.tool()
def turn_on() -> str:
    """ライトをオンにします"""
    global status
    if status == "on":
        return "OK、しかしライトはすでにオンです"
    status = "on"
    return "ライトをオンにしました"

@mcp.tool()
def turn_off() -> str:
    """ライトをオフにします"""
    global status
    if status == "off":
        return "OK、しかしライトはすでにオフです"
    status = "off"
    return "ライトをオフにしました"
```

上記の Python コードは、MCP over MQTT プロトコルを使用してスマートライトデバイスをシミュレートする MCP サーバーを起動します。ライトのオン・オフや明るさ調整の MCP ツールを公開しています。サーバー名は `devices/light` と指定しています。

次に、2 つのターミナルウィンドウで以下のコマンドを実行し、デバイス ID `abc123` と `abc456` の 2 台のデバイスをシミュレートする MCP サーバーを起動します。

```bash
MQTT_CLIENT_ID=abc123 mcp run -t mqtt ./smart_light.py
MQTT_CLIENT_ID=abc456 mcp run -t mqtt ./smart_light.py
```

## Cherry Studio クライアントでのテスト

ここでは、MCP 対応の Cherry Studio クライアントを MCP クライアントとして使用し、EMQX MCP ブリッジプラグインをテストします。

1. Cherry Studio クライアントをインストールするには、Cherry Studio のドキュメントを参照してください。  
   https://docs.cherry-ai.com/

2. **Model Provider** ページで LLM プロバイダーを追加し、モデルエンドポイント、API キーなど必要な情報を設定します。

   ![モデルプロバイダーの追加](./assets/cherry-studio-mcp-config-model-providers.png)

3. **MCP** ページで以下の設定で MCP サーバーを追加します。

   - 名前：`MQTT MCP Tools`  
   - 種類：SSE またはストリーム可能な HTTP（本例ではストリーム可能な HTTP を使用）  
   - URL：MCP ブリッジが提供するストリーム可能な HTTP エンドポイント `http://localhost:9909/mcp`  
   - ヘッダー：モデルに不要なツールを過剰に公開しないよう、`devices/light` タイプのツールのみを読み込むために以下のヘッダーを追加します。

     ```
     Tool-Types=devices/light
     ```

   ここで `devices/light` は、先述の Python 側デバイスコードで指定した MCP サーバー名です。

   Cherry Studio は HTTP と SSE の両プロトコルをサポートしています。ローカルテストでは `http://localhost:9909/mcp` を使用できます。

   ![MCP サーバーの追加](./assets/cherry-studio-mcp-config-mcp-bridge.png)

4. 「Device Assistant」という名前の新しいアシスタントを作成し、その中に「MQTT Device Control」という新しい会話トピックを作成します。アシスタントと会話トピックのシステムプロンプトを以下のように設定します。

   アシスタントのシステムプロンプト：

   ```
   あなたはデバイスアシスタントです。デバイスのアクセスおよび制御に関する質問にのみ回答してください。それ以外の質問には「私はデバイスアシスタントであり、他の質問には答えられません」と直接回答してください。
   ```

   会話のシステムプロンプト：

   ```
   以下のデバイスを所有しています：
   - リビングルームのライト、デバイス ID: abc123
   - 寝室のライト、デバイス ID: abc456
   ```

   会話設定で MCP ツールとして `MQTT MCP Tools` サーバーを有効にします。

   ![Device Assistant の作成](./assets/cherry-studio-mcp-control-devices.png)

5. 最後に、ツール呼び出しをサポートするモデル（例：`qwen-flash`）を選択します。チャットボックスに自然言語でコマンドを入力してデバイス制御をテストできます。

   ```
   リビングルームのライトをオンにして。
   寝室のライトの明るさを 75% に設定して。
   ```

   システムプロンプトに基づき、デバイスアシスタントが正しいデバイス ID を特定し、対応する MCP ツールを呼び出してデバイスを制御する様子が確認できます。
