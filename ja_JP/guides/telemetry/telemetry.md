---
next:
  text: 'Developer Guides'
  link: '../../develop/developer-guide'
---

# テレメトリ

EMQXはテレメトリ機能を通じて利用状況データを収集しています。この機能により、ユーザーコミュニティがEMQXをどのように利用しているかを把握し、使用パターンを理解することで製品の継続的な改善に役立てています。これらのメトリクスを共有することで、EMQXのパフォーマンスや機能向上に貢献いただけます。

プライバシーを最優先に考えており、テレメトリデータは匿名化されており、サーバーモデル、ハードウェアID、IPアドレスなどの個人や識別可能な情報は含まれていません。このデータが第三者に共有されることはありません。

テレメトリは商用ライセンスのユーザーではデフォルトで無効化されており、以下のライセンスタイプではデフォルトで有効になっています。

- EMQX Community
- 教育機関または非営利団体
- トライアル

これらのデフォルト設定はEMQXの設定ファイル内の `telemetry.enable` フラグを変更することで上書き可能です。例えば、設定ファイルでテレメトリを恒久的に無効化するには以下のように設定します。

```bash
telemetry.enable = false
```

また、起動時に環境変数でテレメトリの動作を制御することも可能です。

```bash
export EMQX_TELEMETRY__ENABLE=false && ./bin/emqx foreground
```

これらの設定をカスタマイズすることで、テレメトリデータの収集を完全に制御できます。

## テレメトリメトリクス

収集されるテレメトリデータには以下が含まれます。

- クラスターのハードウェア仕様（ハードウェアUUIDは除く）
- クラスターのトポロジー
- EMQXのバージョン
- 特定設定の有効状態
- 機能の有効化状況
- プラグインの使用状況
- メトリクス情報

テレメトリデータは暗号化されたHTTPリクエストを通じて `https://telemetry.emqx.io/api/telemetry` に送信されます。データは安全なプライベート環境に保存され、公開ネットワークには一切公開されません。

テレメトリデータ収集のコードは [apps/emqx_telemetry/src/emqx_telemetry.erl](https://github.com/emqx/emqx/blob/master/apps/emqx_telemetry/src/emqx_telemetry.erl) をご参照ください。ご質問がある場合は [Issues](http://github.com/emqx/emqx/issues) からお問い合わせください。

送信されるデータの例は以下の通りです：

```json
{
  "emqx_version": "5.0.9",
  "license": {
    "edition": "opensource"
  },
  "os_name": "macOS",
  "os_version": "12.5",
  "otp_version": "24",
  "up_time": 181903,
  "uuid": "5EAAF3C2-6186-11ED-AD7C-D5AAB80CED2E",
  "cluster_uuid": "5EAAF818-6186-11ED-AC1D-3DFDC18ED1BB",
  "nodes_uuid": [],
  "active_plugins": [],
  "num_clients": 0,
  "messages_received": 0,
  "messages_sent": 0,
  "build_info": {
    "wordsize": 64,
    "relform": "tgz",
    "os": "macos11",
    "erlang": "24.2.1-1",
    "elixir": "none",
    "arch": "x86_64-apple-darwin20.6.0"
  },
  "vm_specs": {
    "num_cpus": 8,
    "total_memory": 8589934592
  },
  "mqtt_runtime_insights": {
    "num_topics": 0,
    "messages_sent_rate": 0,
    "messages_received_rate": 0
  },
  "advanced_mqtt_features": {
    "topic_rewrite": 0,
    "retained": 3,
    "delayed": 0,
    "auto_subscribe": 0
  },
  "authn_authz": {
    "authz": [
      "file"
    ],
    "authn_listener": {},
    "authn": []
  },
  "gateway": {},
  "rule_engine": {
    "num_rules": 1
  },
  "bridge": {
    "num_data_bridges": 1,
    "data_bridge": {
      "webhook": {
        "num_linked_by_rules": 1,
        "num": 1
      }
    }
  },
  "exhook": {
    "servers": [],
    "num_servers": 0
  }
}
```
