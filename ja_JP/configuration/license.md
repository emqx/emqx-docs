# ライセンスの設定

`emqx ctl license update` コマンドを使用してライセンスを更新できます。このコマンドはライセンスファイルを更新し、ライセンスをリロードします。

```bash
emqx ctl license update <license_key>
```

または

```bash
# クラスター内のすべてのノードに同じファイルパスでライセンスファイルがアップロードされていることを確認してください。
emqx ctl license update 'file://<license_file_path>'
```

ここで、

- `<license_key>` は改行なしのライセンスキー文字列です。
- `<license_file_path>` はライセンスキーが保存されているファイルのパスです。

`'file://<license_file_path>'` 表記でライセンスを設定した場合、EMQX は2分ごとに自動的にライセンスをリロードします。

さらに、設定ファイルでライセンスの接続クォータ使用状況に関する設定を以下のように行うことができます。

```bash
license {
  key  =  "MjIwMTExCjAKMTAKRXZhbHVhdGlvbgpjb250YWN0QGVtcXguaW8KZGVmYXVsdAoyMDIzMDEwOQoxODI1CjEwMAo=.MEUCIG62t8W15g05f1cKx3tA3YgJoR0dmyHOPCdbUxBGxgKKAiEAhHKh8dUwhU+OxNEaOn8mgRDtiT3R8RZooqy6dEsOmDI="
  connection_low_watermark  =  "60%"
  connection_high_watermark  =  "80%"
  high_watermark_timezone   =  "system"
}
```

ここで、

- `key` は base64 形式でエンコードされたライセンスキーを含むフィールドです。
- `connection_low_watermark` はライセンス接続クォータ使用率のアラームを解除する閾値を設定します。デフォルトは `"75%"` です。
- `connection_high_watermark` はライセンス接続クォータ使用率のアラームを発動する閾値を設定します。デフォルトは `"80%"` です。
- `high_watermark_timezone` は日次のセッション高水準履歴を記録する際のローカル日の境界を決定するためのタイムゾーンです。`"system"` を指定するとノードホストのローカルタイムゾーンに従い、`"+08:00"` のような明示的なUTCオフセットを指定することも可能です。デフォルトは `"system"` です。

実行後、`emqx ctl license info` コマンドを実行して新しいライセンスファイルが反映されていることを確認できます。

::: tip

ライセンスファイルは EMQX ダッシュボードからも設定可能です。ダッシュボードでのライセンス設定方法については、[Work with License](../deploy/license.md) を参照してください。ダッシュボードで設定した内容は、設定ファイル内の同じ設定項目より優先されます。

:::

::: tip

EMQX はカスタマイズニーズに応じたより詳細な設定項目も提供しています。詳細は [EMQX Enterprise Configuration Manual for Enterprise](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/) をご覧ください。

:::
