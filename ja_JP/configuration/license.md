# ライセンス設定

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

`'file://<license_file_path>'` 表記でライセンスを設定した場合、EMQXは2分ごとに自動的にライセンスをリロードします。

さらに、設定ファイルでライセンスの接続クォータ使用率に関する設定を以下のように行うことができます。

```bash
license {
  key  =  "MjIwMTExCjAKMTAKRXZhbHVhdGlvbgpjb250YWN0QGVtcXguaW8KZGVmYXVsdAoyMDIzMDEwOQoxODI1CjEwMAo=.MEUCIG62t8W15g05f1cKx3tA3YgJoR0dmyHOPCdbUxBGxgKKAiEAhHKh8dUwhU+OxNEaOn8mgRDtiT3R8RZooqy6dEsOmDI="
  connection_low_watermark  =  "60%"
  connection_high_watermark  =  "80%"
}
```

ここで、

- `key` はbase64形式でエンコードされたライセンスキーを含むフィールドです。
- `connection_low_watermark` はライセンス接続クォータ使用率のアラームが非活性になる閾値を設定します。デフォルトは `"75%"` です。
- `connection_high_watermark` はライセンス接続クォータ使用率のアラームが活性になる閾値を設定します。デフォルトは `"80%"` です。

実行後、`emqx ctl license info` を実行して新しいライセンスファイルが適用されていることを確認できます。

::: tip

ライセンスファイルはEMQXダッシュボードからも設定可能です。ダッシュボードでのライセンス設定方法については、[Work with License](../deploy/license.md) を参照してください。ダッシュボードで設定した内容は、設定ファイルの同じ項目より優先されます。

:::

::: tip

EMQXはカスタマイズニーズに応じたより詳細な設定項目も提供しています。詳細は [EMQX Enterprise Configuration Manual for Enterprise](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/) をご覧ください。

:::
