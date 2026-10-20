# EMQX 設定の変更

## 目的

EMQX カスタムリソースの `.spec.config.roots` を通じて EMQX の設定を変更します。

## EMQX クラスターの設定

`apps.emqx.io/v3beta1` の EMQX CRD は、トップレベルの EMQX 設定ルートを `.spec.config.roots` に JSON 互換の値として受け入れます。YAML マニフェストでは、各ルートを [EMQX 設定スキーマ](https://docs.emqx.com/en/enterprise/v6.2.0/hocon/) に対応する構造化された YAML オブジェクト、配列、またはスカラーとして表現します。

このフィールドは、include や substitution などの HOCON 固有の構文は受け付けません。

`.spec.config.roots` からルートを削除すると、EMQX Operator はそのルートの管理を停止します。ただし、EMQX が永続化した値を削除したり、ルートをスキーマのデフォルトに戻したりはしません。既知の値にリセットするには、それらの値を明示的に宣言してください。

1. 以下を YAML ファイルとして保存し、`kubectl apply` でデプロイします。

   ```yaml
   apiVersion: apps.emqx.io/v3beta1
   kind: EMQX
   metadata:
     name: emqx
   spec:
     image: emqx/emqx:@EE_VERSION@
     imagePullPolicy: IfNotPresent
     config:
       roots:
         # ポート1884で `test` という名前の TCP リスナーを設定:
         listeners:
           tcp:
             test:
               bind: "0.0.0.0:1884"
               max_connections: 1024000
         license:
           key: "..."
     listenersServiceTemplate:
       spec:
         type: LoadBalancer
     dashboardServiceTemplate:
       spec:
         type: LoadBalancer
   ```

   ::: tip
   `node.cookie` は EMQX Operator が管理しているため、設定しないでください。
   :::

   ::: tip
   EMQX Operator は、リスナー設定などのほとんどの設定を [`base.hocon`](../../../../../guides/configuration/configuration.md#base-configuration-file) に書き込み、EMQX Configs API を通じて Pod の再起動なしにランタイムで変更を適用します。Dashboard リスナーやノード設定など、EMQX 起動時にのみ反映される設定は [`emqx.conf`](../../../../../guides/configuration/configuration.md#immutable-configuration-file) に書き込み、これらの変更は制御されたローリングアップデートをトリガーします。
   :::

2. EMQX クラスターが準備完了になるまで待ちます。`kubectl get` で EMQX クラスターの状態を確認し、`STATUS` が `Ready` であることを確認してください。準備完了までに時間がかかる場合があります。

   ```bash
   $ kubectl get emqx emqx
   NAME   STATUS   AGE
   emqx   Ready    10m
   ```

3. `ConfigApplied` 条件を確認し、目的の設定が有効になっていることを確認します。

   ```bash
   $ kubectl get emqx emqx -o jsonpath='{range .status.conditions[?(@.type=="ConfigApplied")]}{.status}{"\t"}{.reason}{"\t"}{.message}{"\n"}{end}'
   True    Applied    Desired configuration is active
   ```

## 設定の検証

EMQX のリスナーの状態を確認します。

```bash
$ kubectl exec -it emqx-core-0 -c emqx -- emqx ctl listeners
tcp:default
   listen_on: 0.0.0.0:1883
   acceptors: 16
   proxy_protocol : false
   running: true
   current_conn: 0
   max_conns : 1024000
tcp:test
   listen_on: 0.0.0.0:1884
   acceptors: 16
   proxy_protocol : false
   running: true
   current_conn: 0
   max_conns : 1024000
```

ここで、新しいポート1884のリスナーが稼働していることが確認できます。

## 再起動が必要な設定の変更

一部の設定変更は Pod テンプレートを更新し、ローリングアップデートをトリガーします。以下の例は、EMQX 起動時に反映される Dashboard HTTP リスナーの変更です。

1. EMQX リソースをパッチします。

   ```bash
   kubectl patch emqx emqx --type=merge -p '{"spec":{"config":{"roots":{"dashboard":{"listeners":{"http":{"bind":"0.0.0.0:18084"}}}}}}}'
   ```

2. EMQX Operator が変更を検知した後、`ConfigApplied` 条件を確認します。

   ```bash
   $ kubectl get emqx emqx -o jsonpath='{range .status.conditions[?(@.type=="ConfigApplied")]}{.status}{"\t"}{.reason}{"\t"}{.message}{"\n"}{end}'
   False    StartupConfigPending    Configuration roots require rolling restart: [dashboard]
   ```

   `False` かつ理由が `StartupConfigPending` の場合、少なくとも1つの準備完了 Pod が以前の設定を使用しており、ローリングアップデートが進行中であることを意味します。

3. ローリングアップデートが完了するまで待ちます。

   ```bash
   kubectl wait --for=condition=ConfigApplied emqx/emqx --timeout=10m
   ```

4. 新しい設定が有効になっていることを `ConfigApplied` 条件で確認します。

   ```bash
   $ kubectl get emqx emqx -o jsonpath='{range .status.conditions[?(@.type=="ConfigApplied")]}{.status}{"\t"}{.reason}{"\t"}{.message}{"\n"}{end}'
   True    Applied    Desired configuration is active
   ```
