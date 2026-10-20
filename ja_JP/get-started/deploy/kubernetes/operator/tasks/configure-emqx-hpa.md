# HPAによるレプリカントノードのオートスケール

KubernetesのHorizontalPodAutoscaler（HPA）を使用して、EMQXレプリカントノードを自動的にスケールします。EMQX Operator 3.0は、`apps.emqx.io/v3beta1` EMQXリソースのKubernetes `scale` サブリソースを公開しています。このサブリソースは `.spec.replicantTemplate.spec.replicas` を対象としているため、HPAはCore-Replicantクラスター内のレプリカントノードをスケールします。

## はじめに

KubernetesのリソースメトリクスAPIが利用可能であることを確認してください。このページのCPU使用率の例では、[Metrics Server](https://kubernetes-sigs.github.io/metrics-server/) または `metrics.k8s.io` データを提供する他のメトリクスアダプターが必要です。

Podメトリクスが利用可能かどうか、以下のコマンドを実行して確認してください。

```bash
kubectl top pods
```

CPUおよびメモリ使用量が返されない場合は、HPAを作成する前にメトリクスプロバイダーをインストールして設定してください。

レプリカントノードを有効にする場合は、Coreのレプリカを最低2つに設定してください。HPAが利用率を解析してスケールの判断を行うためには、Podのリソースリクエストも設定する必要があります。

## EMQXのデプロイ

1. 以下の内容を `emqx.yaml` として保存します。

   ```yaml
   apiVersion: apps.emqx.io/v3beta1
   kind: EMQX
   metadata:
     name: emqx
   spec:
     image: emqx/emqx:@EE_VERSION@
     config:
       roots:
         license:
           key: "..."
     coreTemplate:
       spec:
         replicas: 2
     replicantTemplate:
       spec:
         replicas: 4
         resources:
           requests:
             cpu: 500m
             memory: 1Gi
   ```

2. EMQXクラスターをデプロイします。

   ```bash
   kubectl apply -f emqx.yaml
   ```

3. EMQXクラスターが準備完了になるまで待ちます。

   ```bash
   kubectl wait --for=condition=Ready emqx/emqx
   ```

## HPAの作成

1. 以下の内容を `emqx-hpa.yaml` として保存します。この例ではCPU使用率を基にレプリカントノードを2〜10レプリカの範囲でスケールします。

   ```yaml
   apiVersion: autoscaling/v2
   kind: HorizontalPodAutoscaler
   metadata:
     name: emqx-replicants
   spec:
     scaleTargetRef:
       apiVersion: apps.emqx.io/v3beta1
       kind: EMQX
       name: emqx
     minReplicas: 2
     maxReplicas: 10
     metrics:
       - type: Resource
         resource:
           name: cpu
           target:
             type: Utilization
             averageUtilization: 70
     behavior:
       scaleDown:
         stabilizationWindowSeconds: 300
   ```

2. HPAを適用します。

   ```bash
   kubectl apply -f emqx-hpa.yaml
   ```

## オートスケールの確認

1. HPAとEMQXのステータスを確認します。

   ```bash
   kubectl get hpa emqx-replicants
   kubectl get emqx emqx -o custom-columns='DESIRED:.spec.replicantTemplate.spec.replicas,CURRENT:.status.replicantReplicas'
   ```

2. MQTTクライアントの負荷がない状態でHPAを監視します。観測されたCPU使用率が低く、HPAが希望するレプリカ数を2と計算した場合、`.spec.replicantTemplate.spec.replicas` が2に更新されていることを確認してください。設定されたスケールダウンの安定化ウィンドウにより、この更新が遅れることがあります。EMQX Operatorはその後、レプリカントセットをそのレプリカ数に調整します。

レプリカントノードのスケールダウンはMQTT接続やセッションの退避を伴い、クラスターに負荷をかけるため、HPAの安定化ウィンドウや保守的なスケーリングポリシーを利用して、負荷の変動による頻繁なレプリカ数の変動を避けてください。
