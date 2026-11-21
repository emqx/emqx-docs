---
description: EMQXのよくあるタスクを確認し、ドキュメントをセクション別に参照できます。
---

<script setup>
const commonTasks = [
  {
    title: 'EMQXをインストール',
    description: 'パッケージ、Docker、またはKubernetesを使用してデプロイします。',
    link: './get-started/deploy/install.html'
  },
  {
    title: '認証を設定',
    description: 'EMQXへのMQTTクライアントアクセスを保護します。',
    link: './guides/access-control/authn/authn.html'
  },
  {
    title: 'クライアントを接続してメッセージングをテスト',
    description: 'MQTTクライアントを接続し、メッセージをパブリッシュおよびサブスクライブします。',
    link: './get-started/getting-started.html#mqttxで接続を検証する'
  },
  {
    title: 'EMQXをアップグレード',
    description: 'アップグレードを計画し、互換性に影響する変更を確認します。',
    link: './get-started/deploy/rolling-upgrades.html'
  }
]

const documentationSections = [
  {
    title: 'クイックスタート',
    description: 'EMQXの概要を確認し、インストールして最初のデプロイを完了します。',
    topics: ['概要とクイックスタート', 'インストールと移行'],
    link: './get-started/getting-started.html'
  },
  {
    title: 'ユーザーガイド',
    description: 'EMQXの設定、セキュリティ、運用、監視、保守を行います。',
    topics: ['設定とセキュリティ', '運用と可観測性'],
    link: './guides/user-guide.html'
  },
  {
    title: '開発者ガイド',
    description: 'クライアントの接続、データの処理、統合の構築、EMQXの拡張を行います。',
    topics: ['クライアントSDKとAPI', 'ルールとデータ統合'],
    link: './develop/developer-guide.html'
  },
  {
    title: 'リリースノート',
    description: 'バージョンごとの機能、修正、既知の問題、互換性のない変更を確認します。',
    topics: ['バージョン別リリースノート', '既知の問題と破壊的変更'],
    link: './release-notes/all-changes-ee.html'
  }
]
</script>

# ドキュメント概要

上部ナビゲーションでドキュメントセクションを切り替えるか、以下のよくあるタスクから始めます。左側のナビゲーションには、選択したセクションのトピックが表示されます。

## よくあるタスク

<div :class="$style.cardGrid">
  <a
    v-for="task in commonTasks"
    :key="task.title"
    :class="$style.card"
    :href="task.link"
  >
    <div :class="$style.cardContent">
      <strong :class="$style.cardTitle">{{ task.title }}</strong>
      <span :class="$style.cardDescription">{{ task.description }}</span>
    </div>
    <span :class="$style.arrow" aria-hidden="true">→</span>
  </a>
</div>

<p :class="$style.supportingText">これらのショートカットから、異なるドキュメントセクションの関連トピックに直接移動できます。</p>

## ドキュメントセクション

<div :class="$style.cardGrid">
  <a
    v-for="section in documentationSections"
    :key="section.title"
    :class="[$style.card, $style.sectionCard]"
    :href="section.link"
  >
    <div :class="$style.cardContent">
      <strong :class="$style.cardTitle">{{ section.title }}</strong>
      <span :class="$style.cardDescription">{{ section.description }}</span>
      <ul :class="$style.topicList">
        <li v-for="topic in section.topics" :key="topic">{{ topic }}</li>
      </ul>
    </div>
    <span :class="$style.arrow" aria-hidden="true">→</span>
  </a>
</div>

<p :class="$style.supportingText">セクションを選択すると、そのトピックが左側のナビゲーションに表示されます。</p>

<style module>
.cardGrid {
  display: grid;
  grid-template-columns: repeat(2, minmax(0, 1fr));
  gap: 16px;
  margin: 20px 0;
}

.cardGrid .card {
  display: flex;
  gap: 16px;
  align-items: flex-start;
  justify-content: space-between;
  min-height: 132px;
  padding: 22px 24px;
  color: var(--vp-c-text-1);
  text-decoration: none;
  background: var(--vp-c-bg-soft);
  border: 1px solid var(--vp-c-divider);
  border-radius: 12px;
  transition: border-color 0.2s, box-shadow 0.2s, transform 0.2s;
}

.cardGrid .card:hover {
  color: var(--vp-c-text-1);
  border-color: var(--vp-c-brand-1);
  box-shadow: var(--vp-shadow-2);
  transform: translateY(-2px);
}

.cardContent {
  display: flex;
  flex-direction: column;
  gap: 10px;
}

.cardTitle {
  font-size: 18px;
  line-height: 1.4;
}

.cardDescription,
.supportingText,
.topicList {
  color: var(--vp-c-text-2);
}

.cardDescription {
  line-height: 1.55;
}

.arrow {
  flex: 0 0 auto;
  color: var(--vp-c-brand-1);
  font-size: 22px;
  line-height: 1.4;
}

.sectionCard {
  min-height: 250px;
}

.topicList {
  margin: 2px 0 0;
  padding-left: 20px;
}

.topicList li + li {
  margin-top: 6px;
}

.supportingText {
  margin-top: 0;
}

@media (max-width: 720px) {
  .cardGrid {
    grid-template-columns: 1fr;
  }

  .card,
  .sectionCard {
    min-height: auto;
  }
}
</style>
