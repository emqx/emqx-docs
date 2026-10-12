---
description: 查找 EMQX 常用任务，并按文档分区浏览内容。
---

<script setup>
const commonTasks = [
  {
    title: '安装 EMQX',
    description: '选择软件包、Docker 或 Kubernetes 部署方式。',
    link: './deploy/install.html'
  },
  {
    title: '配置认证',
    description: '保护 MQTT 客户端对 EMQX 的访问。',
    link: '../guides/access-control/authn/authn.html'
  },
  {
    title: '与 Kafka 集成',
    description: '在 EMQX 和 Kafka 之间传输 MQTT 数据。',
    link: '../develop/data-integration/data-bridge-kafka.html'
  },
  {
    title: '升级 EMQX',
    description: '规划升级并查看兼容性变更。',
    link: './deploy/rolling-upgrades.html'
  }
]

const documentationSections = [
  {
    title: '快速开始',
    description: '了解 EMQX、完成安装并开始首次部署。',
    topics: ['概览和快速开始', '安装和迁移'],
    link: './getting-started.html'
  },
  {
    title: '使用指南',
    description: '配置、保护、运维、监控和维护 EMQX。',
    topics: ['配置和安全', '运维和可观测性'],
    link: '../guides/user-guide.html'
  },
  {
    title: '开发者指南',
    description: '连接客户端、处理数据、构建集成并扩展 EMQX。',
    topics: ['客户端 SDK 和 API', '规则和数据集成'],
    link: '../develop/developer-guide.html'
  },
  {
    title: '版本发布说明',
    description: '按版本查看新功能、修复、已知问题和不兼容变更。',
    topics: ['版本发布说明', '已知问题和破坏性变更'],
    link: '../release-notes/all-changes-ee.html'
  }
]
</script>

# 文档概览

可从常用任务开始，也可按文档分区浏览内容。

## 常用任务

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

<p :class="$style.supportingText">这些快捷入口可直接跳转到不同文档分区中的相关主题。</p>

## 文档分区

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

<p :class="$style.supportingText">选择分区后，可在左侧导航栏中查看该分区的主题。</p>

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
