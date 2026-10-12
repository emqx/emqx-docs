---
description: Find common EMQX tasks and browse the documentation by section.
---

<script setup>
const commonTasks = [
  {
    title: 'Install EMQX',
    description: 'Choose packages, Docker, or Kubernetes.',
    link: './deploy/install.html'
  },
  {
    title: 'Configure Authentication',
    description: 'Secure MQTT client access to EMQX.',
    link: '../guides/access-control/authn/authn.html'
  },
  {
    title: 'Integrate with Kafka',
    description: 'Send MQTT data to or receive data from Kafka.',
    link: '../develop/data-integration/data-bridge-kafka.html'
  },
  {
    title: 'Upgrade EMQX',
    description: 'Plan an upgrade and review compatibility changes.',
    link: './deploy/rolling-upgrades.html'
  }
]

const documentationSections = [
  {
    title: 'Get Started',
    description: 'Learn what EMQX is, install it, and complete your first deployment.',
    topics: ['Overview and quick start', 'Installation and migration'],
    link: './getting-started.html'
  },
  {
    title: 'Guides',
    description: 'Configure, secure, operate, monitor, and maintain EMQX.',
    topics: ['Configuration and security', 'Operations and observability'],
    link: '../guides/user-guide.html'
  },
  {
    title: 'Develop',
    description: 'Connect clients, process data, build integrations, and extend EMQX.',
    topics: ['Client SDKs and APIs', 'Rules and data integration'],
    link: '../develop/developer-guide.html'
  },
  {
    title: 'Release Notes',
    description: 'Review features, fixes, known issues, and incompatible changes by version.',
    topics: ['Version release notes', 'Known and breaking changes'],
    link: '../release-notes/all-changes-ee.html'
  }
]
</script>

# Documentation Overview

Start with a common task or browse by documentation section.

## Common Tasks

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

<p :class="$style.supportingText">These shortcuts link directly to topics across different documentation sections.</p>

## Documentation Sections

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

<p :class="$style.supportingText">Choose a section to see its topics in the left navigation.</p>

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
