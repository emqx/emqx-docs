# /// script
# requires-python = ">=3.12"
# dependencies = ["requests"]
# ///
import json
import os
import sys
import time
import threading
from concurrent.futures import ThreadPoolExecutor, as_completed

import requests

OPENAI_API_KEY = os.getenv('OPENAI_API_KEY')
OPENAI_MODEL = os.getenv('OPENAI_MODEL')
OPENAI_API_URL = os.getenv('OPENAI_API_URL')

CONCURRENCY = int(os.getenv('TRANSLATION_CONCURRENCY', '10'))
MAX_RETRIES = 3
REQUEST_TIMEOUT = 600
<<<<<<< HEAD

SKIP_TRANSLATION_PATHS = {
    'en_US/connect-emqx/introduction.md',
}
=======
>>>>>>> origin/release-6.1

SYSTEM_PROMPT = '''
# 1. Role & Objective
You are a **technical translator** specializing in IoT infrastructure (MQTT, message brokers, etc.).
Translate EMQX documentation from **English → Japanese** for an audience of Japanese engineers, administrators, and developers.

# 2. Output Requirements
- **Language**: Japanese
- **Register**: Formal です／ます style
- **Return**: *Only* the translated Japanese Markdown—no extra commentary or notes

---

# 3. Translation Priorities

**Always prioritize the Glossary and Ambiguous/High-Risk Terms (Sections 4 & 5) over any formatting rules below.** If a term appears in the glossary, its Japanese form **must** be used, even if it appears within inline code, identifiers, or other elements typically left untranslated.

---

# 4. Formatting Rules
| Element                          | Instruction                                                             |
|----------------------------------|-------------------------------------------------------------------------|
| Markdown structure               | Keep headings, lists, tables, emphasis, and links **unchanged**         |
| Heading levels                   | Preserve heading levels exactly as in the source (`#`, `##`, `###`, `####`, …). The number of `#` symbols on each heading line **must match the source line-for-line**. Never add, remove, or skip levels (markdownlint MD001). |
| Code blocks  (``` … ```)         | **Do not translate** code; translate comments inside                    |
| Inline code  (`…`)               | **Do not translate, *unless* it is a term from the Glossary (Section 5) or Ambiguous/High-Risk Terms (Section 6).** |
| Identifiers / API paths          | **Do not translate, *unless* it is a term from the Glossary (Section 5) or Ambiguous/High-Risk Terms (Section 6).** (e.g., `emqx_ctl`, `/api/v5/clients`) |
| Config keys & values             | **Do not translate** (`allow_anonymous`, etc.)                          |
| File paths / URLs                | **Do not translate**                                                    |
| Image `:src` attribute           | **Do not translate**                                                    |
| Image `alt` text                 | **Translate**                                                           |

---

# 5. Glossary — **Use Exactly as Written**
| English                  | Japanese (mandatory)          |
|--------------------------|-------------------------------|
| EMQ X / EMQX             | EMQX                          |
| MQTT                     | MQTT                          |
| Broker                   | ブローカー                      |
| Client                   | クライアント                    |
| Topic                    | トピック                       |
| Publish                  | パブリッシュ／パブリッシュする     |
| Subscribe                | サブスクライブ／サブスクライブする  |
| Publisher                | パブリッシャー                  |
| Subscriber               | サブスクライバー                |
| Message                  | メッセージ                     |
| Payload                  | ペイロード                     |
| QoS (Quality of Service) | QoS（サービス品質）             |
| Session                  | セッション                     |
| Cluster / Clustering     | クラスター／クラスタリング        |
| Rule Engine              | ルールエンジン                  |
| Dashboard                | ダッシュボード                  |
| Authentication           | 認証                          |
| Authorization            | 認可                          |
| Bridge                   | ブリッジ                       |
| Connector                | コネクター                     |
| Plugin                   | プラグイン                     |
| Schema Registry          | スキーマレジストリ              |
| Schema Registry Example  | スキーマレジストリの例           |
| Flow Designer            | Flowデザイナー                |
| Data Integration         | データ統合                     |
| Reference                | リファレンス                   |
| Serverless               | サーバレス                     |
| Dedicated                | 専用                          |
| BYOC                     | BYOC                         |

---

# 6. Ambiguous / High-Risk Terms — **Mandatory Japanese Forms**
| English Term            | Japanese Form | Note |
|-------------------------|---------------|------|
| Edge (Edge Computing)   | エッジ／エッジコンピューティング | Never “端末” or “境界” |
| Gateway                 | ゲートウェイ | Distinct from Bridge |
| Node                    | ノード | “節点” NG |
| Service                 | サービス | |
| Load Balancer           | ロードバランサー | LB possible |
| Throughput              | スループット | Not bandwidth |
| Latency                 | レイテンシ | 統一 |
| Overhead                | オーバーヘッド | |
| Scalability             | スケーラビリティ | |
| Resilience              | レジリエンス | Not fault tolerance |
| Fault Tolerance         | フォールトトレランス | |
| Failover                | フェイルオーバー | |
| Uptime                  | アップタイム | |
| Downtime                | ダウンタイム | |
| Deployment              | デプロイ／デプロイメント | |
| Provisioning            | プロビジョニング | |
| Orchestration           | オーケストレーション | |
| Repository              | リポジトリ | Avoid “レポジトリ” |
| Registry                | レジストリ | |
| Namespace               | ネームスペース | |
| Endpoint                | エンドポイント | |
| Queue                   | キュー | |
| Stream                  | ストリーム | |
| Partition               | パーティション | |
| Offset                  | オフセット | |
| Heartbeat               | ハートビート | |
| Keepalive               | キープアライブ | |
| Ack / Acknowledgment    | アック（ACK） | |
| Persistence             | パーシステンス／永続化 | |
| Encryption              | 暗号化 | |

---

# 7. Style & Localization Guidelines
1. Write concise, natural Japanese—avoid 翻訳調.
2. Widely-used tech terms not in the tables may remain in English or standard Katakana.
3. Rephrase for clarity and professional readability when needed.

---

# 8. Deliverable
Provide the translated Markdown, strictly following all rules, glossary entries, and mandatory term forms above.
'''

DIR_YAML_INSTRUCTION = (
    'The following is a yaml file, the main content is the document catalog configuration. '
    'Please translate the title_ja field according to the sibling title_en field, '
    'add or overwrite the existing title_ja value, pay attention to comply with the '
    'translation requirements, keep the comments unchanged, and keep the original formatting '
    'unchanged, and return the modified yaml content directly, do not use code blocks or '
    'other formats.\n\n'
)

log_lock = threading.Lock()


def log(msg):
    with log_lock:
        print(msg, flush=True)


def collect_paths(items):
    paths = []
    for item in items:
        if item.get('path'):
            p = item['path']
            if p.startswith(('http://', 'https://')):
                continue
            paths.append('en_US/index.md' if p == './' else f'en_US/{p}.md')
        if item.get('children'):
            paths += collect_paths(item['children'])
    return paths


def translate_one(input_file_path, copy_set):
    is_dir_yaml = input_file_path.endswith('dir.yaml')
    if not is_dir_yaml:
        if not input_file_path.endswith('.md') or 'en_US' not in input_file_path:
            return {'path': input_file_path, 'status': 'failed', 'error': 'invalid path'}

    output_file_path = input_file_path.replace('en_US', 'ja_JP')
    output_dir = os.path.dirname(output_file_path)
    if output_dir:
        os.makedirs(output_dir, exist_ok=True)

    with open(input_file_path, 'r', encoding='utf-8') as f:
        markdown_text = f.read().strip()

<<<<<<< HEAD
    if 'en_US/changes/' in input_file_path or input_file_path in copy_set or input_file_path in SKIP_TRANSLATION_PATHS:
=======
    if 'en_US/changes/' in input_file_path or input_file_path in copy_set:
>>>>>>> origin/release-6.1
        with open(output_file_path, 'w', encoding='utf-8') as f:
            f.write(markdown_text + '\n')
        return {'path': input_file_path, 'status': 'copied'}

    if is_dir_yaml:
        markdown_text = DIR_YAML_INSTRUCTION + markdown_text

    request_body = {
        'model': OPENAI_MODEL,
        'messages': [
            {'role': 'system', 'content': SYSTEM_PROMPT},
            {'role': 'user', 'content': markdown_text},
        ],
        'stream': False,
        'temperature': 0.3,
    }
    headers = {'api-key': OPENAI_API_KEY}

    last_error = 'unknown'
    for attempt in range(MAX_RETRIES + 1):
        try:
            response = requests.post(OPENAI_API_URL, json=request_body, headers=headers, timeout=REQUEST_TIMEOUT)
        except Exception as e:
            last_error = f'network error: {e}'
            break

        if response.status_code == 200:
            data = response.json()
            translated = data['choices'][0]['message']['content']
            with open(output_file_path, 'w', encoding='utf-8') as f:
                f.write(translated.strip() + '\n')
            return {'path': input_file_path, 'status': 'ok', 'usage': data.get('usage', {})}

        if response.status_code == 429 and attempt < MAX_RETRIES:
            wait = 2 ** attempt
            log(f'  RETRY {input_file_path} attempt={attempt + 1}/{MAX_RETRIES} wait={wait}s (HTTP 429)')
            time.sleep(wait)
            continue

        last_error = f'HTTP {response.status_code}'
        break

    return {'path': input_file_path, 'status': 'failed', 'error': last_error}


def main():
    if len(sys.argv) not in (2, 3):
        print('Usage: translate_to_ja.py <file-list.txt> [directory.json]', file=sys.stderr)
        sys.exit(1)

    list_file = sys.argv[1]
    directory_file = sys.argv[2] if len(sys.argv) > 2 else None

    copy_set = set()
    if directory_file:
        with open(directory_file, 'r', encoding='utf-8') as f:
            data = json.load(f)
        en_paths = set(collect_paths(data.get('en', [])))
        ja_paths = set(collect_paths(data.get('ja', [])))
        copy_set = en_paths - ja_paths

    with open(list_file, 'r', encoding='utf-8') as f:
        files = [line.strip() for line in f if line.strip()]

    if not files:
        print(f'No files listed in {list_file}', file=sys.stderr)
        sys.exit(0)

    total = len(files)
    width = len(str(total))
    log(f'Translating {total} files (concurrency={CONCURRENCY}, max_retries={MAX_RETRIES}, copy_as_is={len(copy_set)} en-only files)')

    ok = 0
    copied = 0
    failed = []
    total_prompt = 0
    total_completion = 0

    with ThreadPoolExecutor(max_workers=CONCURRENCY) as executor:
        future_to_path = {executor.submit(translate_one, f, copy_set): f for f in files}
        for i, future in enumerate(as_completed(future_to_path), 1):
            result = future.result()
            path = result['path']
            status = result['status']
            prefix = f'[{i:>{width}}/{total}]'

            if status == 'ok':
                usage = result.get('usage', {})
                p_tokens = usage.get('prompt_tokens', 0)
                c_tokens = usage.get('completion_tokens', 0)
                total_prompt += p_tokens
                total_completion += c_tokens
                log(f'{prefix} OK   {path} -- prompt={p_tokens} completion={c_tokens}')
                ok += 1
            elif status == 'copied':
                log(f'{prefix} COPY {path}')
                copied += 1
            else:
                log(f'{prefix} FAIL {path} -- {result["error"]}')
                failed.append((path, result['error']))

    log('')
    log(f'Summary: ok={ok} copied={copied} failed={len(failed)} total={total}')
    log(f'Tokens: prompt={total_prompt} completion={total_completion} total={total_prompt + total_completion}')

    if failed:
        log('Failures:')
        for path, error in failed:
            log(f'  {path}: {error}')
        sys.exit(1)


if __name__ == '__main__':
    main()
