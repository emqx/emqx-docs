# Dashboard 多因素认证

多因素认证（MFA）为 Dashboard 登录增加了第二道安全屏障。用户输入用户名和密码后，还需提供由认证应用生成的基于时间的一次性密码（TOTP）。即使账户凭据泄露，Dashboard 也能得到有效保护。

MFA 兼容所有符合 TOTP 标准的认证应用，包括：

- Google Authenticator
- Microsoft Authenticator
- Authy

认证应用中显示的发行方名称为 **EMQX**。

管理员可以为单个用户启用 MFA，也可以通过 SAML 模块的 `force_mfa` 参数强制所有 SAML SSO 用户使用 MFA。

## MFA 工作原理

EMQX Dashboard 的 MFA 遵循状态机模型。每个用户的 MFA 状态按以下路径流转：

```
未启用 MFA
  │
  ▼ （管理员启用，或 force_mfa 生效）
需要设置
  │
  ▼ （用户扫描 QR 码）
设置进行中
  │
  ▼ （用户验证 TOTP 码）
已启用
  │
  ▼ （管理员禁用）
已禁用
```

当用户 MFA 处于**已启用**状态时，登录需要提供三项信息：用户名、密码以及当前 TOTP 码。服务端在密码验证通过后会签发一个短期有效的 **MFA 状态令牌**（JWT，有效期 5 分钟），用于后续 MFA 验证请求的鉴权。

:::tip

MFA 状态令牌 5 分钟后过期。若用户未在此时间窗口内完成 MFA 验证，需重新发起登录流程。

:::

## 为用户启用 MFA

管理员可通过 API 为任意 Dashboard 用户启用 MFA。

**请求：**

```bash
POST /api/v4/users/:username/mfa/enable
```

**示例：**

```bash
curl -u admin:public -X POST http://localhost:18083/api/v4/users/alice/mfa/enable
```

**响应：**

```json
{
  "code": 0,
  "data": {
    "message": "MFA setup required on next login"
  }
}
```

管理员为其他用户启用 MFA 后，该用户下次登录时会看到 **MFA 设置提示**，必须完成设置流程才能访问 Dashboard。

若管理员为自己启用 MFA，API 响应中会直接包含 QR 码 URI 和密钥，无需等到下次登录。

:::tip

可随时通过 `GET /api/v4/users/:username/mfa` 查询用户的 MFA 状态，详见[查询 MFA 状态](#查询-mfa-状态)。

:::

## MFA 设置流程

当用户的 MFA 状态为**需要设置**时，下次登录将引导其完成一次性设置流程。

### 第一步：使用用户名和密码登录

用户向标准登录接口提交凭据。

```bash
POST /api/v4/auth
```

```bash
curl -u alice:password -X POST http://localhost:18083/api/v4/auth
```

由于需要设置 MFA，服务端不会返回会话令牌，而是返回设置标志和短期有效的 MFA 状态令牌。

```json
{
  "code": 0,
  "data": {
    "mfa_setup_required": true,
    "mfa_state_token": "<mfa_state_token>"
  }
}
```

### 第二步：获取 QR 码

用户以 MFA 状态令牌作为 Bearer 令牌调用设置接口。

```bash
POST /api/v4/mfa/setup
Authorization: Bearer <mfa_state_token>
```

```bash
curl -H "Authorization: Bearer <mfa_state_token>" \
     -X POST http://localhost:18083/api/v4/mfa/setup
```

**响应：**

```json
{
  "code": 0,
  "data": {
    "secret": "BASE32ENCODEDSECRET",
    "qr_uri": "otpauth://totp/EMQX:alice?secret=BASE32ENCODEDSECRET&issuer=EMQX",
    "verification_token": "<verification_token>"
  }
}
```

`qr_uri` 遵循标准 `otpauth://totp/` 格式：

```
otpauth://totp/EMQX:<username>?secret=<base32_secret>&issuer=EMQX
```

### 第三步：扫描 QR 码

用户打开认证应用，扫描 `qr_uri` 所对应的 QR 码。扫描完成后，应用开始每 30 秒生成一个新的 6 位 TOTP 码。

![MFA Setup](./assets/dashboard_mfa_setup.png)

### 第四步：验证 TOTP 码

用户提交认证应用中当前显示的 TOTP 码以完成设置。

```bash
POST /api/v4/mfa/setup/verify
Authorization: Bearer <verification_token>
```

```bash
curl -H "Authorization: Bearer <verification_token>" \
     -H "Content-Type: application/json" \
     -X POST http://localhost:18083/api/v4/mfa/setup/verify \
     -d '{"code": "123456"}'
```

**响应：**

```json
{
  "code": 0,
  "data": {
    "token": "<session_token>"
  }
}
```

响应中的 `token` 是完整的 Dashboard 会话令牌，用户已成功登录，账户 MFA 状态变为**已启用**。

:::warning

请将密钥或备份码保存在安全位置。若认证应用丢失，需由管理员重置 MFA 密钥，详见[重置 MFA 密钥](#重置-mfa-密钥)。

:::

## MFA 登录流程

MFA 设置完成后，后续每次登录均需执行以下两步流程。

### 第一步：使用用户名和密码登录

```bash
POST /api/v4/auth
```

```bash
curl -u alice:password -X POST http://localhost:18083/api/v4/auth
```

服务端识别到该用户已启用 MFA，返回 MFA 挑战响应而非会话令牌。

```json
{
  "code": 0,
  "data": {
    "mfa_required": true,
    "mfa_state_token": "<mfa_state_token>"
  }
}
```

### 第二步：提交 TOTP 码

用户从认证应用获取当前 6 位 TOTP 码并提交。

```bash
POST /api/v4/auth/mfa_challenge
Authorization: Bearer <mfa_state_token>
```

```bash
curl -H "Authorization: Bearer <mfa_state_token>" \
     -H "Content-Type: application/json" \
     -X POST http://localhost:18083/api/v4/auth/mfa_challenge \
     -d '{"code": "123456"}'
```

**响应：**

```json
{
  "code": 0,
  "data": {
    "token": "<session_token>"
  }
}
```

`token` 即为 Dashboard 会话令牌，可用于后续 API 调用。

## 禁用 MFA

管理员可为任意用户禁用 MFA。禁用后，用户只需使用用户名和密码即可登录。

```bash
POST /api/v4/users/:username/mfa/disable
```

```bash
curl -u admin:public -X POST http://localhost:18083/api/v4/users/alice/mfa/disable
```

**响应：**

```json
{
  "code": 0,
  "data": {
    "message": "MFA disabled"
  }
}
```

:::tip

即使 SAML 模块设置了 `force_mfa=true`，管理员仍可为单个 SSO 用户禁用 MFA。该设置在用户后续登录时生效。

:::

## 重置 MFA 密钥

若用户丢失了认证应用的访问权限，管理员可重置其 MFA 密钥。旧密钥立即失效，用户下次登录时将重新进入设置流程。

```bash
POST /api/v4/users/:username/mfa/enable
```

加入 `reset=true` 参数：

```bash
curl -u admin:public \
     -H "Content-Type: application/json" \
     -X POST http://localhost:18083/api/v4/users/alice/mfa/enable \
     -d '{"reset": true}'
```

若管理员重置自己的 MFA 密钥，响应中会立即包含新的 QR 码 URI 和密钥。

若管理员重置其他用户的密钥，该用户将回到**需要设置**状态，下次登录时须重新完成设置流程。

## 查询 MFA 状态

管理员可查询任意用户的 MFA 状态。

```bash
GET /api/v4/users/:username/mfa
```

```bash
curl -u admin:public http://localhost:18083/api/v4/users/alice/mfa
```

**响应：**

```json
{
  "code": 0,
  "data": {
    "enabled": true,
    "setup_required": false
  }
}
```

用户列表接口也会为每个账户包含 MFA 相关字段：

```bash
GET /api/v4/users/
```

响应中每个用户对象包含：

```json
{
  "username": "alice",
  "mfa_enabled": true,
  "mfa_setup_required": false
}
```

## MFA 与 SAML SSO

当 SAML 模块配置了 `force_mfa=true` 时，所有新 SSO 用户在首次登录时必须设置 MFA。SAML 登录重定向包含一个 `login_meta` 字段，指示所需操作：

- `mfa_setup_required: true` — 用户必须先完成 MFA 设置才能访问 Dashboard
- `mfa_required: true` — MFA 已配置，用户需提交 TOTP 码

SSO 用户的设置流程和挑战流程与[MFA 设置流程](#mfa-设置流程)和[MFA 登录流程](#mfa-登录流程)中描述的完全相同。

:::tip

即使模块级别设置了 `force_mfa=true`，管理员仍可为单个 SSO 用户禁用 MFA，详见[禁用 MFA](#禁用-mfa)。

:::

## 安全说明

- MFA 状态令牌（密码验证通过后签发）有效期为 **5 分钟**。
- TOTP 密钥存储在服务端的 Mnesia 数据库中，并在集群所有节点间同步复制。
- 未完成验证的待处理 MFA 会话每隔 5 分钟自动清理一次。

:::warning

TOTP 码在其 30 秒周期前后的短暂时间窗口内有效。请确保服务器时钟与认证设备时钟同步，避免验证失败。

:::

## API 参考

| 接口 | 方法 | 鉴权 | 说明 |
|---|---|---|---|
| `/api/v4/auth` | POST | Basic | 登录。若 MFA 已启用，返回 `mfa_required` 或 `mfa_setup_required`。 |
| `/api/v4/auth/mfa_challenge` | POST | Bearer（MFA 状态令牌） | 提交 TOTP 码以完成登录。 |
| `/api/v4/mfa/setup` | POST | Bearer（MFA 状态令牌） | 获取初始 MFA 设置所需的 QR 码和密钥。 |
| `/api/v4/mfa/setup/verify` | POST | Bearer（验证令牌） | 通过验证 TOTP 码完成 MFA 设置。 |
| `/api/v4/users/:username/mfa/enable` | POST | Basic | 为用户启用 MFA。传入 `reset=true` 可重新生成密钥。 |
| `/api/v4/users/:username/mfa/disable` | POST | Basic | 为用户禁用 MFA。 |
| `/api/v4/users/:username/mfa` | GET | Basic | 查询用户的 MFA 状态。 |
