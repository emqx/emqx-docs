# EMQX 5.3 の既知の問題

## e5.3.0

- **SAMLベースのSSOの制限**

  EMQXダッシュボードは、Security Assertion Markup Language（SAML）2.0標準に基づくシングルサインオンをサポートしており、OktaやOneLoginをアイデンティティプロバイダーとして統合しています。しかし、SAMLベースのSSOは現在、証明書署名検証機構をサポートしておらず、その複雑さからAzure Entra IDとは互換性がありません。
