# Guia de Contribuição

Algumas *guidelines* importantes para manter o código organizado.

## Tópicos

- [Workflow](#workflow)
- [Code Style](#code-style)
- [Commits e PRs](#commits-e-prs)

## Workflow

Cada equipe criará uma *branch* própria para implementar suas *features*.
Isso servirá para impedir que múltiplas mudanças sejam feitas nos mesmos
conjuntos de arquivos.

Quando a equipe implementar todas as funcionalidades atribuídas, as 
mudanças devem ser enviadas na forma de um Pull Request à `main`, que será
avaliada, e sendo adequada, será aceita.

É interessante que cada equipe guarde seus arquivos dentro de um diretório
próprio (e.g. `ui/`, `bot/` e `core/`), a fim de evitar problemas de
**Merge Conflict** quando as mudanças forem enviadas a `main`.

## Code Style

É interessante que as equipes mantenha um certo grau de padronização da
formatação de código. Não queremos várias regras e critérios, pois o
projeto por si já é suficientemente trabalhoso. Porém, tentem aplicar
algumas regras

- Usem **tabs** de 4 espaços no código
- Evitem linhas muito longas (se possível)
- Documentem suas funções e Monads com comentários no formato `{-- ... --}`

## Commits e PRs

Tanto quanto o código, manter os commits e os Pull Requests bem escritos
vai facilitar muito o nosso trabalho. Por isso, é interessante que sigamos
algumas regras:

- **Commits**
  - Evitem mensagens vagas, como "`some changes`"
  - Adicionem uma *tag* no início da mensagem, como `fix`, `feat`, `refactor`, etc
  - Ex.: `feat: adiciona funcionalidades do Deck`
- **Pull Requests**
  - Expliquem **o que foi feito**, não como foi feito
  - Revisem suas PRs antes de publicá-las
