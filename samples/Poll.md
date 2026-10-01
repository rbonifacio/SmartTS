# Poll: roteiro pela linha de comando

Este roteiro executa o contrato [`Poll.smartts`](Poll.smartts) passo a passo
pela CLI do SmartTS: primeiro o deploy (originação) do contrato e, em seguida,
uma sequência de chamadas aos entrypoints. Cada passo diz qual é o objetivo e
qual resultado esperar.

O contrato modela uma enquete. Os eleitores escolhem uma entre `numOptions`
opções (0, 1, ...), podem retirar o voto enquanto a enquete está aberta, e o
encerramento devolve a opção vencedora. Ele exercita records no storage,
`string` (`+` e `length`), `map<K, V>` (`empty_map`, `m[k]`, `mem` e `remove`),
laços `for` e `while`, `if`/`else` e métodos `@private`.

Todos os comandos devem ser executados a partir da raiz do projeto.

## 0. Preparação

**Objetivo:** compilar o projeto e criar um diretório vazio para o
"repositório" local, onde ficam o estado dos contratos (`state.json`) e as
fontes originadas.

```bash
cabal build
rm -rf poll-repo && mkdir poll-repo
```

Nos comandos abaixo, `smart-ts` é um atalho para o executável compilado:

```bash
smart-ts() { "$(cabal list-bin smart-ts)" "$@"; }
```

## 1. Deploy (originação) do contrato

**Objetivo:** criar uma instância do `Poll` com a pergunta e 3 opções
(0, 1 e 2). O `@originate init` monta o storage, e o laço `for` zera a contagem
de cada opção.

```bash
smart-ts --originate --repo poll-repo \
  --source samples/Poll.smartts \
  --args '{"question":"Best language?","numOptions":3}'
```

A saída é `Originated contract at address: KT1..._0`. Guarde o endereço numa
variável para os próximos passos (o comando abaixo lê o endereço do
`state.json`):

```bash
ADDR=$(python3 -c "import json; print(next(iter(json.load(open('poll-repo/state.json'))['instances'])))")
echo $ADDR
```

## 2. Votos válidos

**Objetivo:** registrar três votos. Cada um incrementa `votes[option]` e grava
a escolha em `voters[voter]`.

```bash
smart-ts --call --repo poll-repo --address $ADDR --entrypoint vote --args '{"voter":"ana","option":1}'
smart-ts --call --repo poll-repo --address $ADDR --entrypoint vote --args '{"voter":"bia","option":2}'
smart-ts --call --repo poll-repo --address $ADDR --entrypoint vote --args '{"voter":"caio","option":1}'
```

Resultado esperado: `true` nos três.

## 3. Votos rejeitados

**Objetivo:** ver as regras do `canVote` em ação. Um eleitor não vota duas
vezes (`mem`) e a opção precisa existir (`validOption`).

```bash
smart-ts --call --repo poll-repo --address $ADDR --entrypoint vote --args '{"voter":"ana","option":0}'
smart-ts --call --repo poll-repo --address $ADDR --entrypoint vote --args '{"voter":"duda","option":7}'
```

Resultado esperado: `false` nos dois.

## 4. Retirar um voto

**Objetivo:** o `withdraw` lê a opção escolhida em `voters[voter]`, decrementa
essa opção e remove o eleitor com `remove`. Na segunda tentativa, o `caio` já
não está no map.

```bash
smart-ts --call --repo poll-repo --address $ADDR --entrypoint withdraw --args '{"voter":"caio"}'
smart-ts --call --repo poll-repo --address $ADDR --entrypoint withdraw --args '{"voter":"caio"}'
```

Resultado esperado: `true`, depois `false`.

## 5. Votar de novo depois de retirar

**Objetivo:** mostrar que, depois de retirar o voto, o eleitor pode votar
outra vez.

```bash
smart-ts --call --repo poll-repo --address $ADDR --entrypoint vote --args '{"voter":"caio","option":2}'
```

Resultado esperado: `true`.

## 6. Total de votos

**Objetivo:** somar os votos de todas as opções com um laço `while`.

```bash
smart-ts --call --repo poll-repo --address $ADDR --entrypoint totalVotes --args '{}'
```

Resultado esperado: `3`.

## 7. Encerrar a enquete

**Objetivo:** fechar a enquete (`isOpen = false`) e devolver a opção vencedora,
calculada pelo método `@private winner` com um laço `for`.

```bash
smart-ts --call --repo poll-repo --address $ADDR --entrypoint close --args '{}'
```

Resultado esperado: `2` (a opção 2 tem 2 votos e a opção 1 tem 1).

## 8. Tentativas com a enquete fechada

**Objetivo:** confirmar que, depois de fechada, a enquete não aceita votos nem
retiradas.

```bash
smart-ts --call --repo poll-repo --address $ADDR --entrypoint vote --args '{"voter":"edu","option":0}'
smart-ts --call --repo poll-repo --address $ADDR --entrypoint withdraw --args '{"voter":"ana"}'
```

Resultado esperado: `false` nos dois.

## 9. Inspecionar o estado final

**Objetivo:** ver o storage salvo depois de todas as chamadas. Os maps aparecem
como listas de pares `{key, value}`.

```bash
cat poll-repo/state.json
```

O storage final contém:

```
question: "Best language?", numOptions: 3, isOpen: false
votes:  0 → 0, 1 → 1, 2 → 2
voters: ana → 1, bia → 2, caio → 2
status: "rejected vote from edu"
```

O campo `status` registra a última ação aceita ou rejeitada pelo `vote`, o
`withdraw` ou o `close`. Por isso ele mostra a tentativa do `edu`: o `withdraw`
da `ana`, que veio depois, falhou e não altera o `status`.

## Para explorar

- Origine uma enquete com a pergunta vazia (`"question":""`) ou com uma única
  opção. O `init` deixa `isOpen` como `false`, e nenhum voto é aceito.
- Empate: com votos iguais em duas opções, o `winner` devolve a de menor número.
- Como o SmartTS ainda não tem o conceito de remetente da transação, o nome do
  eleitor é um argumento. Qualquer pessoa pode votar em nome de outra. Como
  isso seria resolvido num contrato Tezos real?
