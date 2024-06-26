# Carta Ninja

<div style="width: 100%; display: flex; justify-content: center; margin-bottom: 32px">
  <img src="./public/imgs/banner.png" alt="Banner" />
</div>

Repositório destinado ao projeto da disciplina de PLP.

## Conteúdo

- [Descrição](#descrição)
- [Guia de Instalação](#guia-de-instalação)
- [Regras do Jogo](#regras-do-jogo)
- [Troubleshooting](#troubleshooting)
- [Equipe](#equipe)

## Descrição

Carta Ninja é um jogo de cartas com uma temática de ninjas e samurais. O jogador
pode utilizar cartas de diferentes elementos em batalhas contra a máquina.
Essas batalhas têm como principal objetivo incrementar o nível de faixa do
jogador, que inicia no nível de faixa branca, mas pode subir de nível
gradativamente ao vencer mais e mais batalhas, até alcançar o nível de faixa
preta.

## Guia de Instalação

Para executar este jogo, você precisa ter pelo menos uma das dependências abaixo
instaladas em sua máquina:

- Versão Haskell: `GHC` e `Cabal` (recomendamos a instalação via `GHCup`)
- Versão Prolog: `SWI Prolog`

Tendo essas dependências instaladas, clone o repositório com o comando abaixo:

```
git clone https://github.com/Samurai-Turtles/Carta-Ninja.git
```

No diretório do projeto, execute `./run.sh` para iniciar o Runner do jogo. Você
poderá selecionar qual das versões deseja executar. Após selecionar, você verá
a seguinte tela:

![Start Screen](./public/imgs/tela-inicial.png)

### Instalação no Windows

Se você pretende executar o jogo no Windows, recomendamos que o faça via WSL, 
pois o terminal do Windows não reconhece alguns dos caracteres usados nos 
Sprites presentes no jogo.

## Regras do Jogo

O jogo funciona com um **sistema de campanhas**, onde o jogador irá travar
uma série de batalhas contra os chefes para **elevar sua faixa**, onde o 
**grau de dificuldade aumenta a cada nível**. As campanhas também tem um 
**sistema de vidas**. Quando o jogador perder uma batalha, seu 
**total de vidas é decrementado em 1**. 

A campanha terminará quando:

- O jogador **vencer todos os chefes** ou; 
- O jogador **perder todas as suas vidas**

Além disso, **o jogador também pode obter mais pontos de vida** ao vencer uma
partida com **25+ pontos**.

Durante uma batalha, o jogador e o chefe recebem as mesmas 15 cartas, ordenadas
de forma aleatória para cada um. Para vencer seu adversário, você deve jogar uma
carta que tenha um **valor de poder maior** ou um **elemento que seja dominante**
sobre a carta do oponente. 

Abaixo está a tabela de elementos das cartas e sua dominância sobre outros elementos:

![Relações Elementais](./public/imgs/elementos.png)

A batalha acaba quando pelo menos uma das condições abaixo for alcançada:

- O jogador (ou chefe) obter **três vitórias seguidas** na partida
- O jogador (ou chefe) obter **uma vitória com uma carta de cada elemento**
- Se ninguém alcançar essas condições ao final das dez rodadas:
  - **Ganha aquele que obteve mais pontos durante a partida**
  - Se houver empate nos pontos, **a partida será repetida sem perda de vidas** 
    para o jogador

## Troubleshooting

### "O script de inicialização não funciona"

Caso o Runner `run.sh` falhe em iniciar o jogo, você pode executar qualquer
uma das versões conforme o procedimento abaixo:

1. Abra uma janela do terminal no diretório do projeto
2. Entre no diretório da versão que deseja executar (`cd haskell` ou `cd prolog`)
3. Execute o comando de inicialização do projeto
   - Para a versão Haskell, use `cabal run`
   - Para a versão Prolog, use `swipl main.pl` (APT) ou `swi-prolog.swipl main.pl` (Snap)

### "Não consigo executar o jogo no Windows"

A falha presente na execução do jogo no Windows se deve à **codificação do CMD**,
que **não aceita alguns caracteres especiais do padrão UTF-8** usados nos Sprites 
do jogo. Para executar o jogo no Windows, recomendamos fortemente que o faça via 
**WSL2**.

Também é possível que o jogo **apresente erros nos Sprites** (como caracteres não
reconhecidos). Isso se deve à **fonte do CMD do Windows não conter representações
para caracteres Braille** (usados em alguns Sprites). Se isto ocorrer, **tente
trocar a fonte do CMD** para alguma fonte que suporte estes caracteres,
(por exemplo, [Cascadia Code](https://github.com/microsoft/cascadia-code)).

## Equipe

- [Leandro de Oliveira](https://github.com/losout0)
- [Lucas Khalil](https://github.com/LucasKhalil)
- [José Willian](https://github.com/JWillianSl)
- [Sérgio Gustavo](https://github.com/sergio-gustavo-andrade-grilo)
- [Vinicius Ataide](https://github.com/ViniciusUltraAD)
- [Douglas Domingos](https://github.com/dougdomingos)
