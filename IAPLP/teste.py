import copy
from random import shuffle,randint,choices,choice
from time import sleep
from Statistics import correlation

# F ganha de N e M
winsOver = {'F':['N','M'],'N' : ['A','T'], 'A': ['F','M'], 'M': ['N','T'],'T':['F','A']}
# F perde de A e T
losesOver = {'F': ['A', 'T'],'N': ['F', 'M'], 'M': ['F', 'A'], 'A': ['N', 'T'], 'T': ['N', 'M']}
# Todas as cartas
cartas = [letra+numero for letra in 'F A M N T'.split(' ') for numero in '1 2 3'.split(' ')]

def game():
    # inicio do jogo

    shuffle(deck_player)
    shuffle(deck_ia)

    hand_player = []
    hand_ia = []

    while rounds < 10:
        print(f'---{rounds+1:^20}---')

        # Preencher as mãos tanto do jogador quanto da IA

        fill_hand(deck_player,deck_ia,hand_player,hand_ia)

        # Jogador seleciona uma carta

        choice_player = player_chooses(hand_player)

        # IA seleciona uma carta
        
        print(f'---{"IA":^20}---')
        choice_ia = ia_chooses(hand_ia,ia_1_weight)
        print(f'escolha da IA: {choice_ia}')
        print()
        sleep(2)

        # Mostra as escolhas

        print(f"---{choice_player +' VS '+choice_ia:^20}---")
        print()
        sleep(2)
        
        # Mostra o vencedor

        match compare(choice_player,choice_ia):
            case 1:
                print('Você venceu!!')
                score_player += int(choice_player[1])
            case -1:
                print('Você perdeu!!')
                score_ia += int(choice_ia[1])
            case 0:
                print('Empate!!')
            
        print()
        sleep(2)

        print(f'Sua pontuação: {score_player}')
        print(f'Pontuação da IA: {score_ia}')
        print()

        sleep(2)

        for elem in loses[choice_player[0]]:
            ia_1_weight[elem] -= I1
        for elem in wins[choice_player[0]]:
            ia_1_weight[elem] += I1
        print(ia_1_weight.items())
        sleep(2)
        rounds += 1

def round_teste(i):
    weight_ia = {'F':(3*i)+1,'A':(9*i)+1,'M':1,'N':(6*i)+1,'T':(12*i)+1}
    hand_ia = ['F3','A3','M3','N3','T3']
    hand_player = ['F1','F2','F3','A1','A3']
    choice_ia = ia_chooses(hand_ia,weight_ia)
    choice_player = choice(hand_player)
    return compare(choice_player,choice_ia)



def ia_game(I1,I2,peso1,peso2):
    deck1 = copy.deepcopy(cartas)
    deck2 = copy.deepcopy(cartas)

    score1 = 0
    score2 = 0

    shuffle(deck1)
    shuffle(deck2)

    hand1 = []
    hand2 = []

    rounds = 0

    while rounds < 10:

        # Preencher as mãos tanto do jogador quanto da IA

        fill_hand(deck1,deck2,hand1,hand2)

        ia_1_choice = ia_chooses(hand1,peso1)
        ia_2_choice = ia_chooses(hand2,peso2)

        match compare(ia_1_choice,ia_2_choice):
            case 1:
                score1 += int(ia_1_choice[1])
            case -1:
                score2 += int(ia_2_choice[1])

        for elem in loses[ia_2_choice[0]]:
            peso1[elem] -= I1
        for elem in wins[ia_2_choice[0]]:
            peso1[elem] += I1

        for elem in loses[ia_1_choice[0]]:
            peso2[elem] -= I2
        for elem in wins[ia_1_choice[0]]:
            peso2[elem] += I2
        rounds += 1
        print(peso1.items())
        print(peso2.items())

    return score1 > score2

I1 = 0
I2 = 0
i1we = {elem: (6*I1)+1 for elem in 'F A M N T'.split(' ')}
i2we = {elem: (6*I2)+1 for elem in 'F A M N T'.split(' ')}

i1vic = 0
i2vic = 0
for i in range(100):
    result = ia_game(I1,I2,i1we,i2we)
    if result: i1vic += 1
    else: i2vic += 1

print(f'I1: {i1vic}')
print(f'I2: {i2vic}')
