# F ganha de N e M
import copy
from random import shuffle

from utils import compare, fill_hand, ia_chooses


wins = {'F':['N','M'],'N' : ['A','T'], 'A': ['F','M'], 'M': ['N','T'],'T':['F','A']}
# F perde de A e T
loses = {'F': ['A', 'T'],'N': ['F', 'M'], 'M': ['F', 'A'], 'A': ['N', 'T'], 'T': ['N', 'M']}
# Todas as cartas
cartas = [letra+numero for letra in 'F A M N T'.split(' ') for numero in '1 2 3'.split(' ')]


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

        fill_hand(deck1,deck2,hand1,hand2)

        choice1 = ia_chooses(hand1,peso1)
        choice2 = ia_chooses(hand2,peso2)

        match compare(choice1,choice2,wins):
            case 1:
                score1 += int(choice1[1])
            case -1:
                score2 += int(choice2[1])

        for elem in loses[choice2[0]]:
            peso1[elem] -= I1
        for elem in wins[choice2[0]]:
            peso1[elem] += I1

        for elem in loses[choice1[0]]:
            peso2[elem] -= I2
        for elem in wins[choice1[0]]:
            peso2[elem] += I2
        rounds += 1

    return score1 > score2

I1 = 10
I2 = 0
i1we = {elem: (6*I1)+1 for elem in 'F A M N T'.split(' ')}
i2we = {elem: (6*I2)+1 for elem in 'F A M N T'.split(' ')}

i1vic = 0
i2vic = 0
for i in range(100):
    result = ia_game(I1,I2,i1we.copy(),i2we.copy())
    if result: i1vic += 1
    else: i2vic += 1

print(f'I1: {i1vic}')
print(f'I2: {i2vic}')