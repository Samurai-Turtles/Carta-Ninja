from random import choices
from time import sleep


def ia_chooses(hand,weight):
    weight_hand_ia = [weight[card[0]] for card in hand]
    ia_choice = choices(hand,weight_hand_ia,k=1)[0]
    hand.remove(ia_choice)
    return ia_choice

def player_chooses(hand_player):
    print(f'---{"JOGADOR":^20}---')
    print('Escolha uma carta: ')
    print(f'Sua mão: {hand_player}')
    choice_player = input().upper()
    while choice_player not in hand_player:
        print(f'Sua mão: {hand_player}')
        choice_player = input('Escolha uma carta válida: ')
    hand_player.remove(choice_player)
    print(f'Sua escolha: {choice_player}')
    print()
    sleep(2)
    return choice_player

def fill_hand(d1,d2,h1,h2):
    while len(h1) < 5:
            h1.append(d1.pop(0))
            h2.append(d2.pop(0))

def compare(cp,cc,wins):
    if cp[0] in wins[cc[0]]:
        return -1
    elif cc[0] in wins[cp[0]]:
        return 1

    elif int(cc[1]) > int(cp[1]):
        return -1
    
    elif int(cp[1]) > int(cc[1]):
        return 1
    
    return 0