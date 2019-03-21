#!/usr/bin/env python3
# Gambling Statistics Problem:
#     If you play roulette every day and quit whenever you're ahead by x amount, 
#     do you make money in the long run?
# Answer:
#     No.

import random

STARTING_BALANCE = 100000
BET_AMT = 100
MAX_EARNINGS = 100 * BET_AMT
MAX_LOSSES = 10 * BET_AMT
DAYS = 1000
ODDS_OF_WINNING = 0.5


def make_wager(winner: int=None, amt: int=None):
    return {
        'winner': winner or random.randint(0, 1),
        'amt': amt or 100,
    }


def toss_coin(bet: dict):
    desired = bet['winner']
    undesired = 0 if desired == 1 else 0

    outcome = random.randint(1, 100)
    if outcome < (ODDS_OF_WINNING * 100):
        return bet['amt']
    else:
        return -1 * bet['amt']


def trade(starting_balance: int=STARTING_BALANCE, max_earnings: int=MAX_EARNINGS, max_losses: int=MAX_LOSSES, bet_amt: int=BET_AMT):
    """trade using a quit-while-you're ahead strategy"""
    # print(f'Starting with {t.balance}')
    balance = starting_balance
    while (balance < (starting_balance + max_earnings)
           and balance > (starting_balance - max_losses)):

        bet = make_wager()
        delta = toss_coin(bet)
        # print(f'{["lost", "won "][outcome == bet["winner"]]} {balance} + {delta}')
        balance += delta

    return balance


if __name__ == '__main__':
    days = DAYS

    days_won, days_lost = 0, 0
    total_input, total_output = 0, 0

    print(f'Simulating playing roulette over {days} days where you stop playing'
          f' each day after winning {MAX_EARNINGS} or after losing {MAX_LOSSES}')

    for day in range(days):
        total_input += STARTING_BALANCE
        end_of_day_outcome = trade(STARTING_BALANCE, max_earnings=MAX_EARNINGS, max_losses=MAX_LOSSES, bet_amt=BET_AMT)

        if end_of_day_outcome <= STARTING_BALANCE + MAX_LOSSES:
            days_won += 1
        else:
            days_lost += 1

        total_output += end_of_day_outcome

        # if total_output - total_input < (0 - MAX_LOSSES):
            # break

    print(f'Days where you came out ahead:  {days_won}')
    print(f'Days where you came out behind: {days_lost}')
    print(f'Total amount bet:               {total_input}')
    print(f'Total amount remaining:         {total_output}')
    print(f'Net winnings:                   {total_output - total_input}')
