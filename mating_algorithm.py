# Nick Sweeting 2016/10/08
# Mating Algorithm
#
# Find ideal matching pairs given a list of boys and girls, and their preferences for eachother.
# MIT 6.042J Mathematics for Computer Science: Lecture 6
# https://www.youtube.com/watch?v=5RSMLgy06Ew

from collections import defaultdict


# e.g boy A's favorite girls in order are 1, 4, 5, 3, 2, etc.
GIRLS_PREFERENCES = {
    'A': ('3', '5', '2', '1', '4'),
    'B': ('5', '2', '1', '4', '3'),
    'C': ('4', '3', '5', '1', '2'),
    'D': ('1', '2', '3', '4', '5'),
    'E': ('2', '3', '1', '4', '5'),
}
BOYS_PREFERENCES = {
    '1': ('C', 'B', 'E', 'A', 'D'),
    '2': ('A', 'B', 'E', 'C', 'D'),
    '3': ('D', 'C', 'B', 'A', 'E'),
    '4': ('A', 'C', 'D', 'B', 'E'),
    '5': ('A', 'B', 'D', 'E', 'C'),
}


def get_suitors(boys: dict, crossed_off: dict):
    suitors = defaultdict(set)
    for boy, choices in boys.items():
        # find the first favorite girl who isn't crossed off
        for girl in choices:
            if girl not in crossed_off[boy]:
                break
        suitors[girl] |= {boy}
    return suitors

def find_matches(girls_prefs: dict, boys_prefs: dict):
    crossed_off = {boy: set() for boy in boys_prefs.keys()}
    suitors = get_suitors(boys_prefs, crossed_off)

    # while there are more or less than one suitor per girl
    while any(len(suitor_list) != 1 for suitor_list in suitors.values()):
        print('Suitors:     {}'.format(dict(suitors)))
        print('Crossed Off: {}'.format(dict(crossed_off)))

        for girl, choices in girls_prefs.items():
            # find the girl's favorite out of all her suitors
            for boy in choices:
                if boy in suitors[girl]:
                    break

            # say no to all the boys who arent the girls first choice
            not_chosen = suitors[girl] - {boy}
            for boy in not_chosen:
                crossed_off[boy] |= {girl}

        suitors = get_suitors(boys_prefs, crossed_off)

    # return the single suitor per girl
    return {girl: boys.pop() for girl, boys in suitors.items()}

if __name__ == '__main__':
    # every boy gets best possible mate
    print('MATCHES:    ', find_matches(GIRLS_PREFERENCES, BOYS_PREFERENCES))

    # every girl gets optimal mate
    print('MATCHES:    ', find_matches(BOYS_PREFERENCES, GIRLS_PREFERENCES))
