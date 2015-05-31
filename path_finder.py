grid = [
    0,0,0,0,0,0,0,0,0,1,    
    0,0,0,0,0,0,0,0,1,0,
    0,0,0,0,0,0,0,1,0,0,
    0,0,0,0,0,0,1,0,0,0,
    0,0,0,0,0,1,0,0,0,0,
    0,0,0,0,1,0,0,0,0,0,
    0,0,0,1,0,0,0,0,0,0,
    0,0,1,0,0,0,0,0,0,0,
    0,1,0,0,0,0,0,0,0,0,
    1,0,0,0,0,0,0,0,0,0,
]

move = {
    'right': lambda i: i + 1 if (i + 1) % 10 else i,
    'left':  lambda i: i - 1 if i % 10 else i,
    'up':    lambda i: i - 10 if i > 10 else i,
    'down':  lambda i: i + 10 if i < 90 else i,
}
moves = move.iteritems()
opposite =  {'up':'down', 'down':'up', 'left':'right', 'right':'left'}

idx = lambda x, y: (y * 10) + x

def find_path(grid, start=(0,0), end=(10,10)):
    "find the path from start to finish, avoiding any obstacles in the way"
    path = []
    cursor = idx(*start)
    end = idx(*end)
    last_move_opposite = 'up'
    while cursor != end:
        for move_name, move in moves:
            # if the move is legal and doesn't simply reverse the previous move
            if move(cursor) != cursor and move_name != last_move_opposite:
                cursor = move(cursor)    # perform the move
                path.append(move_name)
                last_move_opposite = [move_name]
                break
    
    print path

find_path(grid)
# > Execution timed out.
