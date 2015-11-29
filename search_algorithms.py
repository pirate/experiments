graph = {'A': set(('B', 'C')),
         'B': set(('A', 'D', 'E')),
         'C': set(('A', 'F')),
         'D': set(('B')),
         'E': set(('B', 'F')),
         'F': set(('C', 'E'))}


def dfs_paths(graph, start, goal):
    stack = [(start, [start])]
    while stack:
        vertex, path = stack.pop()
        for next in graph[vertex] - set(path):
            if next == goal:
                yield tuple(path + [next])
            else:
                stack.append((next, path + [next]))

def bfs_paths(graph, start, goal):
    queue = [(start, [start])]
    while queue:
        vertex, path = queue.pop(0)                 # the only difference between the two is that we pop of the beginning instead of the end
        for next in graph[vertex] - set(path):
            if next == goal:
                yield tuple(path + [next])
            else:
                queue.append((next, path + [next]))

print list(dfs_paths(graph, 'A', 'F')) # [['A', 'B', 'E', 'F'], ['A', 'C', 'F']]
print list(bfs_paths(graph, 'A', 'F')) # [['A', 'C', 'F'], ['A', 'B', 'E', 'F']]
