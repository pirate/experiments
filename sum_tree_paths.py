from collections import deque
import random

class Node:
    val = None
    left = None
    right = None

    def __init__(self, val=None, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right

    def __repr__(self):
        return '<%s>' % self.val

    def __str__(self):
        return self.__repr__()

def generate_tree(depth=15, max_val=100, _i=0):
    """Generate a random-value filled binary tree of a specified depth."""
    root = Node(random.randint(0,max_val))
    if _i < depth-1:
        root.left = generate_tree(depth=depth, max_val=max_val, _i=_i+1)
        root.right = generate_tree(depth=depth, max_val=max_val, _i=_i+1)
    return root

def bfs(root, until=None):
    """Returns breadth-first list of nodes in a whole tree, or up until a specific value is found."""
    Q = deque([root])
    nodes = []
    while Q:
        n = Q.popleft()
        if n is not None:
            nodes.append(n)
            if n.val != until:
                Q.append(n.left)
                Q.append(n.right)
    return nodes

paths = {}  # globals are bad mmkay
def sum_all_paths(node, path=None, path_sum=0, until=None):
    """Starting at every node, walk down the tree, recording the path taken and sum of all its values at every point."""
    path = path or []                                                       # path up to the current node
    if node is not None:
        path.append(node)
        path_sum += node.val

        if path_sum == until:
            paths['>'.join(str(x.val) for x in path)] = path_sum            # store the path and sum up to this point e.g 95>32>23>4':154

        # don't waste processing power computing paths that are already greater than the sum we're looking for
        if until is None or (until is not None and path_sum < until):
            sum_all_paths(node.left, path[:], path_sum, until)              # continue this path to the left
            sum_all_paths(node.right, path[:], path_sum, until)             # continue this path to the right

        sum_all_paths(node.left, [], 0, until)                              # start a brand new path here, and continue to the left
        sum_all_paths(node.right, [], 0, until)                             # start a brand new path here, and continue to the right

if __name__ == "__main__":

    # The initial goal was to traverse a tree and find all possible paths who's nodes sum up to a certain value.
    # Paths can start at any node in the tree, but they all must go straight down, and the sum of their values must equal the number we're looking for.

    # The code below calculates the average number of paths found for a range of randomly generated tree depths, node values, and search values.

    # trial averages = {...,
    #     desired_sum=33: {
    #         tree_depth=1: [0,0,0,0,0,1,0,...],              # 100 trials tell us that usually we rarely find paths that sum to 33 in a tree of depth 1 (the root would have to be 33)
    #         tree_depth=2: [0,0,0,1,0,1,0,...],
    #         ...
    #         tree_depth=9: [5,8,9,5,6,8,7,...],              # but we will find many paths summing to 33 in a tree with 2^9 nodes
    #     }, 
    #     desired_sum=34: {
    #         ...                                             # repeat the same experiment, this time looking for paths that sum to 34
    #     },
    #     ...
    # }

    max_node_val = 100              # maximum tree node value
    max_tree_depth = 8              # maximum tree depth to test
    min_sum,max_sum = 0,5           # number of path sums to test
    trials = 100                    # number of trials to run for each depth and sum, also the max node value


    print "Average number of paths that add up to each sum (100 trials per tree depth)\n"
    header_line = "|SUM|   tree depth: | %s |" % ' |   | '.join(str(i) for i in range(1,max_tree_depth+1))
    print "%s\n%s" % (header_line, len(header_line)*'-')
    trial_avgs = {}
    for trial_sum in range(min_sum,max_sum+1):
        trial_avgs[trial_sum] = {}              

        for depth in range(1,max_tree_depth+1):
            trial_avgs[trial_sum][depth] = []

            for trial in range(0,trials+1):
                root = generate_tree(depth=depth, max_val=random.randint(0,max_node_val))
                sum_all_paths(root, until=trial_sum)
                trial_avgs[trial_sum][depth].append(len(paths))
                paths = {}

        rowstr = '  '+str(trial_sum).ljust(20)
        for depth, avg in trial_avgs[trial_sum].iteritems():
            rowstr += str(sum(trial_avgs[trial_sum][depth])/len(trial_avgs[trial_sum][depth])).ljust(8)
        print rowstr


    averages = [sum(trial_avgs[s][d])/len(trial_avgs[s][d]) for s ]

    print "%s\n%s" % (header_line, len(header_line)*'-')
    print "Averages:     ",
    print str('%s    ' for x in ().ljust(8)
