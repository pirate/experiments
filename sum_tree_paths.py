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

def generate_tree(depth=15, min_val=0, max_val=100, _i=0):
    """Generate a random-value filled binary tree of a specified depth."""
    root = Node(random.randint(min_val,max_val))
    if _i < depth-1:
        root.left = generate_tree(depth=depth, min_val=min_val, max_val=max_val, _i=_i+1)
        root.right = generate_tree(depth=depth, min_val=min_val, max_val=max_val, _i=_i+1)
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

paths = []  # globals are bad mmkay
history = set() # to make sure we dont re-start path creation when we've already done that
def sum_all_paths(node, path=None, path_sum=0, until=None):
    """Starting at every node, walk down the tree, recording the path taken and sum of all its values at every point."""
    path = path or []                                                    # path up to the current node

    if node is not None:
        path.append(node)
        path_sum += node.val

        if node.val == until and node not in history:
            history.add(node)

        elif path_sum == until: #or path_sum == until+1 or path_sum == until-1:
            paths.append('>'.join(str(x.val) for x in path))            # store the path and sum up to this point e.g 95>32>23>4'

        # don't waste processing power computing paths that are already greater than the sum we're looking for
        if until is None or (until is not None and path_sum < until):
            sum_all_paths(node.left, path[:], path_sum, until)              # continue this path to the left
            sum_all_paths(node.right, path[:], path_sum, until)             # continue this path to the right

        if node.left not in history:
            history.add(node.left)
            sum_all_paths(node.left, None, 0, until)                              # start a brand new path here, and continue to the left
        if node.right not in history:
            history.add(node.right)
            sum_all_paths(node.right, None, 0, until)                             # start a brand new path here, and continue to the right

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

    min_val,max_val =     1,50                          # min, max tree node value
    min_depth,max_depth = 10,15                         # min, max tree depth to test
    min_sum,max_sum,sum_interval =     0,1000,50        # min, max number of path sums to test
    trials = 10                                         # number of trials to run for each depth and sum, also the max node value


    print "This program generates a random-value filled binary tree.  It then walks through the tree in a depth-first order, finding all the paths who's nodes add up to a specified sum.\nThe paths can start at any node, but they must go straight down."
    print "It runs this pathfinding search over a range of tree depths, random values, and desired path sums.  It then averages the result to find the relationship between #pathsfound:treedepth:desiredsum."

    min_val = int(raw_input("Minimum node value (1):").strip() or min_val)
    max_val = int(raw_input("Maximum node value (50):").strip() or max_val)
    min_sum = int(raw_input("Starting Path sum to look for (0):").strip() or min_sum)

    
    print "You selected a max tree depth of %s, a max node value of %s, and a max search sum of %s.\n" % (max_depth, max_val, max_sum)
    if max_depth*max_val < max_sum:
        print "Warning, you will get 0 paths for several sum trials because %s*%s=%s, %s<%s!\n" % (max_depth, max_val, max_depth*max_val, max_depth*max_val,max_sum)

    print "Average number of paths that add up to each sum (%s trials per tree depth)\n" % trials
    header_line = "|SUM|  btree depth:  |%s " % '    |'.join(str(i).ljust(3) for i in range(min_depth,max_depth+1))
    print "%s\n%s" % (header_line, len(header_line)*'-')
    trial_avgs = {}
    avgs_by_depth = {d:[] for d in range(min_depth,max_depth+1)}

    for trial_sum in range(min_sum,max_sum+1,sum_interval):
        if trial_sum == 0:
            continue
        trial_avgs[trial_sum] = {}              

        for depth in range(min_depth,max_depth+1):
            trial_avgs[trial_sum][depth] = []
            for trial in range(0,trials+1):
                root = generate_tree(depth=depth, min_val=min_val, max_val=max_val)
                sum_all_paths(root, until=trial_sum)
                trial_avgs[trial_sum][depth].append(len(paths))
                paths = []
                history = set()

        rowstr = '  '+str(trial_sum).ljust(20)
        for depth, avg in trial_avgs[trial_sum].iteritems():
            avg = sum(trial_avgs[trial_sum][depth])/len(trial_avgs[trial_sum][depth])
            avgs_by_depth[depth].append(avg)
            rowstr += str(avg).ljust(8)
        print rowstr

    print len(header_line)*'-'
    print "Total Averages:       " + ''.join(str(sum(a)/len(a)).ljust(8) for a in avgs_by_depth.itervalues())
