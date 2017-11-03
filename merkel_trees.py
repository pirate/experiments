import json
from collections import defaultdict
from hashlib import md5

HASH_FUNC = md5

def hash_hashes(hashes: list) -> str:
    return HASH_FUNC(str(hashes).encode()).hexdigest()


def merkel_tree(obj: dict) -> dict:
    """turn {a: {x: 1, y: 2}, b: 3} into
            {hash, subtree: {
                a: {hash, subtree: {
                    x: hash, y: hash}},
                b: hash}}
    """
    if isinstance(obj, dict):
        sub_hashes = {
            key: merkel_tree(val)
            for key, val in obj.items()
        }
        return {
            'hash': hash_hashes(sub_hashes.values()),
            'subtree': sub_hashes,
        }
    return HASH_FUNC(str(obj).encode()).hexdigest()


def verify_obj(obj: dict, hash_tree: dict) -> bool:
    """take an object and its merkel tree to verify the hashes match"""
    
    obj_hash_tree = merkel_tree(obj)
    return obj_hash_tree == hash_tree


def flat_merkel_index(merkel_tree: dict) -> dict:
    """turn a nested merkel tree into a flat map {key1: hash, key2: hash}"""
    index = {}
    for key, val in merkel_tree.items():
        if isinstance(val, dict):
            index[key] = val['hash']
            index.update(flat_merkel_index(val['subtree']))
        else:
            index[key] = val
    return index


def reverse_flat_index(index: dict) -> dict:
    """reverse a map of {key: hash} into {hash: [keys]}"""
    reverse_index = defaultdict(set)
    for key, val in index.items():
        reverse_index[val].add(key)

    return {
        key: list(val)
        for key, val in reverse_index.items()
    }


def test_merkel(print_debug=True):
    archive = {
        'https://google.com': {
            'index.html': 125,
            'js': {
                'index.js': 34235,
                'tracking.js': 2341,
            },
            'static': {
                'test.mp3': 2353,
                'alt': {
                    'duplicate_test.mp3': 2353,
                }
            },
        },
    }
    hash_tree = merkel_tree(archive)
    test_file_hash = md5(str(125).encode()).hexdigest()
    assert hash_tree['subtree']['https://google.com']['subtree']['index.html'] == test_file_hash
    
    index = flat_merkel_index({'Google': hash_tree})
    assert index['index.html'] == test_file_hash
    
    reversed_index = reverse_flat_index(index)
    assert 'index.html' in reversed_index[test_file_hash]

    good_copy = archive.copy()
    assert verify_obj(good_copy, hash_tree)

    bad_copy = archive.copy()
    bad_copy['https://googal.com'] = bad_copy.pop('https://google.com')
    assert not verify_obj(bad_copy, hash_tree)

    bad_copy2 = archive.copy()  # TODO: should be deepcopy but whatevs
    bad_copy2['https://google.com']['index.html'] += 1
    assert not verify_obj(bad_copy2, hash_tree)

    if print_debug:
        print(json.dumps(archive, indent=4))
        print('---')
        print(json.dumps(hash_tree, indent=4))
        print('---')
        print(json.dumps(index, indent=4))
        print('---')
        print(json.dumps(reversed_index, indent=4))

if __name__ == '__main__':
    test_merkel()
