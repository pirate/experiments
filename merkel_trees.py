"""
Take a bookmark archiver folder structure and turn it into a deduped
merkel-tree blob index.

For example:

archive = {
    'https://google.com': {
        'index.html': 125,  # arbitrary bytes as file content
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

Becomes:
archive                                                      -> index/b17644f0ec1fc5f7b9ff7e7346b59aea
archive/https://google.com                                   -> index/e75b0a87eaf354eb1ad950287e8bf716
archive/https://google.com/index.html                        -> index/3def184ad8f4755ff269862ea77393dd
archive/https://google.com/js                                -> index/033ff67f157230f984fbe5150b4b3dc9
archive/https://google.com/js/index.js                       -> index/28a32c20769baa8373833005b125864b
archive/https://google.com/js/tracking.js                    -> index/c8dfece5cc68249206e4690fc4737a8d
archive/https://google.com/static                            -> index/1102530be9ed53fe4dcc41f72b5392bd
archive/https://google.com/static/test.mp3                   -> index/23af4b45f1e166141a790d1a3126e77a  # same file as duplicate_test.mp3
archive/https://google.com/static/alt                        -> index/b0dd67ebc9d1fe35dacd7382216348ff
archive/https://google.com/static/alt/duplicate_test.mp3     -> index/23af4b45f1e166141a790d1a3126e77a  # notice how these two files point to the same hashed blob in the index
"""

import os
import json

from collections import defaultdict
from hashlib import md5

HASH_FUNC = md5   # sha256 is prob better but whatever


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
            'hash': HASH_FUNC(str(sub_hashes.values()).encode()).hexdigest(),
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
        key: sorted(list(val))
        for key, val in reverse_index.items()
    }

def symlink_file(original, link):
    # if os.path.exists(link):
    #     run(['rm', original])
    #     print(f'{original.ljust(60)} -> {link} (already present)')
    # else:
    #     run(['mv', original, link])
    #     print(f'{original.ljust(60)} -> {link}')
    # run(['ln', '-s', link, original])

    print(f'{original.ljust(60)} -> {link}')


def symlink_merkel_tree(merkel_tree: dict, index_path: str, archive_path: str) -> None:
    """
    take an archive directory and move the files to an index based on their hash
    then simlink original files to the hashed blobs in the index
    """

    symlink_path = os.path.join(index_path, merkel_tree['hash'])
    symlink_file(archive_path, symlink_path)

    for filename, hash_val in merkel_tree['subtree'].items():
        original_path = os.path.join(archive_path, filename)
        if isinstance(hash_val, str):
            symlink_path = os.path.join(index_path, hash_val)
            symlink_file(original_path, symlink_path)
        elif isinstance(hash_val, dict):
            symlink_merkel_tree(hash_val, index_path, original_path)


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

    symlink_merkel_tree(hash_tree, 'index', 'archive')

if __name__ == '__main__':
    test_merkel()
