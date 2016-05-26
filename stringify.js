// stringify any non-object, only handles String, Number, Boolean, null, undefined
const stringifyItem = (item) => {
    if (item === null)
        return 'null'
    if (item === undefined)
        return 'undefined'
    if (typeof(item) === 'string')
        return `"${item}"`
    return item.toString()
}

// recursive function to stringify any Object or List
const stringifyObject = (item) => {
    // base case, item is not an object, just return string directly
    if (!item || typeof(item) !== 'object')
        return stringifyItem(item)

    // recursive case where item is a list
    if (item && item.map) {
        const str_items = item.reduce((acc, val) =>
            `${acc},${stringifyObject(val)}`)

        return `[${str_items}]`
    }

    // recursive case where item is an object
    let str_items = ''
    for (let key of Object.keys(item)) {
        const str_key = stringifyObject(key)
        const str_value = stringifyObject(item[key])
        str_items += `${str_key}:${str_value},`
    }
    // remove trailing comma
    if (str_items.length)
        str_items = str_items.slice(0, -1)

    return `{${str_items}}`
}

const test_object = {
    1: 'value_one',
    'key_two': 2,
    3: [1,2,3, 'four', {5: 6}],
    7: {
        'nested_key1': 11,
        'nested_key2': 22,
    },
    9: null,
    '10': 'WHATSUP DAWG'
}

const stringified = stringifyObject(test_object)
const professionally_stringified = JSON.stringify(test_object)

console.log(JSON.stringify(test_object))
// {"1":"value_one","3":[1,2,3,"four",{"5":6}],"7":{"nested_key1":11,"nested_key2":22},"9":null,"10":"WHATSUP DAWG","key_two":2}
console.log(stringified)
// {"1":"value_one","3":[1,2,3,"four",{"5":6}],"7":{"nested_key1":11,"nested_key2":22},"9":null,"10":"WHATSUP DAWG","key_two":2}
console.log(stringified == professionally_stringified ? 'Success!' : 'Fail!')
// Success!
