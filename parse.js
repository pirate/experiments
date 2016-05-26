const parseJSON = (string) => {
    return {}
}


const test_str = `{"1":"value_one","3":[1,2,3,"four",{"5":6}],"7":{"nested_key1":11,"nested_key2":22},"9":null,"10":"WHATSUP DAWG","key_two":2}`

const parsed = parseJSON(test_str)
const professionally_parsed = JSON.parse(test_str)

console.log(parsed)
// Object {...}
console.log(professionally_parsed)
// Object {1: "value_one", 3: Array[5], 7: Object, 9: null, 10: "WHATSUP DAWG", key_two: 2}
console.log(parsed == professionally_parsed ? 'Success!' : 'Fail!')
// Fail!
