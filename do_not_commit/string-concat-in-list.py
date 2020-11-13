# ruleid:string-concat-in-list
bad = ["123" "456" "789"]

# ruleid:string-concat-in-list
bad = [
    "abc"
    "cde"
    "efg",
    "hijk"
]

good = ["123"]
good = [123, 456]
good = ["123", "456"]
