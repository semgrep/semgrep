# ruleid: decorator-unordered-ok-when-rule-option-false
@first("syn")
@second("ack")
def func1():
    pass

# ruleid: decorator-unordered-ok-when-rule-option-false
@second("ack")
@first("syn")
def func2():
    pass
