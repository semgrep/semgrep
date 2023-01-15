import email.message as message

# this parameter used to create an infinite loop in Naming_AST.ml
# because it created a cycle in the AST generic where message.Message
# which is parsed as an OtherType (OT_EXpr) contain an Id for message
# that will gets id_type reference again message.Message.
def process_message(message: message.Message):
    #ERROR: match
    if 1:
        message
    else:
        message
