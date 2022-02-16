from deeper import get_user

def indirection1(user_id):
    return indirection2(user_id)

def indirection2(user_id):
    return get_user(user_id)