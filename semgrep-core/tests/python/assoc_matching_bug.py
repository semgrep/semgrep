# m_assoc_op must use MV.Id when binding `$D` to `default`
def not_append_func8(default=[]):
    #ERROR:
    default = default or []
    default.append(5)
