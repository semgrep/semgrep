#MATCH:
after_update :update_positions, unless: :act_as_list_no_update?
