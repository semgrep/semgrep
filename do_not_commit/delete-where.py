delete = table.delete().where(table.post_id == post_id).execute()
# ruleid:delete-where-no-execute
delete = table.delete().where(table.post_id == post_id)
