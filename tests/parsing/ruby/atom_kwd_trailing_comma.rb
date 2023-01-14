#this works
Group.where(
         bob:'alice',
         resource_id_or_key: team.id.to_s,
      )
      
#this also works
Group.where(
         resource_type:'team',
         resource_id_or_key: team.id.to_s
      )      

# this barfs
Group.where(
         resource_type:'team',
         resource_id_or_key: team.id.to_s,
      )
