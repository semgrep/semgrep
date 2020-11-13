class bad_attr_accessible
   include  ActiveModel::MassAssignmentSecurity

   # ruleid: model-attr-accessible
   attr_accessible :name, :admin,
                   :telephone, as: :create_params
   # ruleid: model-attr-accessible
   attr_accessible :name, :banned,
                   as: :create_params
   # ruleid: model-attr-accessible
   attr_accessible :role,
                   :telephone, as: :create_params
   # ruleid: model-attr-accessible
   attr_accessible :name,
                   :account_id, as: :create_params

   # ruleid: model-attr-accessible
   User.new(params.permit(:name, :admin))
   # ruleid: model-attr-accessible
   params_with_conditional_require(ctrl.params).permit(:name, :age, :admin)

   # ruleid: model-attr-accessible
   User.new(params.permit(:role))
   # ruleid: model-attr-accessible
   params_with_conditional_require(ctrl.params).permit(:name, :age, :role)

   # ruleid: model-attr-accessible
   User.new(params.permit(:banned, :name))
   # ruleid: model-attr-accessible
   params_with_conditional_require(ctrl.params).permit(:banned, :name, :age)

   # ruleid: model-attr-accessible
   User.new(params.permit(:address, :account_id, :age))
   # ruleid: model-attr-accessible
   params_with_conditional_require(ctrl.params).permit(:name, :account_id, :age)

   # ruleid: model-attr-accessible
   params.permit!
end

class ok_attr_accessible
   # ok
   attr_accessible :name, :address, :age,
                   :telephone, as: :create_params
   # ok
   User.new(params.permit(:address, :acc, :age))
   # ok
   params_with_conditional_require(ctrl.params).permit(:name, :address, :age)
end
