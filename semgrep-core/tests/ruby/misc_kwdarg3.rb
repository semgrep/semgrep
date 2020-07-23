class X

  desc 'Create a status.'
  params do
        #ERROR: match
        requires :status, type: String, desc: 'Your status.'
  end
  post do
        authenticate!
        Status.create!({
          user: current_user,
          text: params[:status]
        })
  end
end

