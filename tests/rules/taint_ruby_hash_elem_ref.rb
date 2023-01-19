class Test

  def update
    sort = order_clause_bad(params[:sort_by])
    #OK: test
    all_user = User.where("id = '#{params[:user][:id]}'").order(sort)
  end

end
