defmodule MyAppWeb.UserController do
  use MyAppWeb, :controller

  #ruleid: match
  def show(conn, %{"id" => id}) do
    user = Repo.get(User, id)
    render(conn, :show, user: user)
  end
end
