#ERROR: match
class Post < ActiveRecord::Base
  serialize :tags
#end
#
#def bad1()
#  post = Post.new
  post.tags = params[:tags] # expect this to match
end
