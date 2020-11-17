# Some of those snippers were not parsing correctly with tree-sitter-ruby
# at some point. Now they work, but those tests act as regression tests.

class Scope
        def each
          node = self
          until node.equal? NULL
            yield node
            node = node.parent
          end
        end
end

describe "with" do
  it "should support basic WITH" do
    select_manager = comments.project(Arel.star).with(users_as)
                      .where(comments[:author_id].in(users_top.project(users_top[:id])))
  end

  it "should support WITH RECURSIVE" do
    recursive_term = Arel::SelectManager.new
    recursive_term.from(comments).project(comments_id, comments_parent_id).where(comments_id.eq 42)
  end
end
