class MyController < ApplicationController
    def bad_route
        # ruleid: filter-skipping
        match '/:controller(/:action(/:id))'
    end

    def ok_route
        match '/:controller(/:action(/:id))', :action => /[a-z_]+/
    end
end
