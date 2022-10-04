# from https://github.com/rails/rails/blob/main/activejob/lib/active_job/enqueuing.rb

# this actually parses in semgrep because even if tree-sitter
# says ... is an error, pfff accepts it
# but for more complex files where pfff will fail, then
# we will get an error.
def perform_later(...)
        job = job_or_instantiate(...)
        enqueue_result = job.enqueue
end
 
