
from concurrent.futures.thread import ThreadPoolExecutor

def foo():
    with ThreadPoolExecutor(max_workers=5) as executor:
        # ruleid:uncaught-executor-exceptions
        executor.map(run_with_app_context, tasks)

    with ThreadPoolExecutor(max_workers=5) as executor:
        # ok
        for _ in executor.map(run_with_app_context, tasks):
            pass

    with ThreadPoolExecutor(max_workers=5) as executor:
        # ok
        print [x for x in executor.map(run_with_app_context, tasks)]
