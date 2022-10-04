from concurrent.futures.thread import ThreadPoolExecutor

def foo():
    with ThreadPoolExecutor(max_workers=5) as executor:
        #ERROR: match
        print [x for x in executor.map(run_with_app_context, tasks)]
