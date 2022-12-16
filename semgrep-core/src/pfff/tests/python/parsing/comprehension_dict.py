def foo():
      return {k: v for k, v in d.items() if v is not None}

GRANULAR_CHECKS = {k: [fix(e) for e in v] for k, v in GRANULAR_CHECKS}
version_time_map = {k: parse_timestamp(v) for k, v in time_metadata.items()}

RECORDS = map(lambda s: json.loads(s), results)

def parse_remaining(pairs: str) -> Dict:
     """
     Given a string of remaining arguments (after the "--"), that looks like "['x=y', 'a=b'] return a dict of { 'x': 'y' }
     """
     return {pair.split("=")[0]: pair.split("=")[1] for pair in pairs}

def get_common_metadata(request: Any) -> Dict:
    r2c_headers = {k: v for k, v in request.headers.items() if k.startswith(("X-R2C-"))}

    repos = list({r for r in f.read().splitlines() if r})


def multimetric(prefix=""):
     def decorator(func):
         @functools.wraps(func)
         def wrapper(*args, **kwargs):
             value = func(*args, **kwargs)
             return tuple({prefix + k: v for k, v in value.items()}.items())

         return wrapper

     return decorator

def get_common_metadata(request):
    r2c_headers = {k: v for k, v in request.headers.items() if k.startswith(("X-R2C-"))}


def foo():
     self._reverse_mapping = {
             value: key for key, value in self._error_mapping.items()
     }

     return {k: v for k, v in raw_location.items() if v is not None}
