def no_auth_get(
    url: str, params: Dict[str, str] = {}, headers: Dict[str, str] = {}, **kwargs: Any
) -> Response:
    """Perform a requests.get and default headers set"""
    headers = {**_get_default_headers(), **headers}
    r = requests.get(url, headers=headers, params=params, **kwargs)
    return r
