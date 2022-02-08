class _MetricManager:
    """
    To prevent sending unintended metrics, be sure that any data
    stored on this object is sanitized of anything that we don't
    want sent (i.e. sanitize before saving not before sending)

    Made explicit decision to be verbose in setting metrics instead
    of something more dynamic (and thus less boiler plate code) to
    be very explicit in what metrics are being collected
    """

    def __init__(self) -> None: ...

# TODO: type
metric_manager = ...
