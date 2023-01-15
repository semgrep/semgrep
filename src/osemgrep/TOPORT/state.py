class SemgrepState:
    """
    """
    app_session: AppSession = Factory(AppSession)
    metrics: Metrics = Factory(Metrics)
    error_handler: ErrorHandler = Factory(ErrorHandler)
    settings: Settings = Factory(Settings)
