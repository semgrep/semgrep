# ok
REST_FRAMEWORK = {
    'PAGE_SIZE': 10,
    'DEFAULT_THROTTLE_CLASSES': [
        'rest_framework.throttling.AnonRateThrottle',
        'rest_framework.throttling.UserRateThrottle'
    ],
    'DEFAULT_THROTTLE_RATES': {
        'anon': '100/day',
        'user': '1000/day'
    },
    "SOMETHING_ELSE": {1: 2}
}

# ruleid: missing-throttle-config
REST_FRAMEWORK = {
    'PAGE_SIZE': 10
}
