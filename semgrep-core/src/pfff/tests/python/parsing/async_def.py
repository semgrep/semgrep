@async_to_sync
async def test_get_asgi_application(self):
  return 1

class DatabaseConnectionTest(SimpleTestCase):
    """A database connection cannot be used in an async context."""
    @async_to_sync
    async def test_get_async_connection(self):
        with self.assertRaises(SynchronousOnlyOperation):
            list(SimpleModel.objects.all())

    @Throttle(MIN_TIME_BETWEEN_UPDATES)
    async def async_update(self, **kwargs):
        return 1
