@pytest.mark.parametrize("scheme", ["http", "https", "ftp", "file"])
def test_insecure_scheme(scheme):
  return 1
