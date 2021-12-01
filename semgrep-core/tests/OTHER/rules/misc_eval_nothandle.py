# This used to crash semgrep with a NotHandle exn
os.getenv('a', 'b')
