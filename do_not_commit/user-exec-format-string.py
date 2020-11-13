from textwrap import dedent

def unsafe(request):
    # ruleid: user-exec-format-string
    message = request.POST.get('message')
    print("do stuff here")
    code = """
    print(%s)
    """ % message
    exec(code)

def unsafe_inline(request):
    # ruleid: user-exec-format-string
    exec("print(%s)" % request.GET.get('message'))

def unsafe_dict(request):
    # ruleid: user-exec-format-string
    exec("print(%s)" % request.POST['message'])

def safe(request):
    # ok
    code = """
    print('hello')
    """
    exec(dedent(code))

def fmt_unsafe(request):
    # ruleid: user-exec-format-string
    message = request.POST.get('message')
    print("do stuff here")
    code = """
    print({})
    """.format(message)
    exec(code)

def fmt_unsafe_inline(request):
    # ruleid: user-exec-format-string
    exec("print({})".format(request.GET.get('message')))

def fmt_unsafe_dict(request):
    # ruleid: user-exec-format-string
    exec("print({}, {})".format(request.POST['message'], "pwned"))

def fmt_safe(request):
    # ok
    code = """
    print('hello')
    """
    exec(dedent(code))

def code_execution(request):
    data = ''
    msg = ''
    first_name = ''
    if request.method == 'POST':

        # Clear out a previous success to reset the exercise
        try:
            os.unlink('p0wned.txt')
        except:
            pass

        # ruleid: user-exec-format-string
        first_name = request.POST.get('first_name', '')

        try:    # Try it the Python 3 way...
            exec(base64.decodestring(bytes(first_name, 'ascii')))
        except TypeError:
            try:    # Try it the Python 2 way...
                exec(base64.decodestring(first_name))
            except:
                pass
        except:
            pass