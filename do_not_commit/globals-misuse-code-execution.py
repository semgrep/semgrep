def unvalidated_forward(request):
    # ruleid: globals-misuse-code-execution
    forward = request.GET.get('fwd')
    function = globals().get(forward)

    if function:
        return function(request)

    env = {'fwd': forward}
    return render(request, 'vulnerable/redirects/forward_failed.html', env)

def admin(request):
    return render(request, 'vulnerable/redirects/admin.html', {})

