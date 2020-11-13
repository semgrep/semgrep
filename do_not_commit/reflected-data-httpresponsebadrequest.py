import urllib
from django.db.models import Q
from django.auth import User
from django.http import HttpResponse, HttpResponseBadRequest
from django.utils.translation import ugettext as _

def search_certificates(request):
    # ruleid: reflected-data-httpresponsebadrequest
    user_filter = request.GET.get("user", "")
    if not user_filter:
        msg = _("user is not given.")
        return HttpResponseBadRequest(msg)


    user = User.objects.get(Q(email=user_filter) | Q(username=user_filter))
    if user.DoesNotExist:
        return HttpResponseBadRequest(_("user '{user}' does not exist").format(user_filter))

def previewNode(request, uid):
    """Preview evaluante node"""
    try:
        if uid in engines:
            # ok
            _nodeId = request.data.get('nodeId')
            engines[uid].stoppable = True
            _res = engines[uid].model.previewNode(_nodeId)
            if _res is None:
                return HttpResponseBadRequest('', status=204)
            return HttpResponseBadRequest(_res)
        return manageNoEngine()
    except Exception as e:
        return genericApiException(e, engines[uid])
    finally:
        engines[uid].stoppable = False

def inline_test(request):
    # ruleid: reflected-data-httpresponsebadrequest
    return HttpResponseBadRequest("Received {}".format(request.POST.get('message')))