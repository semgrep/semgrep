import time
from django.contrib.auth.models import User
from django.http import HttpResponse
from . import settings as USettings

def save_scrawl_file(request, filename):
    import base64
    try:
        # ruleid: request-data-write
        content = request.POST.get(USettings.UEditorUploadSettings.get("scrawlFieldName", "upfile"))
        f = open(filename, 'wb')
        f.write(base64.decodestring(content))
        f.close()
        state = "SUCCESS"
    except Exception as e:
        state = u"写入图片文件错误:%s" % e
    return state

def save_file(request):
    # ok
    user = User.objects.get(username=request.session.get('user'))
    content = "user logged in at {}".format(time.time())
    f = open("{}-{}".format(user, time.time()), 'wb')
    f.write(content)
    f.close()