import _osx_support
import os
import sys
import sysconfig

import setuptools._distutils

print("** sys.version_info")
print(sys.version_info)
print("** setuptools._distutils.util.get_platform()")
print(setuptools._distutils.util.get_platform())
print("** os.uname()")
print(os.uname())
print("** sysconfig.get_platform()")
print(sysconfig.get_platform())

osname, host, release, version, machine = os.uname()
osname = osname.lower().replace("/", "")
machine = machine.replace(" ", "_")
machine = machine.replace("/", "-")
print("** osname")
print(osname)
print("** release")
print(release)
print("** machine")
print(machine)
print("** CFLAGS")
_config_vars = sysconfig.get_config_vars()
for key, value in _config_vars.items():
    print(f"{key}: {value}")
print(
    "** _osx_support.get_platform_osx(sysconfig.get_config_vars(), osname, release, machine)"
)
print(
    _osx_support.get_platform_osx(sysconfig.get_config_vars(), osname, release, machine)
)
