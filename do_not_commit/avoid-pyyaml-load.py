import yaml


#ruleid:avoid-pyyaml-load
yaml.load("!!python/object/new:os.system [echo EXPLOIT!]")

def check_ruamel_yaml():
    from ruamel.yaml import YAML
    yaml = YAML(typ="rt")
    # ok
    yaml.load("thing.yaml")