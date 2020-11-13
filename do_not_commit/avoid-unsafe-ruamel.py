from ruamel.yaml import YAML

#ok:avoid-unsafe-ruamel
y1 = YAML()  # default is 'rt'

#ok:avoid-unsafe-ruamel
y2 = YAML(typ='rt')

#ok:avoid-unsafe-ruamel
y3 = YAML(typ='safe')

#ruleid:avoid-unsafe-ruamel
y3 = YAML(typ='unsafe')

#ruleid:avoid-unsafe-ruamel
y4 = YAML(typ='base')
