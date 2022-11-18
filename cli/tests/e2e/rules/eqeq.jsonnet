local all = import "eqeq.yaml";

{ rules:
   [ r + { "message": "the eqeq message has been overriden!" }
    for r in all.rules] }
