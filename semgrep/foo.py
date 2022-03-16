from google.protobuf import json_format

from semgrep.interface_pb2 import Location
from semgrep.interface_pb2 import Position

start = Position(line=1, col=2, offset=2)
end = Position(line=3, col=0, offset=10)

lines = ["hi", "mom"]

location = Location(start=start, end=end, lines=lines)

print(json_format.MessageToJson(location))

# Outputs
# {
#   "start": {
#     "line": 1,
#     "col": 2,
#     "offset": 2
#   },
#   "end": {
#     "line": 3,
#     "offset": 10
#   },
#   "lines": [
#     "hi",
#     "mom"
#   ]
# }
