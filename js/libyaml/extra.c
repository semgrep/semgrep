// NOTE: this is not part of the official libyaml codebase. These methods are used to sanity check our allocation math.

#include "yaml.h"

YAML_DECLARE(int)
get_sizeof_yaml_parser_t() {
	return sizeof(yaml_parser_t);
}

YAML_DECLARE(int)
get_sizeof_yaml_event_t() {
	return sizeof(yaml_event_t);
}
