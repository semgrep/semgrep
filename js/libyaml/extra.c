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

#define PRINT_FIELD(STR, VAL) sprintf(STR, "  %i: " #VAL "=%i\n", &(VAL), VAL)
#define PRINT_SIZE(STR, PTR) sprintf(STR, " --- end (size=%i)\n", sizeof(*PTR))

YAML_DECLARE(char *)
print_event(yaml_event_t* ptr) {
	char* str = malloc(1024);
	char* scalar = malloc(1024);
	char* str2 = str;

	switch (ptr->type) {
		case YAML_NO_EVENT:
			str2 += sprintf(str2, "YAML_NO_EVENT:\n");
			break;
		case YAML_STREAM_START_EVENT:
			str2 += sprintf(str2, "YAML_STREAM_START_EVENT:\n");
			break;
		case YAML_DOCUMENT_START_EVENT:
			str2 += sprintf(str2, "YAML_DOCUMENT_START_EVENT:\n");
			break;
		case YAML_MAPPING_START_EVENT:
			str2 += sprintf(str2, "YAML_MAPPING_START_EVENT:\n");
			break;
		case YAML_SCALAR_EVENT:
			str2 += sprintf(str2, "YAML_SCALAR_EVENT:\n");
			break;
		case YAML_MAPPING_END_EVENT:
			str2 += sprintf(str2, "YAML_MAPPING_END_EVENT:\n");
			break;
		default:
			str2 += sprintf(str2, " mystery event %i\n", ptr->type);
	}
	str2 += PRINT_FIELD(str2, ptr->type);
	switch (ptr->type) {
		case YAML_STREAM_START_EVENT:
			str2 += PRINT_FIELD(str2, ptr->data.stream_start.encoding);
			break;
		case YAML_DOCUMENT_START_EVENT:
			str2 += PRINT_FIELD(str2, ptr->data.document_start.version_directive);
			str2 += PRINT_FIELD(str2, ptr->data.document_start.tag_directives.start);
			str2 += PRINT_FIELD(str2, ptr->data.document_start.tag_directives.end);
			str2 += PRINT_FIELD(str2, ptr->data.document_start.implicit);
			break;
		case YAML_MAPPING_START_EVENT:
			str2 += PRINT_FIELD(str2, ptr->data.mapping_start.anchor);
			str2 += PRINT_FIELD(str2, ptr->data.mapping_start.tag);
			str2 += PRINT_FIELD(str2, ptr->data.mapping_start.implicit);
			str2 += PRINT_FIELD(str2, ptr->data.mapping_start.style);
			break;
		case YAML_SCALAR_EVENT:
			str2 += PRINT_FIELD(str2, ptr->data.scalar.anchor);
			str2 += PRINT_FIELD(str2, ptr->data.scalar.tag);
			str2 += PRINT_FIELD(str2, ptr->data.scalar.value);
			str2 += PRINT_FIELD(str2, ptr->data.scalar.length);
			strncpy(ptr->data.scalar.value, scalar, ptr->data.scalar.length);
			scalar[ptr->data.scalar.length] = '\0';
			str2 += sprintf(str2, "  -> string: \"%s\"\n", scalar);
			str2 += PRINT_FIELD(str2, ptr->data.scalar.plain_implicit);
			str2 += PRINT_FIELD(str2, ptr->data.scalar.quoted_implicit);
			str2 += PRINT_FIELD(str2, ptr->data.scalar.style);
			break;

	}
	str2 += PRINT_FIELD(str2, ptr->start_mark.index);
	str2 += PRINT_FIELD(str2, ptr->start_mark.line);
	str2 += PRINT_FIELD(str2, ptr->start_mark.column);
	str2 += PRINT_FIELD(str2, ptr->end_mark.index);
	str2 += PRINT_FIELD(str2, ptr->end_mark.line);
	str2 += PRINT_FIELD(str2, ptr->end_mark.column);
	str2 += PRINT_SIZE(str2, ptr);
	str2 += sprintf(str2, "sizeof(int) = %i\n", sizeof(int));
	str2 += sprintf(str2, "sizeof(long) = %i\n", sizeof(long));
	str2 += sprintf(str2, "sizeof(long long) = %i\n", sizeof(long long));
	str2 += sprintf(str2, "sizeof(double) = %i\n", sizeof(double));
	str2 += sprintf(str2, "sizeof(long double) = %i\n", sizeof(long double));
	str2 += sprintf(str2, "sizeof(size_t) = %i\n", sizeof(size_t));

	return str;
}
