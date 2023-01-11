package foo;

func transformMappingOrList(mappingOrList interface{}, sep string, allowNil bool) interface{} {
	switch value := mappingOrList.(type) {
	case map[string]interface{}:
		return toMapStringString(value, allowNil)
	case ([]interface{}):
		result := make(map[string]interface{})
		for _, value := range value {
			parts := strings.SplitN(value.(string), sep, 2)
			key := parts[0]
			switch {
			case len(parts) == 1 && allowNil:
				result[key] = nil
			case len(parts) == 1 && !allowNil:
				result[key] = ""
			default:
				result[key] = parts[1]
			}
		}
		return result
	}
	panic(errors.Errorf("expected a map or a list, got %T: %#v", mappingOrList, mappingOrList))
}
