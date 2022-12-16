def foo():
    try:
        manifest.output.integration_test_validator(output).validate(test_case_js)
    except jsonschema.ValidationError as err:
        raise InvalidAnalyzerIntegrationTestDefinition(err) from err

    try:
        manifest.output.validator(output).validate(output)
    except jsonschema.ValidationError as err:
            raise InvalidAnalyzerOutput(err) from err

    try:
        validator.validate(data)
    except jsonschema.ValidationError as err:
        raise MalformedManifestException(data, err.message) from err
