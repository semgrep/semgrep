// from https://github.com/returntocorp/semgrep/issues/4725
package generated

func (ec *executionContext) field_Mutation_createAccount_args(ctx context.Context, rawArgs map[string]interface{}) (map[string]interface{}, error) {
	var err error
	args := map[string]interface{}{}
	var arg0 model.Role
	if tmp, ok := rawArgs["role"]; ok {
		ctx := graphql.WithPathContext(ctx, graphql.NewPathWithField("role"))
		arg0, err = ec.unmarshalNRole2myvendorᚗmytldᚋmyprojectᚋbackendᚋapiᚋgraphᚋmodelᚐRole(ctx, tmp)
		if err != nil {
			return nil, err
		}
	}
	args["role"] = arg0
	var arg1 string
	if tmp, ok := rawArgs["emailAddress"]; ok {
		ctx := graphql.WithPathContext(ctx, graphql.NewPathWithField("emailAddress"))
		arg1, err = ec.unmarshalNString2string(ctx, tmp)
		if err != nil {
			return nil, err
		}
	}
	args["emailAddress"] = arg1
	var arg2 string
	if tmp, ok := rawArgs["password"]; ok {
		ctx := graphql.WithPathContext(ctx, graphql.NewPathWithField("password"))
		arg2, err = ec.unmarshalNString2string(ctx, tmp)
		if err != nil {
			return nil, err
		}
	}
	args["password"] = arg2
	var arg3 *uuid.UUID
	if tmp, ok := rawArgs["organisationId"]; ok {
		ctx := graphql.WithPathContext(ctx, graphql.NewPathWithField("organisationId"))
		arg3, err = ec.unmarshalOUUID2ᚖgithubᚗcomᚋgofrsᚋuuidᚐUUID(ctx, tmp)
		if err != nil {
			return nil, err
		}
	}
	args["organisationId"] = arg3
	return args, nil
}
