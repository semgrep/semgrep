func f () {
	x := `type Mutation {
			userLifeApp(input: User!): UserCreationResponse!
		}
		
		type UserCreationResponse {
			statusCode: Int!
		}
		
		input PolicyPreference {
			carrier: String!
			carrierName: String!
			monthlyPremium: Float!
			annualPremium: Float!
			name: String!
			returnOfPremium: Boolean!
			underwritingClass: String!
			isTobacco: Boolean!
			healthCategory: String!
			tableRating: Int!
			issueType: String!
			issueAge: Int!
			planIdentifierName: String!
			productType: String!
			useNearestAge: Boolean!
		}
		
		input AgentInfo {
			code: String!
			email: String!
		}
		
		input User {
			gender: Gender!
			dateOfBirth: String!
			state: String!
			tobaccoUse: Boolean
			firstName: String!
			lastName: String!
			phone: String!
			email: String!
			agentInfo: AgentInfo!
			policyPreference: PolicyPreference
			coverageAmount: Int
			termInYears: Int
			notes: String
			paymentMode: String
		}
		`
}