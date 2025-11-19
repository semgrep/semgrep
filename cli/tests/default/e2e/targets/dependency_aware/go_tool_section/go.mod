module github.com/foo

go 1.18

require (
	github.com/go-chi/chi/v5 v5.0.7
)

tool (
	github.com/joho/godotenv/cmd/godotenv
	github.com/swaggo/swag/cmd/swag
	github.com/campoy/jsonenums // jsonenums is used for generating (Un)MarshalJSON methods
)