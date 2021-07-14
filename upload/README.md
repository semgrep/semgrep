# Semgrep Rule Upload Script (Experimental)


## Usage

### Dockerhub
```
docker pull returntocorp/semgrep-upload:latest
docker run -v $(pwd):/src -e SEMGREP_UPLOAD_DEPLOYMENT=DEPLOYMENT_ID -e SEMGREP_TOKEN=SOME_TOKEN returntocorp/semgrep-upload:latest /src/RULE_YAML
```

### Using different registry_url

```
docker run -v $(pwd):/src -e SEMGREP_UPLOAD_DEPLOYMENT=DEPLOYMENT_ID -e SEMGREP_TOKEN=SOME_TOKEN -e SEMGREP_REGISTRY_BASE_URL=https://staging.semgrep.dev returntocorp/semgrep-upload:latest /src/RULE_YAML
```

## Local Dev

```
make setup
pipenv install
pipenv run python upload_private_rules.py rule.yaml --deployment_id DEPLOYMENT_ID --token SOME_TOKEN
```

## Docker Dev

```
make build
docker run -v $(pwd):/src -e SEMGREP_UPLOAD_DEPLOYMENT=DEPLOYMENT_ID -e SEMGREP_TOKEN=SOME_TOKEN returntocorp/semgrep-upload /src/RULE_YAML
```

## Deploy

```
make deploy
```
