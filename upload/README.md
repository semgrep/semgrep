# Semgrep Rule Upload Script (Experimental)


## Usage

### Dockerhub
```
docker run -v $(pwd):/src -e SEMGREP_UPLOAD_DEPLOYMENT=DEPLOYMENT_ID -e SEMGREP_TOKEN=SOME_TOKEN returntocorp/semgrep-upload:latest /src/RULE_YAML
```

## Local Dev

```
pipenv install
python upload_private_rules.py rule.yaml --deployment_id DEPLOYMENT_ID --token SOME_TOKEN
```

## Docker Dev

```
docker build -t semgrep-upload .
docker run -v $(pwd):/src -e SEMGREP_UPLOAD_DEPLOYMENT=DEPLOYMENT_ID -e SEMGREP_TOKEN=SOME_TOKEN upload /src/RULE_YAML
```

## Deploy

```
docker build -t returntocorp/semgrep-upload
docker push returntocorp/semgrep-upload
```
