# Semgrep Rule Upload Script (Beta)

Users in the Team and Enterprise tier for Semgrep App can publish rules to the Semgrep Registry that are not visible to others outside their organization. This can be useful for organizations where rules may contain code-sensitive information or legal requirements prevent using a public registry.
<br />
<br />
As we continue to develop and refine this feature, we welcome and appreciate all feedback via email at product@returntocorp.com or in our [Community Slack](https://r2c.dev/slack).

## Usage

***DEPLOYMENT_ID*** can be found in Semgrep App [here](https://semgrep.dev/manage/settings)
<br />
***SEMGREP_TOKEN*** can be generated in Semgrep App [here](https://semgrep.dev/manage/settings/tokens)

### Dockerhub
In a directory with the rule file RULE_YAML you want to upload run:
```
docker pull returntocorp/semgrep-upload:latest
docker run -v $(pwd):/rules -e SEMGREP_UPLOAD_DEPLOYMENT=DEPLOYMENT_ID -e SEMGREP_TOKEN=SOME_TOKEN returntocorp/semgrep-upload:latest /rules/RULE_YAML
```

### Using different registry_url

You can change the registry URL used by setting the SEMGREP_REGISTRY_BASE_URL env variable

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
