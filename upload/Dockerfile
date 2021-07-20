FROM python:3.9-slim

RUN pip install --no-cache-dir pipenv==2020.11.15

WORKDIR /src

COPY Pipfile* ./
RUN pipenv install --system

COPY upload_private_rules.py .
COPY rule_schema.yaml .

ENTRYPOINT ["/src/upload_private_rules.py"]
