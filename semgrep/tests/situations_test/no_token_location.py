from app.config import ENVIRONMENT
from app.config import VALID_COMPANIES
from pydantic import BaseModel
from pydantic import validator


class Company(BaseModel):
    company: str

    @validator("company")
    def check_valid_company(cls, v):
        if (
            v not in VALID_COMPANIES and ENVIRONMENT != "test"
        ):  # dont validate companies if in test env
            raise Exception(f"Invalid company: {v}")
        return v
