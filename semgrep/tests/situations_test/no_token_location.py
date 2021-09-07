from pydantic import BaseModel, validator
from app.config import VALID_COMPANIES, ENVIRONMENT


class Company(BaseModel):
    company: str

    @validator("company")
    def check_valid_company(cls, v):
        if v not in VALID_COMPANIES and ENVIRONMENT != "test":  # dont validate companies if in test env
            raise Exception(f"Invalid company: {v}")
        return v

