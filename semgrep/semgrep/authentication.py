import json
from pathlib import Path

class Authentication:
    """
    Load,Write,Delete local config file that for now is only used for auth
    """
    FILE_PATH = Path.home() / ".semgrep/config"

    def __init__(self):
        self.token: Optional[str] = None

    def load(self) -> bool:
        """
        Reads config in FILE_PATH

        Returns False if file does not exist
        """
        if not Authentication.FILE_PATH.exists():
            return False

        with Authentication.FILE_PATH.open() as f:
            # Error handling if not valid json
            config = json.load(f)


        self.token = config.get("TOKEN", None)
        return True

    def save(self) -> None:
        Authentication.FILE_PATH.parent.mkdir(parents=True, exist_ok=True)
        with Authentication.FILE_PATH.open("w") as f:
            json.dump({"TOKEN": self.token}, f)

    @classmethod
    def delete(cls):
        """
        Deletes configuration file
        """
        Authentication.FILE_PATH.unlink(missing_ok=True)
