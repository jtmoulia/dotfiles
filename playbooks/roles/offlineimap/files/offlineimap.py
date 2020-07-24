import subprocess
from typing import Optional


DEFAULT_PASS_GET = "/home/jtmoulia/bin/pass-get"


def pass_get(pass_name: str, key: str) -> Optional[str]:
    """Use ``pass`` to get a particular line from a password file.

    See ``pass-get``.

    """
    result = subprocess.check_output([DEFAULT_PASS_GET, pass_name, key]).strip()
    if result:
        return result
