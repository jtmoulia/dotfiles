import subprocess
import os


DEFAULT_PASS_GET = os.path.expanduser("~/.local/bin/pass-get")


def pass_get(pass_name, key):
    """Use ``pass`` to get a particular line from a password file.

    See ``pass-get``.

    """
    result = subprocess.check_output([DEFAULT_PASS_GET, pass_name, key], text=True).strip()
    if result:
        return result
