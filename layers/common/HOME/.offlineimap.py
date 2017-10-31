import os
import subprocess
import getpass

DEFAULT_ARGS = ['gpg', '--decrypt', '--quiet', '--batch']
DEFAULT_PATH = '/home/jtmoulia/.authinfo.gpg'

def _auth_parse_line(line):
    "Return a dict containing the line's auth info."
    words = line.split(' ')
    words.reverse()
    info = {}
    while words:
        label, value = words.pop(), words.pop()
        info[label] = value
    return info

def _auth_parse(string):
    "Generator for parsing the lines in `string`."
    for line in string.split('\n'):
        yield _auth_parse_line(line)

def _auth_decrypt(path):
    return subprocess.check_output(DEFAULT_ARGS + [path])

def _auth_read(path, *args):
    infos = {}
    for info in _auth_parse(_auth_decrypt(path, *args)):
        name = info.pop('login')
        infos[name] = info
    return infos

def auth_info(account, path=DEFAULT_PATH):
    """Load the auth info from an encrypted authinfo file."""
    return _auth_read(path)[account]

def auth_pass(account, *args):
    """Return the password associated with `account` in authinfo."""
    return auth_info(account, *args)['password']

def pass_get(pass_name, key):
    """Use ``pass`` to get a particular line from a password file.

    See ``pass-get``.
    """
    result = subprocess.check_output(["pass-get", pass_name, key]).strip()
    if result:
        return result
