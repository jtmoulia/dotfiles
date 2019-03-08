# Dotsible Role

The `dotsible` role provides helpers for calling `bin/dotsible.sh`, the script
used to deploy these roles. It install a script called `dotsible` into the
user's bin folder.

## Usage

To install the roles in the default playbook run:

    dotsible

To install a specific tag, say `dotsible`, from the playbook's roles run:

    dotsible -- -t dotsible
