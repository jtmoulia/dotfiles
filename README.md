# dotfiles

My dotfiles. They're organized as ansible roles.

## Prelude

Set your root password to something secure:

```shell
# passwd
```

If you want to create a user account (`jtmoulia` for the sake of history):

```shell
# useradd --create-home -G wheel jtmoulia
# passwd jtmoulia
```

Note: the ansible deploy scripts will ensure that the user is properly configured.

Make sure that you've installed the necessary deps for running `wifi-menu`,
otherwise you won't be able to connect to the network:

```shell
# pacman -S dialog wpa_supplicant
```

Install the Python dependencies via poetry:

```shell
# pacman -S python-poetry
$ poetry install
```

## Basic Usage

To install everything run:

```shell
poetry run bin/dotsible.sh
```

You can use alternate configurations; here it's run with configuration specific to my
Lenovo Flex 4:

```shell
dotsible -p lenovo-flex-4
```

### The `dotsible` executable

The first run will create an executable in your path named `dotsible`, which can
be called instead of `poetry run bin/dotsible.sh -p $DOTSIBLE_PLAYBOOK`. The
following examples use this shortcut.

Sometimes you'll want to install only a few roles, say `ssh` and `net-utils`:

```shell
dotsible -t ssh -t net-utils
```

## Roles

Related configurations are organized into [Ansible
roles](https://docs.ansible.com/ansible/latest/user_guide/playbooks_reuse_roles.html).

All of the available roles are located under `playbooks/roles/` -- you can list
them by simply running:

```shell
ls playbooks/roles
```

Some roles have documentation under `$ROLE/README.md`.

## Playbooks

A set of roles and variables are organized into [Ansible
playbooks](https://docs.ansible.com/ansible/latest/user_guide/playbooks.html).
Typically a playbook is used to define the overall configuration for a target
environment.

The currently available playbooks are:

  - `base`: basic roles such as `git`, `bash`, and `tmux`
  - `cli`: portable CLI specific configuration
  - `lenovo-flex-4`: Arch Linux configuration for a Lenovo Flex 4
  - `lenovo-yoga`: Arch Linux configuration for a Lenovo Yoga
  - `osx`: OS X on a 2022 MacBook Pro M1 (Monterey)

## Portability

Roles and playbooks were typically designed against Arch Linux. Roles listed in
the `base` or `osx` playbooks will work under OS X.

