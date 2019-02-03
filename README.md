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

Install the Python dependencies via pipenv:

```shell
# pacman -S pipenv
$ pipenv install
```

## Basic Usage

To install everything run:

```shell
pipenv run bin/dotsible.sh
```

Sometimes you'll want to install only a few roles, say `ssh` and `net-utils`:

```shell
pipenv run bin/dotsible.sh -- -t ssh -t net-utils
```

You can use alternate configurations; her it's run with configuration specific to my
Lenovo Flex 4:

```shell
pipenv run bin/dotsible.sh -p lenovo-flex-4
```
