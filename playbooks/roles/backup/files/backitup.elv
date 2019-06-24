#!/usr/bin/env elvish

fn backup []{
  backup_root = /mnt/jtdrive/backup
  backup_targets = [
    &scratch= ~/scratch
    &Downloads= ~/Downloads
  ]

  # TODO
  # if (test ! -e $backup_root) {
  #   raise error
  # }

  for target [(keys $backup_targets)] {
    backup_source = $backup_targets[$target]
    backup_target = $backup_root"/"$target
    echo Backing up $backup_source to $backup_target ...
    rsync -a --delete $backup_source $backup_target
  }
}

backup
