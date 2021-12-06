systems := $(wildcard baba/system/*.scm)

HOSTNAME := $(shell hostname)

.PHONY: all
all: install system home

# Install from lock file
install:
	guix pull -C etc/channels.lock

# Update lock file
update:
	guix time-machine -C etc/channels -- describe -f channels > etc/channels.lock

upgrade: update install system home

system: system/${HOSTNAME}

system/%:
	sudo -E guix time-machine -C etc/channels.lock -- system reconfigure -L . baba/system/$*.scm

home: home/${HOSTNAME}

home/%:
	guix time-machine -C etc/channels.lock -- home reconfigure -L . baba/home/$*.scm

deploy/%:
	guix time-machine -C etc/channels.lock -- deploy -L . baba/deployment/$*.scm
