systems := $(wildcard baba/system/*.scm)

HOSTNAME := $(shell hostname)

guix-lock := guix time-machine -C etc/channels.lock --

.PHONY: all
all: install system home

# Install from lock file
install:
	guix pull -C etc/channels.lock

# Update lock file
update:
	guix time-machine -C etc/channels -- describe -f channels > etc/channels.lock
	$(guix-lock) upgrade

upgrade: update install system home

system: system/${HOSTNAME}

system/%:
	sudo $(guix-lock) system reconfigure -L . baba/system/$*.scm

build/system/%:
	$(guix-lock) system build -L . baba/system/$*.scm

home: home/${HOSTNAME}

home/%:
	$(guix-lock) home reconfigure -L . baba/home/$*.scm

build/home/%:
	$(guix-lock) home build -L . baba/home/$*.scm

deploy/%:
	$(guix-lock) deploy -L . baba/deployment/$*.scm
