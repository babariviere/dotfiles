set -xg fish_greeting

if [ (ibus address) = "(null)" ]
	echo "Starting ibus..."
	ibus-daemon -drx
end

if test -z $SSH_AUTH_SOCK
    echo "Starting ssh..."
    eval (ssh-agent -c)
    set -xg SSH_AUTH_SOCK $SSH_AUTH_SOCK
    set -xg SSH_AGENT_PID $SSH_AGENT_PID
    ssh-add
end

function __add_to_path
	for p in $argv
		if test -d $p
			set -xg PATH $p $PATH
		end
	end
end

function __set_if_exists
	if test -d $argv[2]
		set -xg $argv[1] $argv[2]
	end
end

__add_to_path $HOME/flutter/bin
__add_to_path $HOME/.cargo/bin
__set_if_exists FLUTTER_HOME $HOME/flutter
__set_if_exists ANDROID_HOME $HOME/android

# opam configuration
source /home/babariviere/.opam/opam-init/init.fish > /dev/null 2> /dev/null; or true

eval (direnv hook fish)
