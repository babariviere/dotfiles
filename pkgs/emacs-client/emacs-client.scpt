on emacsclient(input)
	set scr to "/etc/profiles/per-user/bastienriviere/bin/emacsclient -n -c -a \"/etc/profiles/per-user/bastienriviere/bin/emacs\""
	if input is not "" then
		set scr to script & " '" & input & "'"
	end if
	do shell script scr
end emacsclient

on open location input
	emacsclient(input)
end open location

on open inputs
	repeat with raw_input in inputs
		set input to POSIX path of raw_input
		emacsclient(input)
	end repeat
end open

on run
	do shell script emacsclient("")
end run