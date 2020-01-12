for file in $XDG_CONFIG_HOME/zsh/rc.d/env.*.zsh(N); do
  source $file
done

export CHROME_EXECUTABLE=google-chrome-stable
export TIMEFMT=$'\nreal\t%E\nuser\t%U\nsys\t%S'
