# Defined in /tmp/fish.0ltdYg/spotify.fish @ line 2
function spot --description 'Run spotify command (play, pause, next, prev)'
	set s "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player."
  switch $argv
    case "play" "pause"
      echo $s"PlayPause"
      eval $s"PlayPause"

    case "prev"
      eval $s"Previous"

    case "next"
      eval $s"Next"
  end
end
