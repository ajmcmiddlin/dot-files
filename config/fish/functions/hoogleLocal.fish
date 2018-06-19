# Defined in /tmp/fish.0ypEt9/hoogleLocal.fish @ line 1
function hoogleLocal
	nohup nix-shell --run 'hoogle server --local' &
end
