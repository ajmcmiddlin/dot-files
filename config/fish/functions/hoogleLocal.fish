function hoogleLocal
  if [ -z $argv ];
      set hoogle_port 8080
  else
      set hoogle_port $argv[1]
  end

  nohup nix-shell --run "hoogle server --local --port=$hoogle_port &"
end
