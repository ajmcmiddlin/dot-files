set -x EDITOR nvim

source (dirname (status -f))"/init-ssh-agent.fish"
if not ssh-add -l | grep /home/andrew/\.ssh/id_rsa > /dev/null
  ssh-add
end

