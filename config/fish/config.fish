set -x EDITOR nvim

source (dirname (status -f))"/init-ssh-agent.fish"

if begin not ssh-add -l | grep /home/andrew/\.ssh/id_rsa > /dev/null; and status --is-interactive; end
  ssh-add
end

