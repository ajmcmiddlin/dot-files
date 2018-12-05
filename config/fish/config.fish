set -x EDITOR nvim

# source (dirname (status -f))"/init-ssh-agent.fish"

function dontHaveKey
  not ssh-add -l | grep /home/andrew/\.ssh/id_rsa > /dev/null;
end

if begin dontHaveKey; and status --is-interactive; and not status --is-login; end
  ssh-add
  start-autos
end

