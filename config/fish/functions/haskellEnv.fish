function haskellEnv
	nix-shell --run fish -p "haskellPackages.ghcWithPackages (hp: with hp; [$argv])"
end
