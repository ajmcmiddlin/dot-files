function svgToPng
	set out (echo $argv[0] | sed -e 's/\.svg/\.png/')
convert -background none -size $argv[1] $argv[0] out
end
