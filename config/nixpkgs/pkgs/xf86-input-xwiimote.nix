{ stdenv, pkgconfig, fetchurl, xwiimote, udev, xproto, xorgserver }:

stdenv.mkDerivation rec {
  name = "xf86-input-xwiimote";
  src = fetchurl {
    url = "https://github.com/dvdhrm/xf86-input-xwiimote/releases/download/${name}-0.5/${name}-0.5.tar.xz";
    sha256 = "1gwnbnhybrmhmrp4i2b5yr624mvrr9bpk0r2592y8rm7c5lzqfba";
  };

  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ xwiimote udev xproto xorgserver];

  # preConfigure = ''
  #     mkdir -p $out/share/X11/xorg.conf.d
  #     configureFlags="--with-xorg-module-dir=$out/lib/xorg/modules
  #     --with-sdkdir=$out/include/xorg --with-xorg-conf-dir=$out/share/X11/xorg.conf.d"
  #   '';

  meta = {
    homepage = http://dvdhrm.github.io/xwiimote;
    description = "X.Org Wii Remote Input Driver";
    platforms = stdenv.lib.platforms.linux;
  };

  postInstallPhase = ''
    mkdir -p "$out/share/X11/xorg.conf.d/"
    cp "60-xorg-xwiimote.conf" "$out/share/X11/xorg.conf.d/60-xorg-xwiimote.conf"
  '';
}
