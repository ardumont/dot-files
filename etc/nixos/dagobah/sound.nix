{ ... }:

{
  # disable pc speaker audio card
  # blacklistedKernelModules = [ "snd_pcsp" ];

  boot.extraModprobeConfig = ''
    options snd slots=snd-hda-intel
    # disable first card and enable the second one
    options snd_hda_intel enable=0,1
  '';
}
