{ pkgs, ... }:

{
  # Commands executed after the system resumes from suspend-to-RAM.
  powerManagement.resumeCommands = "${pkgs.xscreensaver}/bin/xscreensaver-command -lock";
  services = {
    acpid = {
      enable = true;     # acpi
      lidEventCommands = ''
        # suspend on lid close event or do nothing
        grep -q open /proc/acpi/button/lid/LID0/state && exit 0 || systemctl suspend
      ''; # suspend on lid close
    };

    xserver = {
      # touchpad
      synaptics = {
        enable = true;
        twoFingerScroll = true;

        # - Activate palm detection
        palmDetect = false;
        # - Activate tap to click
        tapButtons = true;
        # - Activate 2 fingers tapping as right click
        # - Activate 3 fingers tapping as middle click
        buttonsMap = [ 1 3 2 ];

        # additionalOptions = ''
        #   Option "PalmDetect" "0"
        #   Option "TapButton1" "1"
        #   Option "TapButton2" "3"
        #   Option "TapButton3" "2"
        #  '';
      };
    };
  };
}
