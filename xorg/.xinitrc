[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap

case $2 in
  i3)
    exec i3

    ;;
  dwm)
    exec dwm

    ;;
  bspwm)
    exec bspwm

    ;;
  plasma)
    exec startplasma-x11

    ;;
  xfce4-session)
    exec xfce4-session

    ;;
  ?)
    exec i3

    ;;
esac
