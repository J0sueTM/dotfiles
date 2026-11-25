#!/usr/bin/env bash
# Builds the two notification sounds: soft, low, bell-like notes with a slow fade.
#   done.ogg  two notes falling (E4 -> C4): settled, finished
#   ask.ogg   two notes rising  (C4 -> G4): a gentle question
# Tweak the notes or VOLUME and run again.
cd "$(dirname "$0")" || exit 1
VOLUME=0.10

# note <frequency> <start seconds>: sine + a quiet octave, 15 ms attack, slow decay
note() {
  echo "if(gte(t,$2), (sin(2*PI*$1*(t-$2)) + 0.18*sin(4*PI*$1*(t-$2))) * exp(-4.5*(t-$2)) * (1-exp(-(t-$2)/0.015)), 0)"
}

build() { # build <file> <freq1> <freq2>
  ffmpeg -v error -y -f lavfi \
    -i "aevalsrc='$VOLUME*($(note "$2" 0) + $(note "$3" 0.22))':s=48000:d=1.6" \
    -af "lowpass=f=1800,afade=t=out:st=1.2:d=0.4" -ac 2 -c:a libvorbis -q:a 5 "$1"
}

build done.ogg 329.63 261.63
build ask.ogg  261.63 392.00
