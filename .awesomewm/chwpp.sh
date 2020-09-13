# change feh wallpaper every 2 minutes

while [ false ]
do
    feh --bg-fill --randomize ~/Pictures/wallpapers/*.jpg
    sleep 120
done 
