# change feh wallpaper every 2 minutes

while [ true ]
do
    feh --bg-fill --randomize ~/Pictures/wallpapers/*.jpg
    sleep 400
done 
