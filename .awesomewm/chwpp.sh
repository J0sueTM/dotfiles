# change feh wallpaper every 400 seconds

while [ true ]
do
    feh --bg-fill --randomize ~/Pictures/wallpapers/*.jpg
    sleep 400
done 
