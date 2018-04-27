/usr/bin/time -f '%E' \
    sudo tcpdump -i lo -q -e portrange 8000-8020 \
    > dump.log
