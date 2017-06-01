

FROM base/archlinux
RUN pacman -Sy
RUN pacman -S --noconfirm postgresql-libs
