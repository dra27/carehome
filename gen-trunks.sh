#!/usr/bin/env bash

# XXX Shared with gen-baseline.sh

opam_repository='c603e596bfaf195a5c1a3baf992b11c1de5ad35b'

declare -A DISTROS=(\
  ['Alpine']='alpine-3.22' \
  ['Arch']='archlinux' \
  ['CentOS']='centos-10' \
  ['Debian']='debian-13' \
  ['Fedora']='fedora-43' \
  ['openSUSE']='opensuse-16.0' \
  ['OracleLinux']='oraclelinux-10' \
  ['Ubuntu']='ubuntu-25.10')

# Branches 3.07-3.11 are completely closed in ocaml/ocaml

declare -A RELEASES=(\
  ['3.12']='3.12.1' \
  ['4.00']='4.00.1' \
  ['4.01']='4.01.0' \
  ['4.02']='4.02.3' \
  ['4.03']='4.03.0' \
  ['4.04']='4.04.2' \
  ['4.05']='4.05.0' \
  ['4.06']='4.06.1' \
  ['4.07']='4.07.1' \
  ['4.08']='4.08.1' \
  ['4.09']='4.09.1' \
  ['4.10']='4.10.2' \
  ['4.11']='4.11.2' \
  ['4.12']='4.12.1' \
  ['4.13']='4.13.1' \
  ['4.14']='4.14.2' \
  ['5.0']='5.0.0' \
  ['5.1']='5.1.1' \
  ['5.2']='5.2.1' \
  ['5.3']='5.3.0' \
  ['5.4']='5.4.0')

declare -A OPAM=(\
  ['alpine-3.20']='opam-2.3' \
  ['fedora-39']='opam-2.2')

# 3.12 - can't build with gcc >= 14
# 4.00 - can't build with gcc >= 10
# 4.01 - can't build with gcc >= 10
# 4.02-4.13 - can't build with gcc >= 14

declare -A BREAKAGE=(\
  # Geriatrics only - not currently built by the base image builder
  # XXX Both of these I think are gcc-14 related
  # XXX Some implicit definition apparently fixed in 4.03.0
  ['Alpine-3.07']='alpine-3.20' \
  ['Alpine-3.08']='alpine-3.20' \
  ['Alpine-3.09']='alpine-3.20' \
  ['Alpine-3.10']='alpine-3.20' \
  ['Alpine-3.11']='alpine-3.20' \
  ['Alpine-3.12']='alpine-3.20' \
  ['Alpine-3.12-trunk']='alpine-3.20' \
  ['Alpine-4.00']='alpine-3.20' \
  ['Alpine-4.00-trunk']='alpine-3.12' \
  ['Alpine-4.01']='alpine-3.20' \
  ['Alpine-4.01-trunk']='alpine-3.12' \
  ['Alpine-4.02']='alpine-3.20' \
  ['Alpine-4.02-trunk']='alpine-3.20' \
  ['Arch-3.07']='skip' \
  ['Arch-3.08']='skip' \
  ['Arch-3.09']='skip' \
  ['Arch-3.10']='skip' \
  ['Arch-3.11']='skip' \
  ['Arch-3.12']='skip' \
  ['Arch-3.12-trunk']='skip' \
  ['Arch-4.00']='skip' \
  ['Arch-4.00-trunk']='skip' \
  ['Arch-4.01']='skip' \
  ['Arch-4.01-trunk']='skip' \
  ['Arch-4.02']='skip' \
  ['Arch-4.02-trunk']='skip' \
  ['CentOS-3.07']='centos-9' \
  ['CentOS-3.08']='centos-9' \
  ['CentOS-3.09']='centos-9' \
  ['CentOS-3.10']='centos-9' \
  ['CentOS-3.11']='centos-9' \
  ['CentOS-3.12']='centos-9' \
  ['CentOS-3.12-trunk']='centos-9' \
  ['CentOS-4.00']='centos-9' \
  ['CentOS-4.00-trunk']='centos-8' \
  ['CentOS-4.01']='centos-9' \
  ['CentOS-4.01-trunk']='centos-8' \
  ['CentOS-4.02']='centos-9' \
  ['CentOS-4.02-trunk']='centos-9' \
  ['Debian-3.07']='debian-12' \
  ['Debian-3.08']='debian-12' \
  ['Debian-3.09']='debian-12' \
  ['Debian-3.10']='debian-12' \
  ['Debian-3.11']='debian-12' \
  ['Debian-3.12']='debian-12' \
  ['Debian-3.12-trunk']='debian-12' \
  ['Debian-4.00']='debian-12' \
  ['Debian-4.00-trunk']='debian-10' \
  ['Debian-4.01']='debian-12' \
  ['Debian-4.01-trunk']='debian-10' \
  ['Debian-4.02']='debian-12' \
  ['Debian-4.02-trunk']='debian-12' \
  ['Fedora-3.07']='fedora-39' \
  ['Fedora-3.08']='fedora-39' \
  ['Fedora-3.09']='fedora-39' \
  ['Fedora-3.10']='fedora-39' \
  ['Fedora-3.11']='fedora-39' \
  ['Fedora-3.12']='fedora-39' \
  ['Fedora-3.12-trunk']='fedora-39' \
  ['Fedora-4.00']='fedora-39' \
  ['Fedora-4.00-trunk']='skip' \
  ['Fedora-4.01']='fedora-39' \
  ['Fedora-4.01-trunk']='skip' \
  ['Fedora-4.02']='fedora-39' \
  ['Fedora-4.02-trunk']='fedora-39' \
  ['openSUSE-3.07']='opensuse-15.6' \
  ['openSUSE-3.08']='opensuse-15.6' \
  ['openSUSE-3.09']='opensuse-15.6' \
  ['openSUSE-3.10']='opensuse-15.6' \
  ['openSUSE-3.11']='opensuse-15.6' \
  ['openSUSE-3.12']='opensuse-15.6' \
  ['openSUSE-3.12-trunk']='opensuse-15.6' \
  ['openSUSE-4.00']='opensuse-15.6' \
  ['openSUSE-4.00-trunk']='opensuse-15.6' \
  ['openSUSE-4.01']='opensuse-15.6' \
  ['openSUSE-4.01-trunk']='opensuse-15.6' \
  ['openSUSE-4.02']='opensuse-15.6' \
  ['openSUSE-4.02-trunk']='opensuse-15.6' \
  ['OracleLinux-3.07']='oraclelinux-9' \
  ['OracleLinux-3.08']='oraclelinux-9' \
  ['OracleLinux-3.09']='oraclelinux-9' \
  ['OracleLinux-3.10']='oraclelinux-9' \
  ['OracleLinux-3.11']='oraclelinux-9' \
  ['OracleLinux-3.12']='oraclelinux-9' \
  ['OracleLinux-3.12-trunk']='oraclelinux-9' \
  ['OracleLinux-4.00']='oraclelinux-9' \
  ['OracleLinux-4.00-trunk']='oraclelinux-8' \
  ['OracleLinux-4.01']='oraclelinux-9' \
  ['OracleLinux-4.01-trunk']='oraclelinux-8' \
  ['OracleLinux-4.02']='oraclelinux-9' \
  ['OracleLinux-4.02-trunk']='oraclelinux-9' \
  ['Ubuntu-3.07']='ubuntu-24.04' \
  ['Ubuntu-3.08']='ubuntu-24.04' \
  ['Ubuntu-3.09']='ubuntu-24.04' \
  ['Ubuntu-3.10']='ubuntu-24.04' \
  ['Ubuntu-3.11']='ubuntu-24.04' \
  ['Ubuntu-3.12']='ubuntu-24.04' \
  ['Ubuntu-3.12-trunk']='ubuntu-24.04' \
  ['Ubuntu-4.00']='ubuntu-24.04' \
  ['Ubuntu-4.00-trunk']='ubuntu-20.04' \
  ['Ubuntu-4.01']='ubuntu-24.04' \
  ['Ubuntu-4.01-trunk']='ubuntu-20.04' \
  ['Ubuntu-4.02']='ubuntu-24.04' \
  ['Ubuntu-4.02-trunk']='ubuntu-24.04' \
  # XXX Missing get_cwd warning (configure issue)
  ['Alpine-4.03-trunk']='alpine-3.20' \
  ['Alpine-4.04-trunk']='alpine-3.20' \
  ['Alpine-4.05-trunk']='alpine-3.20' \
  ['Alpine-4.06-trunk']='alpine-3.20' \
  ['Alpine-4.07-trunk']='alpine-3.20' \
  ['Arch-4.03-trunk']='skip' \
  ['Arch-4.04-trunk']='skip' \
  ['Arch-4.05-trunk']='skip' \
  ['Arch-4.06-trunk']='skip' \
  ['Arch-4.07-trunk']='skip' \
  ['CentOS-4.03-trunk']='centos-9' \
  ['CentOS-4.04-trunk']='centos-9' \
  ['CentOS-4.05-trunk']='centos-9' \
  ['CentOS-4.06-trunk']='centos-9' \
  ['CentOS-4.07-trunk']='centos-9' \
  ['Debian-4.03-trunk']='debian-12' \
  ['Debian-4.04-trunk']='debian-12' \
  ['Debian-4.05-trunk']='debian-12' \
  ['Debian-4.06-trunk']='debian-12' \
  ['Debian-4.07-trunk']='debian-12' \
  ['Fedora-4.03-trunk']='fedora-39' \
  ['Fedora-4.04-trunk']='fedora-39' \
  ['Fedora-4.05-trunk']='fedora-39' \
  ['Fedora-4.06-trunk']='fedora-39' \
  ['Fedora-4.07-trunk']='fedora-39' \
  ['openSUSE-4.03-trunk']='opensuse-15.6' \
  ['openSUSE-4.04-trunk']='opensuse-15.6' \
  ['openSUSE-4.05-trunk']='opensuse-15.6' \
  ['openSUSE-4.06-trunk']='opensuse-15.6' \
  ['openSUSE-4.07-trunk']='opensuse-15.6' \
  ['OracleLinux-4.03-trunk']='oraclelinux-9' \
  ['OracleLinux-4.04-trunk']='oraclelinux-9' \
  ['OracleLinux-4.05-trunk']='oraclelinux-9' \
  ['OracleLinux-4.06-trunk']='oraclelinux-9' \
  ['OracleLinux-4.07-trunk']='oraclelinux-9' \
  ['Ubuntu-4.03-trunk']='ubuntu-24.04' \
  ['Ubuntu-4.04-trunk']='ubuntu-24.04' \
  ['Ubuntu-4.05-trunk']='ubuntu-24.04' \
  ['Ubuntu-4.06-trunk']='ubuntu-24.04' \
  ['Ubuntu-4.07-trunk']='ubuntu-24.04' \
  # XXX This is the new GCC 15 issue
  ['Arch-4.08-trunk']='skip' \
  ['Arch-4.09-trunk']='skip' \
  ['Arch-4.10-trunk']='skip' \
  ['Arch-4.11-trunk']='skip' \
  ['Arch-4.12-trunk']='skip' \
  ['Arch-4.13-trunk']='skip' \
  ['Fedora-4.08-trunk']='fedora-41' \
  ['Fedora-4.09-trunk']='fedora-41' \
  ['Fedora-4.10-trunk']='fedora-41' \
  ['Fedora-4.11-trunk']='fedora-41' \
  ['Fedora-4.12-trunk']='fedora-41' \
  ['Fedora-4.13-trunk']='fedora-41' \
  ['openSUSE-4.08-trunk']='opensuse-15.6' \
  ['openSUSE-4.09-trunk']='opensuse-15.6' \
  ['openSUSE-4.10-trunk']='opensuse-15.6' \
  ['openSUSE-4.11-trunk']='opensuse-15.6' \
  ['openSUSE-4.12-trunk']='opensuse-15.6' \
  ['openSUSE-4.13-trunk']='opensuse-15.6' \
  ['Ubuntu-4.08-trunk']='ubuntu-25.04' \
  ['Ubuntu-4.09-trunk']='ubuntu-25.04' \
  ['Ubuntu-4.10-trunk']='ubuntu-25.04' \
  ['Ubuntu-4.11-trunk']='ubuntu-25.04' \
  ['Ubuntu-4.12-trunk']='ubuntu-25.04' \
  ['Ubuntu-4.13-trunk']='ubuntu-25.04')

names=($(printf '%s\n' "${!DISTROS[@]}" | sort -f))
versions=($(printf '%s\n' "${!RELEASES[@]}" | sort -f))

declare -A BASES

cat <<'END'
FROM ocaml/opam:ubuntu-25.04-opam AS cache
RUN git clone -o upstream https://github.com/ocaml/ocaml.git && git -C ocaml checkout fe9e6f41d0942869bf95a43c3244ffe237f9439e
END

for distro in "${names[@]}"; do
  for release in "${versions[@]}"; do
    image="${BREAKAGE[$distro-$release-trunk]}"
    [[ -n $image ]] || image="${DISTROS[$distro]}"
    [[ $image != skip ]] || continue
    case $release in
      3.12|4.00|4.01|4.02|4.03|4.04|4.05|4.06)
        preprocess="sed -i -e 's/ -Werror//' configure && "
        configure=''
        parallel=''
        target=' world.opt';;
      4.07|4.08|4.09)
        preprocess="sed -i -e 's/ -Werror//' configure && "
        configure=''
        parallel=' -j'
        target=' world.opt';;
      4.10|4.11)
        preprocess="sed -i -e 's/ -Werror//' configure && "
        configure=''
        parallel=' -j'
        target='';;
      *)
        preprocess=''
        configure=' --disable-warn-error'
        parallel=' -j'
        target='';;
    esac
    cat <<END
FROM ocaml/opam:$image-opam AS ${distro,,}-$release
COPY --from=cache --chown=opam:opam /home/opam/ocaml /home/opam/ocaml
WORKDIR /home/opam/ocaml
RUN git checkout -b $release upstream/$release
RUN $preprocess./configure$configure && make$parallel$target
RUN ./ocamlopt.opt -v | head -n 1 > /home/opam/vnum
END
  done
done

cat <<END
FROM ocaml/opam:${DISTROS['Ubuntu']}-opam AS collect
END

prefix='RUN'
for distro in "${names[@]}"; do
  for release in "${versions[@]}"; do
    image="${BREAKAGE[$distro-$release-trunk]}"
    [[ -n $image ]] || image="${DISTROS[$distro]}"
    [[ $image != 'skip' ]] || continue
    cat <<END
$prefix --mount=from=${distro,,}-$release,src=/home/opam,dst=/mnt/opam-$distro-$release \\
END
    prefix='   '
  done
done
cat <<'END'
    cat /mnt/opam-*/vnum > /home/opam/results
END

cat <<'END'
FROM ubuntu-5.4 AS finished
RUN echo true
END
