#!/usr/bin/env bash

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

declare -A RELEASES=(\
  ['3.07']='3.07+2' \
  ['3.08']='3.08.4' \
  ['3.09']='3.09.3' \
  ['3.10']='3.10.2' \
  ['3.11']='3.11.2' \
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
  ['Alpine-4.00']='alpine-3.20' \
  ['Alpine-4.01']='alpine-3.20' \
  ['Alpine-4.02']='alpine-3.20' \
  ['Arch-3.07']='skip' \
  ['Arch-3.08']='skip' \
  ['Arch-3.09']='skip' \
  ['Arch-3.10']='skip' \
  ['Arch-3.11']='skip' \
  ['Arch-3.12']='skip' \
  ['Arch-4.00']='skip' \
  ['Arch-4.01']='skip' \
  ['Arch-4.02']='skip' \
  ['CentOS-3.07']='centos-9' \
  ['CentOS-3.08']='centos-9' \
  ['CentOS-3.09']='centos-9' \
  ['CentOS-3.10']='centos-9' \
  ['CentOS-3.11']='centos-9' \
  ['CentOS-3.12']='centos-9' \
  ['CentOS-4.00']='centos-9' \
  ['CentOS-4.01']='centos-9' \
  ['CentOS-4.02']='centos-9' \
  ['Debian-3.07']='debian-12' \
  ['Debian-3.08']='debian-12' \
  ['Debian-3.09']='debian-12' \
  ['Debian-3.10']='debian-12' \
  ['Debian-3.11']='debian-12' \
  ['Debian-3.12']='debian-12' \
  ['Debian-4.00']='debian-12' \
  ['Debian-4.01']='debian-12' \
  ['Debian-4.02']='debian-12' \
  ['Fedora-3.07']='fedora-39' \
  ['Fedora-3.08']='fedora-39' \
  ['Fedora-3.09']='fedora-39' \
  ['Fedora-3.10']='fedora-39' \
  ['Fedora-3.11']='fedora-39' \
  ['Fedora-3.12']='fedora-39' \
  ['Fedora-4.00']='fedora-39' \
  ['Fedora-4.01']='fedora-39' \
  ['Fedora-4.02']='fedora-39' \
  ['openSUSE-3.07']='opensuse-15.6' \
  ['openSUSE-3.08']='opensuse-15.6' \
  ['openSUSE-3.09']='opensuse-15.6' \
  ['openSUSE-3.10']='opensuse-15.6' \
  ['openSUSE-3.11']='opensuse-15.6' \
  ['openSUSE-3.12']='opensuse-15.6' \
  ['openSUSE-4.00']='opensuse-15.6' \
  ['openSUSE-4.01']='opensuse-15.6' \
  ['openSUSE-4.02']='opensuse-15.6' \
  ['OracleLinux-3.07']='oraclelinux-9' \
  ['OracleLinux-3.08']='oraclelinux-9' \
  ['OracleLinux-3.09']='oraclelinux-9' \
  ['OracleLinux-3.10']='oraclelinux-9' \
  ['OracleLinux-3.11']='oraclelinux-9' \
  ['OracleLinux-3.12']='oraclelinux-9' \
  ['OracleLinux-4.00']='oraclelinux-9' \
  ['OracleLinux-4.01']='oraclelinux-9' \
  ['OracleLinux-4.02']='oraclelinux-9' \
  ['Ubuntu-3.07']='ubuntu-24.04' \
  ['Ubuntu-3.08']='ubuntu-24.04' \
  ['Ubuntu-3.09']='ubuntu-24.04' \
  ['Ubuntu-3.10']='ubuntu-24.04' \
  ['Ubuntu-3.11']='ubuntu-24.04' \
  ['Ubuntu-3.12']='ubuntu-24.04' \
  ['Ubuntu-4.00']='ubuntu-24.04' \
  ['Ubuntu-4.01']='ubuntu-24.04' \
  ['Ubuntu-4.02']='ubuntu-24.04' \
  # XXX Missing get_cwd warning (configure issue)
  ['Alpine-4.03']='alpine-3.20' \
  ['Alpine-4.04']='alpine-3.20' \
  ['Alpine-4.05']='alpine-3.20' \
  ['Alpine-4.06']='alpine-3.20' \
  ['Alpine-4.07']='alpine-3.20' \
  ['Arch-4.03']='skip' \
  ['Arch-4.04']='skip' \
  ['Arch-4.05']='skip' \
  ['Arch-4.06']='skip' \
  ['Arch-4.07']='skip' \
  ['CentOS-4.03']='centos-9' \
  ['CentOS-4.04']='centos-9' \
  ['CentOS-4.05']='centos-9' \
  ['CentOS-4.06']='centos-9' \
  ['CentOS-4.07']='centos-9' \
  ['Debian-4.03']='debian-12' \
  ['Debian-4.04']='debian-12' \
  ['Debian-4.05']='debian-12' \
  ['Debian-4.06']='debian-12' \
  ['Debian-4.07']='debian-12' \
  ['Fedora-4.03']='fedora-39' \
  ['Fedora-4.04']='fedora-39' \
  ['Fedora-4.05']='fedora-39' \
  ['Fedora-4.06']='fedora-39' \
  ['Fedora-4.07']='fedora-39' \
  ['openSUSE-4.03']='opensuse-15.6' \
  ['openSUSE-4.04']='opensuse-15.6' \
  ['openSUSE-4.05']='opensuse-15.6' \
  ['openSUSE-4.06']='opensuse-15.6' \
  ['openSUSE-4.07']='opensuse-15.6' \
  ['OracleLinux-4.03']='oraclelinux-9' \
  ['OracleLinux-4.04']='oraclelinux-9' \
  ['OracleLinux-4.05']='oraclelinux-9' \
  ['OracleLinux-4.06']='oraclelinux-9' \
  ['OracleLinux-4.07']='oraclelinux-9' \
  ['Ubuntu-4.03']='ubuntu-24.04' \
  ['Ubuntu-4.04']='ubuntu-24.04' \
  ['Ubuntu-4.05']='ubuntu-24.04' \
  ['Ubuntu-4.06']='ubuntu-24.04' \
  ['Ubuntu-4.07']='ubuntu-24.04' \
  # XXX This is the new GCC 15 issue
  ['Arch-4.08']='skip' \
  ['Arch-4.09']='skip' \
  ['Arch-4.10']='skip' \
  ['Arch-4.11']='skip' \
  ['Arch-4.12']='skip' \
  ['Arch-4.13']='skip' \
  ['Arch-5.0']='skip' \
  ['Fedora-4.08']='fedora-41' \
  ['Fedora-4.09']='fedora-41' \
  ['Fedora-4.10']='fedora-41' \
  ['Fedora-4.11']='fedora-41' \
  ['Fedora-4.12']='fedora-41' \
  ['Fedora-4.13']='fedora-41' \
  ['Fedora-5.0']='fedora-41' \
  ['openSUSE-4.08']='opensuse-15.6' \
  ['openSUSE-4.09']='opensuse-15.6' \
  ['openSUSE-4.10']='opensuse-15.6' \
  ['openSUSE-4.11']='opensuse-15.6' \
  ['openSUSE-4.12']='opensuse-15.6' \
  ['openSUSE-4.13']='opensuse-15.6' \
  ['openSUSE-5.0']='opensuse-15.6' \
  ['Ubuntu-4.08']='ubuntu-25.04' \
  ['Ubuntu-4.09']='ubuntu-25.04' \
  ['Ubuntu-4.10']='ubuntu-25.04' \
  ['Ubuntu-4.11']='ubuntu-25.04' \
  ['Ubuntu-4.12']='ubuntu-25.04' \
  ['Ubuntu-4.13']='ubuntu-25.04' \
  ['Ubuntu-5.0']='ubuntu-25.04')

names=($(printf '%s\n' "${!DISTROS[@]}" | sort -f))
versions=($(printf '%s\n' "${!RELEASES[@]}" | sort -f))

declare -A BASES

cat <<'END'
FROM ocaml/opam:ubuntu-25.04-opam AS cache
ENV OPAMYES="1" OPAMCONFIRMLEVEL="unsafe-yes" OPAMERRLOGLEN="0" OPAMPRECISETRACKING="1"
RUN git -C opam-repository fetch origin && git -C opam-repository checkout $opam_repository && sudo ln -f /usr/bin/opam-2.4 /usr/bin/opam && opam update
RUN for v in $(seq 0 3); do opam source ocaml-config.$v; done
END
# XXX This could be generated in a single command...
# XXX Separate list is so that testing a new compiler doesn't _have_ to invalidate the cache
#     (the other option would be to compose these caches better)
for release in 3.07+2 3.08.4 3.09.3 3.10.2 3.11.2 3.12.1 4.00.1 4.01.0 4.02.3 4.03.0 4.04.2 4.05.0 4.06.1 4.07.1 4.08.1 4.09.1 4.10.2 4.11.2 4.12.1 4.13.1 4.14.2 5.0.0 5.1.1 5.2.1 5.3.0 5.4.0; do
  cat <<END
RUN opam source ocaml-base-compiler.$release
END
done

for distro in "${names[@]}"; do
  for release in "${versions[@]}"; do
    image="${BREAKAGE[$distro-$release]}"
    [[ -n $image ]] || image="${DISTROS[$distro]}"
    [[ $image != skip ]] || continue
    if [[ ! -v "BASES[$image]" ]]; then
      BASES[$image]=1
      opam="${OPAM[$image]}"
      test -n "$opam" || opam='opam-2.4'
      cat <<END
FROM ocaml/opam:$image-opam AS $image
ENV OPAMYES="1" OPAMCONFIRMLEVEL="unsafe-yes" OPAMERRLOGLEN="0" OPAMPRECISETRACKING="1"
RUN git -C opam-repository fetch origin && git -C opam-repository checkout $opam_repository && sudo ln -f /usr/bin/$opam /usr/bin/opam && opam update && rm -rf .opam/download-cache
COPY --from=cache --chown=opam:opam /home/opam/.opam/download-cache /home/opam/.opam/download-cache

END
    fi
    cat <<END
FROM $image AS ${distro,,}-$release
RUN opam switch create ocaml ocaml-base-compiler.${RELEASES[$release]}
RUN opam exec -- ocamlopt -v | head -n 1 > /home/opam/vnum

END
  done
done

cat <<END
FROM ocaml/opam:${DISTROS['Ubuntu']}-opam AS collect
END

prefix='RUN'
for distro in "${names[@]}"; do
  for release in "${versions[@]}"; do
    image="${BREAKAGE[$distro-$release]}"
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
