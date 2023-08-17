;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2019, 2020, 2021 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015, 2016, 2017, 2019, 2021, 2022, 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016, 2017, 2019, 2020 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
;;; Copyright © 2016, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Jochem Raat <jchmrt@riseup.net>
;;; Copyright © 2016-2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2016 Alex Sassmannshausen <alex@pompo.co>
;;; Copyright © 2016, 2018, 2020, 2021 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2016, 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2017 Raoul J.P. Bonnal <ilpuccio.febo@gmail.com>
;;; Copyright © 2017, 2018, 2020-2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2017 Adriano Peluso <catonano@gmail.com>
;;; Copyright © 2017, 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2018, 2019 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2018, 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019 Stephen J. Scheck <sscheck@cpan.org>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020 Paul Garlick <pgarlick@tourbillion-technology.com>
;;; Copyright © 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Malte Frank Gerdes <malte.f.gerdes@gmail.com>
;;; Copyright © 2021, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Raghav Gururajan <rg@raghavgururajan.name>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2022, 2023 Evgeny Pisemsky <evgeny@pisemsky.com>
;;; Copyright © 2022, 2023 gemmaro <gemmaro.dev@gmail.com>
;;; Copyright © 2023 Mădălin Ionel Patrașcu <madalinionel.patrascu@mdc-berlin.de>
;;; Copyright © 2023 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2023 Jake Leporte <jakeleporte@outlook.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu packages perl)
  #:use-module (srfi srfi-1)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix memoization)
  #:use-module (guix search-paths)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages image)
  #:use-module (gnu packages language)
  #:use-module (gnu packages less)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages perl-compression)
  #:use-module (gnu packages perl-maths)
  #:use-module (gnu packages perl-web)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:export (perl-extutils-pkgconfig))

;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;


(define-public perl
  ;; Yeah, Perl...  It is required early in the bootstrap process by Linux.
  (package
    (name "perl")
    (version "5.36.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://cpan/src/5.0/perl-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "02p0ljvxgay5g8s8j1kghdylkj581qx3qlwavlmgd5n3iapqaq72"))
             (patches (search-patches
                       "perl-no-sys-dirs.patch"
                       "perl-autosplit-default-time.patch"
                       "perl-reproducible-build-date.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags
       (let ((out  (assoc-ref %outputs "out"))
             (libc (assoc-ref %build-inputs "libc")))
         (list
          (string-append "-Dprefix=" out)
          (string-append "-Dman1dir=" out "/share/man/man1")
          (string-append "-Dman3dir=" out "/share/man/man3")
          "-de" "-Dcc=gcc"
          "-Uinstallusrbinperl"
          "-Dinstallstyle=lib/perl5"
          "-Duseshrplib"
          (string-append "-Dlocincpth=" libc "/include")
          (string-append "-Dloclibpth=" libc "/lib")
          "-Dusethreads"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'setup-configure
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Use the right path for `pwd'.
             (substitute* "dist/PathTools/Cwd.pm"
               (("'/bin/pwd'")
                (string-append "'" (search-input-file inputs "bin/pwd") "'")))

             ;; Build in GNU89 mode to tolerate C++-style comment in libc's
             ;; <bits/string3.h>.
             (substitute* "cflags.SH"
               (("-std=c89")
                "-std=gnu89"))))
         ,@(if (%current-target-system)
               `((add-after 'unpack 'unpack-cross
                   (lambda* (#:key native-inputs inputs #:allow-other-keys)
                     (let ((cross-checkout
                            (assoc-ref native-inputs "perl-cross")))
                       (rename-file "Artistic" "Artistic.perl")
                       (rename-file "Copying" "Copying.perl")
                       (copy-recursively cross-checkout "."))
                     (let ((bash (search-input-file inputs "bin/bash")))
                       (substitute* '("Makefile.config.SH"
                                      "cnf/config.guess"
                                      "cnf/config.sub"
                                      "cnf/configure"
                                      "cnf/configure_misc.sh"
                                      "miniperl_top")
                         (("! */bin/sh") (string-append "! " bash))
                         ((" /bin/sh") bash))
                       (substitute* '("ext/Errno/Errno_pm.PL")
                         (("\\$cpp < errno.c") "$Config{cc} -E errno.c")))))
                 (replace 'configure
                   (lambda* (#:key configure-flags outputs inputs #:allow-other-keys)
                     (let* ((out (assoc-ref outputs "out"))
                            (store-directory (%store-directory))
                            (configure-flags
                             (cons*
                              ;; `perl-cross' confuses target and host
                              (string-append "--target=" ,(%current-target-system))
                              (string-append "--prefix=" out)
                              (string-append "-Dcc=" ,(%current-target-system) "-gcc")
                              "-Dbyteorder=1234"
                              (filter (negate
                                       (lambda (x) (or (string-prefix? "-d" x)
                                                       (string-prefix? "-Dcc=" x))))
                                      configure-flags)))
                            (bash (assoc-ref inputs "bash-minimal")))
                       (format (current-error-port)
                               "running ./configure ~a\n"
                               (string-join configure-flags))
                       (apply invoke (cons "./configure" configure-flags))
                       (substitute* "config.sh"
                         (((string-append store-directory "/[^/]*-bash-[^/]*"))
                          bash))
                       (substitute* '("config.h")
                         (("^#define SH_PATH .*")
                          (string-append  "#define SH_PATH \""
                                          bash "/bin/bash\"\n"))))))
                 (add-after 'build 'touch-non-built-files-for-install
                   (lambda _
                     ;; `make install' wants to install these although they do
                     ;; not get built...
                     (with-directory-excursion "cpan"
                       (mkdir-p "Pod-Usage/blib/script")
                       (mkdir-p "Pod-Parser/blib/script")
                       (for-each (lambda (file)
                                   (call-with-output-file file
                                     (lambda (port) (display "" port))))
                                 '("Pod-Usage/blib/script/pod2text"
                                   "Pod-Usage/blib/script/pod2usage"
                                   "Pod-Checker/blib/script/podchecker"
                                   "Pod-Parser/blib/script/podselect"))))))
               `((replace 'configure
                   (lambda* (#:key configure-flags #:allow-other-keys)
                     (format #t "Perl configure flags: ~s~%" configure-flags)
                     (apply invoke "./Configure" configure-flags)))))
         (add-after 'install 'remove-extra-references
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (libc    (assoc-ref inputs
                                        ,(if (%current-target-system)
                                             "cross-libc" "libc")))
                    (config1 (car (find-files (string-append out "/lib/perl5")
                                              "^Config_heavy\\.pl$")))
                    (config2 (find-files (string-append out "/lib/perl5")
                                         "^Config\\.pm$")))
               ;; Force the library search path to contain only libc because
               ;; it is recorded in Config.pm and Config_heavy.pl; we don't
               ;; want to keep a reference to everything that's in
               ;; $LIBRARY_PATH at build time (GCC, Binutils, bzip2, file,
               ;; etc.)
               (substitute* config1
                 (("^incpth=.*$")
                  (string-append "incpth='" libc "/include'\n"))
                 (("^(libpth|plibpth|libspath)=.*$" _ variable)
                  (string-append variable "='" libc "/lib'\n")))

               (for-each (lambda (file)
                           (substitute* config2
                             (("libpth => .*$")
                              (string-append "libpth => '" libc
                                             "/lib',\n"))))
                         config2)))))))
    (inputs
     (if (%current-target-system)
       (list bash-minimal coreutils-minimal)
       '()))
    (native-inputs
     (if (%current-target-system)
         `(("perl-cross"
            ,(origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/arsv/perl-cross")
                     (commit "1.4")))
               (file-name (git-file-name "perl-cross" "1.4"))
               (sha256
                (base32 "1ydjvlhrk06ccyj4bm8by7xk90krsll2k380mc3x1mhfrc7r9gzy")))))
         '()))
    (native-search-paths (list (search-path-specification
                                (variable "PERL5LIB")
                                (files '("lib/perl5/site_perl")))))
    (synopsis "Implementation of the Perl programming language")
    (description
     "Perl is a general-purpose programming language originally developed for
text manipulation and now used for a wide range of tasks including system
administration, web development, network programming, GUI development, and
more.")
    (home-page "https://www.perl.org/")
    (license license:gpl1+)))                          ; or "Artistic"

(define-public perl-5.14
  (package
    (name "perl")
    (version "5.14.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/src/5.0/perl-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1js47zzna3v38fjnirf2vq6y0rjp8m86ysj5vagzgkig956d8gw0"))
              (patches (search-patches
                        "perl-5.14-no-sys-dirs.patch"
                        "perl-5.14-autosplit-default-time.patch"
                        "perl-5.14-module-pluggable-search.patch"))))
    (properties `((release-date . "2013-03-10")))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out  (assoc-ref outputs "out"))
                   (libc (assoc-ref inputs "libc")))
               ;; Use the right path for `pwd'.
               (substitute* "dist/Cwd/Cwd.pm"
                 (("/bin/pwd")
                  (which "pwd")))

               (invoke "./Configure"
                       (string-append "-Dprefix=" out)
                       (string-append "-Dman1dir=" out "/share/man/man1")
                       (string-append "-Dman3dir=" out "/share/man/man3")
                       "-de" "-Dcc=gcc"
                       "-Uinstallusrbinperl"
                       "-Dinstallstyle=lib/perl5"
                       "-Duseshrplib"
                       (string-append "-Dlocincpth=" libc "/include")
                       (string-append "-Dloclibpth=" libc "/lib")

                       ;; Force the library search path to contain only libc
                       ;; because it is recorded in Config.pm and
                       ;; Config_heavy.pl; we don't want to keep a reference
                       ;; to everything that's in $LIBRARY_PATH at build
                       ;; time (Binutils, bzip2, file, etc.)
                       (string-append "-Dlibpth=" libc "/lib")
                       (string-append "-Dplibpth=" libc "/lib")))))

         (add-before 'strip 'make-shared-objects-writable
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The 'lib/perl5' directory contains ~50 MiB of .so.  Make them
             ;; writable so that 'strip' actually strips them.
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib")))
               (for-each (lambda (dso)
                           (chmod dso #o755))
                         (find-files lib "\\.so$"))))))))
    (native-inputs
     (list gcc-7))
    (native-search-paths (list (search-path-specification
                                (variable "PERL5LIB")
                                (files '("lib/perl5/site_perl")))))
    (home-page "https://www.perl.org/")
    (synopsis "Implementation of the Perl programming language")
    (description
     "Perl is a general-purpose programming language originally developed for
text manipulation and now used for a wide range of tasks including system
administration, web development, network programming, GUI development, and
more.")
    (license license:gpl1+)))

(define-public perl-5.6
  (package
    (inherit perl-5.14)
    (name "perl")
    (version "5.6.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.cpan.org/src/5.0/perl-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0khk94gvc8qkwxdb98khmxbwxxdbhap7rxb9ymkha6vhpxp6zrm5"))))
    (properties `((release-date . "2003-11-15")
                  (hidden? . #t))) ;only for GHC 4.
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out  (assoc-ref outputs "out"))
                   (libc (assoc-ref inputs "libc")))
               ;; Use the right path for `pwd'.
               (substitute* "lib/Cwd.pm"
                 (("/bin/pwd")
                  (which "pwd")))

               (invoke "./Configure"
                       (string-append "-Dprefix=" out)
                       (string-append "-Dman1dir=" out "/share/man/man1")
                       (string-append "-Dman3dir=" out "/share/man/man3")
                       "-de" "-Dcc=gcc"
                       "-Uinstallusrbinperl"
                       "-Dinstallstyle=lib/perl5"
                       "-Duseshrplib"
                       (string-append "-Dlocincpth=" libc "/include")
                       (string-append "-Dloclibpth=" libc "/lib")

                       ;; Force the library search path to contain only libc
                       ;; because it is recorded in Config.pm and
                       ;; Config_heavy.pl; we don't want to keep a reference
                       ;; to everything that's in $LIBRARY_PATH at build
                       ;; time (Binutils, bzip2, file, etc.)
                       (string-append "-Dlibpth=" libc "/lib")
                       (string-append "-Dplibpth=" libc "/lib")))))
         (add-after 'configure 'bleh
           (lambda _
             (substitute* '("makefile"
                            "x2p/makefile")
               ((".*\\<command-line>.*") ""))
             ;; Don't look for /usr/include/errno.h.
             (substitute* "ext/Errno/Errno_pm.PL"
               (("O eq 'linux'") "O eq 'loonix'"))
             (substitute* "ext/IPC/SysV/SysV.xs"
               ((".*asm/page.h.*") ""))))
         (add-before 'strip 'make-shared-objects-writable
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The 'lib/perl5' directory contains ~50 MiB of .so.  Make them
             ;; writable so that 'strip' actually strips them.
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib")))
               (for-each (lambda (dso)
                           (chmod dso #o755))
                         (find-files lib "\\.so$"))))))))
    (native-inputs
     (list gcc-5))))

(define-public perl-algorithm-c3
  (package
    (name "perl-algorithm-c3")
    (version "0.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HA/HAARG/"
                           "Algorithm-C3-" version ".tar.gz"))
       (sha256
        (base32 "02ck52cf0yyk57354rd1rp5l0kbfwi1pvg2lh3jadvjxfrkq9x5a"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Algorithm-C3")
    (synopsis "Module for merging hierarchies using the C3 algorithm")
    (description "This module implements the C3 algorithm, which aims to
provide a sane method resolution order under multiple inheritance.")
    (license (package-license perl))))

(define-public perl-algorithm-diff
  (package
    (name "perl-algorithm-diff")
    (version "1.1903")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/T/TY/TYEMQ/"
                           "Algorithm-Diff-" version ".tar.gz"))
       (sha256
        (base32
         "0l8pk7ziz72d022hsn4xldhhb9f5649j5cgpjdibch0xng24ms1h"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Algorithm-Diff")
    (synopsis "Compute differences between two files or lists")
    (description "This is a module for computing the difference between two
files, two strings, or any other two lists of things.  It uses an intelligent
algorithm similar to (or identical to) the one used by the Unix \"diff\"
program.  It is guaranteed to find the *smallest possible* set of
differences.")
    (license (package-license perl))))

(define-public perl-aliased
  (package
    (name "perl-aliased")
    (version "0.34")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "aliased-" version ".tar.gz"))
       (sha256
        (base32
         "1syyqzy462501kn5ma9gl6xbmcahqcn4qpafhsmpz0nd0x2m4l63"))))
    (build-system perl-build-system)
    (native-inputs (list perl-module-build))
    (home-page "https://metacpan.org/release/aliased")
    (synopsis "Use shorter versions of class names")
    (description "The alias module loads the class you specify and exports
into your namespace a subroutine that returns the class name.  You can
explicitly alias the class to another name or, if you prefer, you can do so
implicitly.")
    (license (package-license perl))))

(define-public perl-alien-sdl
  (package
    (name "perl-alien-sdl")
    (version "1.446")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/F/FR/FROGGS/"
                           "Alien-SDL-" version ".tar.gz"))
       (sha256
        (base32 "0ajipk43syhlmw0zinbj1i6r46vdlkr06wkx7ivqjgf6qffjran9"))))
    (build-system perl-build-system)
    (arguments
     `(#:module-build-flags
       ;; XXX: For some reason, `sdl-config' reports stand-alone SDL
       ;; directory, not SDL-union provided as an input to the
       ;; package.  We force the latter with "--prefix=" option.
       (list (let ((sdl (assoc-ref %build-inputs "sdl")))
               (string-append "--with-sdl-config=" sdl "/bin/sdl-config"
                              " --prefix=" sdl)))
       #:phases
       (modify-phases %standard-phases
         ;; Fix "unrecognized option: --with-sdl-config" during build.
         ;; Reported upstream as
         ;; <https://github.com/PerlGameDev/SDL/issues/261>.  See also
         ;; <https://github.com/PerlGameDev/SDL/issues/272>.
         (add-after 'unpack 'fix-build.pl
           (lambda _
             (substitute* "Build.PL"
               (("use Getopt::Long;") "")
               (("GetOptions\\( \"travis\" => \\\\\\$travis \\);") ""))
             #t)))))
    (native-inputs
     (list perl-archive-extract
           perl-archive-zip
           perl-capture-tiny
           perl-file-sharedir
           perl-file-which
           perl-module-build
           perl-text-patch))
    (inputs
     `(("freetype" ,freetype)
       ("fontconfig" ,fontconfig)
       ("pango" ,pango)
       ("sdl" ,(sdl-union
                (list sdl sdl-gfx sdl-image sdl-mixer sdl-net sdl-ttf
                      sdl-pango)))
       ("zlib" ,zlib)))
    (home-page "https://metacpan.org/release/Alien-SDL")
    (synopsis "Get, build and use SDL libraries")
    (description
     "Alien::SDL can be used to detect and get configuration settings from an
installed SDL and related libraries.  Based on your platform it offers the
possibility to download and install prebuilt binaries or to build SDL & co.@:
from source codes.")
    (license license:perl-license)))

(define-public perl-any-moose
  (package
    (name "perl-any-moose")
    (version "0.27")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                                  "Any-Moose-" version ".tar.gz"))
              (sha256
               (base32
                "0dc55mpayrixwx8dwql0vj0jalg4rlb3k64rprc84bl0z8vkx9m8"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-mouse perl-moose))
    (home-page "https://metacpan.org/release/Any-Moose")
    (synopsis "Transparently use Moose or Mouse modules")
    (description
     "This module facilitates using @code{Moose} or @code{Mouse} modules
without changing the code.  By default, Mouse will be provided to libraries,
unless Moose is already loaded, or explicitly requested by the end-user.  End
users can force the decision of which backend to use by setting the environment
variable ANY_MOOSE to be Moose or Mouse.")
    (license (package-license perl))))

(define-public perl-app-cpanminus
  (package
    (name "perl-app-cpanminus")
    (version "1.7046")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/M/MI/MIYAGAWA/App-cpanminus-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0qpq1x24dcrm7bm2qj814nkmxg8mzkdn6wcirjd8yd578jdrv31y"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/App-cpanminus")
    (synopsis "CPAN package manager")
    (description "App::cpanminus is a script to get, unpack, build and install
modules from CPAN and does nothing else.  It's dependency free (can bootstrap
itself), requires zero configuration, and stands alone.  When running, it
requires only 10MB of RAM.")
    (license (package-license perl))))

(define-public perl-app-xml-docbook-builder
  (package
    (name "perl-app-xml-docbook-builder")
    (version "0.1003")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SH/SHLOMIF/"
                           "App-XML-DocBook-Builder-" version ".tar.gz"))
       (sha256
        (base32 "12423lk4r7m5pkm1dvk1ci6s1d6rsnnl4chnavckpmja18jyay3j"))))
    (build-system perl-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
             (add-after 'unpack 'refer-to-xsltproc
               (lambda* (#:key inputs #:allow-other-keys)
                 (substitute* (list "lib/App/XML/DocBook/Docmake.pm"
                                    "t/01-use.t")
                   (("\"xsltproc\"")
                    (format #f "\"~a\""
                            (search-input-file inputs "bin/xsltproc")))))))))
    (native-inputs
     (list perl-module-build python))
    (inputs
     (list libxslt))
    (propagated-inputs
     (list perl-class-xsaccessor perl-test-trap))
    (native-search-paths
     ;; xsltproc's search paths, to avoid propagating libxslt.
     (list (search-path-specification
            (variable "XML_CATALOG_FILES")
            (separator " ")
            (files '("xml"))
            (file-pattern "^catalog\\.xml$")
            (file-type 'regular))))
    (home-page "https://www.shlomifish.org/open-source/projects/docmake/")
    (synopsis "Translate DocBook/XML documentation into other file formats")
    (description
     "This package provides the @command{docmake} command-line tool, and the
@code{App::XML::DocBook::Docmake} and @code{App::XML::DocBook::Builder} Perl
modules.

It translates DocBook/XML mark-up into various other documentation formats such
as XHTML, RTF, PDF, and XSL-FO, using the more low-level tools.  It aims to be a
replacement for @command{xmlto}.")
    (license license:expat)))

(define-public perl-appconfig
  (package
    (name "perl-appconfig")
    (version "1.71")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/N/NE/NEILB/"
                           "AppConfig-" version ".tar.gz"))
       (sha256
        (base32
         "03vvi3mk4833mx2c6dkm9zhvakf02mb2b7wz9pk9xc7c4mq04xqi"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-pod))
    (home-page "https://metacpan.org/release/AppConfig")
    (synopsis "Configuration files and command line parsing")
    (description "AppConfig is a bundle of Perl5 modules for reading
configuration files and parsing command line arguments.")
    (license (package-license perl))))

(define-public perl-array-utils
  (package
    (name "perl-array-utils")
    (version "0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/Z/ZM/ZMIJ/Array/Array-Utils-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0w1pwvnjdpb0n6k07zbknxwx6v7y75p4jxrs594pjhwvrmzippc9"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Array-Utils")
    (synopsis "Small utils for array manipulation")
    (description "@code{Array::Utils} is a small pure-perl module containing
list manipulation routines.")
    (license (package-license perl))))

(define-public perl-async-interrupt
  (package
    (name "perl-async-interrupt")
    (version "1.26")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/ML/MLEHMANN/"
                                  "Async-Interrupt-" version ".tar.gz"))
              (sha256
               (base32
                "0nq8wqy0gsnwhiw23wsp1dmgzzbf2q1asi85yd0d7cmg4haxsmib"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-canary-stability))
    (propagated-inputs
     (list perl-common-sense))
    (home-page "https://metacpan.org/release/Async-Interrupt")
    (synopsis "Allow C/XS libraries to interrupt perl asynchronously")
    (description
     "@code{Async::Interrupt} implements a single feature only of interest
to advanced perl modules, namely asynchronous interruptions (think \"UNIX
signals\", which are very similar).

Sometimes, modules wish to run code asynchronously (in another thread,
or from a signal handler), and then signal the perl interpreter on
certain events.  One common way is to write some data to a pipe and use
an event handling toolkit to watch for I/O events.  Another way is to
send a signal.  Those methods are slow, and in the case of a pipe, also
not asynchronous - it won't interrupt a running perl interpreter.

This module implements asynchronous notifications that enable you to
signal running perl code from another thread, asynchronously, and
sometimes even without using a single syscall.")
    (license (package-license perl))))

(define-public perl-attribute-util
  (package
    (name "perl-attribute-util")
    (version "1.07")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://cpan.metacpan.org/authors/id/D/DA/DANKOGAI/"
                    "Attribute-Util-" version ".tar.gz"))
              (sha256
               (base32
                "1z79d845dy96lg0pxw0kr2za0gniwnpn963r7ccajfpj6k7jfw07"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/pod/Attribute::Util")
    (synopsis "Assorted general utility attributes")
    (description "This package provides various utility functions.  When used
without argument, this module provides four universally accessible attributes
of general interest as follows:
@itemize
@item Abstract
@item Alias
@item Memoize
@item Method
@item SigHandler
@end itemize")
    (license (package-license perl))))

(define-public perl-authen-dechpwd
  (package
    (name "perl-authen-dechpwd")
    (version "2.007")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/Z/ZE/ZEFRAM/Authen-DecHpwd-"
            version ".tar.gz"))
      (sha256
       (base32
        "0xzind7zr2prjq3zbs2j18snfpshd4xrd7igv4kp67xl0axr6fpl"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build perl-test-pod perl-test-pod-coverage))
    (propagated-inputs
     (list perl-data-integer perl-digest-crc perl-scalar-string))
    (home-page "https://metacpan.org/release/Authen-DecHpwd")
    (synopsis "DEC VMS password hashing")
    (description "@code{Authen::DecHpwd} implements the
SYS$HASH_PASSWORD password hashing function from VMS (also known as
LGI$HPWD) and some associated VMS username and password handling
functions.  The password hashing function is implemented in XS with a
pure Perl backup version for systems that cannot handle XS.")
    (license license:gpl2+)))

(define-public perl-authen-passphrase
  (package
    (name "perl-authen-passphrase")
    (version "0.008")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/Z/ZE/ZEFRAM/Authen-Passphrase-"
            version ".tar.gz"))
      (sha256
       (base32
        "0qq4krap687rxf6xr31bg5nj5dqmm1frcm7fq249v1bxc4h4bnsm"))))
  (build-system perl-build-system)
  (native-inputs
   (list perl-module-build perl-test-pod perl-test-pod-coverage))
  (propagated-inputs
   (list perl-authen-dechpwd
         perl-crypt-des
         perl-crypt-eksblowfish
         perl-crypt-mysql
         perl-crypt-passwdmd5
         perl-crypt-unixcrypt_xs
         perl-data-entropy
         perl-digest-md4
         perl-module-runtime
         perl-params-classify))
  (home-page "https://metacpan.org/release/Authen-Passphrase")
  (synopsis "Hashed passwords/passphrases as objects")
  (description "@code{Authen-Passphrase} is the base class for a
system of objects that encapsulate passphrases.  An object of this
type is a passphrase recogniser; its job is to recognise whether an
offered passphrase is the right one.  For security such passphrase
recognisers usually do not themselves know the passphrase they are
looking for; they can merely recognise it when they see it.  There are
many schemes in use to achieve this effect and the intent of this
class is to provide a consistent interface to them all.  In addition
to the base class, this module also contains implementations of
several specific passphrase schemes.")
  (license license:perl-license)))

(define-public perl-autovivification
  (package
    (name "perl-autovivification")
    (version "0.18")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/V/VP/VPIT/"
                           "autovivification-" version ".tar.gz"))
       (sha256
        (base32
         "01giacr2sx6b9bgfz6aqw7ndcnf08j8n6kwhm7880a94hmb9g69d"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/autovivification")
    (synopsis "Lexically disable autovivification")
    (description "When an undefined variable is dereferenced, it gets silently
upgraded to an array or hash reference (depending of the type of the
dereferencing).  This behaviour is called autovivification and usually does
what you mean but it may be unnatural or surprising because your variables get
populated behind your back.  This is especially true when several levels of
dereferencing are involved, in which case all levels are vivified up to the
last, or when it happens in intuitively read-only constructs like
@code{exists}.  The pragma provided by this package lets you disable
autovivification for some constructs and optionally throws a warning or an
error when it would have happened.")
    (license (package-license perl))))

(define-public perl-bareword-filehandles
  (package
    (name "perl-bareword-filehandles")
    (version "0.007")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/I/IL/ILMARI/bareword-filehandles-"
             version ".tar.gz"))
       (sha256
        (base32
         "0zy1v746pzv3vvvpr3plpykz0vfhi940q9bfypzzhynq2qvm6d21"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-b-hooks-op-check perl-extutils-depends))
    (propagated-inputs
     (list perl-b-hooks-op-check perl-lexical-sealrequirehints))
    (home-page "https://metacpan.org/release/bareword-filehandles")
    (synopsis "Disables bareword filehandles")
    (description "This module disables bareword filehandles.")
    (license (package-license perl))))

(define-public perl-browser-open
  (package
    (name "perl-browser-open")
    (version "0.04")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/C/CF/CFRANKS/Browser-Open-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0rv80n5ihy9vnrzsc3l7wlk8880cwabiljrydrdnxq1gg0lk3sxc"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Browser-Open")
    (synopsis "Open a browser in a given URL")
    (description "The functions exported by this module allow you to open URLs
in the user's browser.  A set of known commands per OS-name is tested for
presence, and the first one found is executed.  With an optional parameter,
all known commands are checked.")
    (license (package-license perl))))

(define-public perl-bsd-resource
  (package
   (name "perl-bsd-resource")
   (version "1.2911")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://cpan.metacpan.org/authors/id/J/JH/JHI/BSD-Resource-"
           version ".tar.gz"))
     (sha256
      (base32 "0g8c7825ng2m0yz5sy6838rvfdl8j3vm29524wjgf66ccfhgn74x"))))
   (build-system perl-build-system)
   (home-page "https://metacpan.org/release/BSD-Resource")
   (synopsis "BSD process resource limit and priority functions")
   (description "This package provides procedures to get and set resource
limits like @code{getrlimit} and @code{setpriority}.")
   (license license:artistic2.0)))

(define-public perl-b-hooks-endofscope
  (package
    (name "perl-b-hooks-endofscope")
    (version "0.24")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "B-Hooks-EndOfScope-" version ".tar.gz"))
       (sha256
        (base32
         "1imcqxp23yc80a7p0h56sja9glbrh4qyhgzljqd4g9habpz3vah3"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-module-runtime perl-module-implementation
           perl-sub-exporter-progressive perl-variable-magic))
    (home-page "https://metacpan.org/release/B-Hooks-EndOfScope")
    (synopsis "Execute code after a scope finished compilation")
    (description "This module allows you to execute code when perl finished
compiling the surrounding scope.")
    (license (package-license perl))))

(define-public perl-b-hooks-op-check
  (package
    (name "perl-b-hooks-op-check")
    (version "0.22")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/E/ET/ETHER/B-Hooks-OP-Check-"
             version ".tar.gz"))
       (sha256
        (base32
         "1kfdv25gn6yik8jrwik4ajp99gi44s6idcvyyrzhiycyynzd3df7"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-extutils-depends))
    (home-page "https://metacpan.org/release/B-Hooks-OP-Check")
    (synopsis "Wrap OP check callbacks")
    (description "This module allows you to wrap OP check callbacks.")
    (license (package-license perl))))

(define-public perl-b-keywords
  (package
    (name "perl-b-keywords")
    (version "1.22")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RU/RURBAN/B-Keywords-"
                           version ".tar.gz"))
       (sha256
        (base32 "0i2ksp0w9wv1qc22hrdl3k48cww64syhmv8zf6x0kgyd4081hr56"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/B-Keywords")
    (synopsis "Lists of reserved barewords and symbol names")
    (description "@code{B::Keywords} supplies several arrays of exportable
keywords: @code{@@Scalars, @@Arrays, @@Hashes, @@Filehandles, @@Symbols,
@@Functions, @@Barewords, @@TieIOMethods, @@UNIVERSALMethods and
@@ExporterSymbols}.")
    ;; GPLv2 only
    (license license:gpl2)))

(define-public perl-benchmark-timer
  (package
    (name "perl-benchmark-timer")
    (version "0.7102")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/D/DC/DCOPPIT/"
                                  "Benchmark-Timer-" version ".tar.gz"))
              (sha256
               (base32
                "1gl9ybm9hgia3ld5s11b7bv2p2hmx5rss5hxcfy6rmbzrjcnci01"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-install))
    ;; The optional input module Statistics::PointEstimation (from
    ;; Statistics-TTest) lists no license.
    (synopsis "Benchmarking with statistical confidence")
    (description
     "The Benchmark::Timer class allows you to time portions of code
conveniently, as well as benchmark code by allowing timings of repeated
trials.  It is perfect for when you need more precise information about the
running time of portions of your code than the Benchmark module will give you,
but don't want to go all out and profile your code.")
    (home-page "https://metacpan.org/release/Benchmark-Timer")
    (license license:gpl2)))

(define-public perl-bit-vector
  (package
    (name "perl-bit-vector")
    (version "7.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/ST/STBEY/"
                           "Bit-Vector-" version ".tar.gz"))
       (sha256
        (base32
         "09m96p8c0ipgz42li2ywdgy0vxb57mb5nf59j9gw7yzc3xkslv9w"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-carp-clan))
    (home-page "https://metacpan.org/release/Bit-Vector")
    (synopsis "Bit vector library")
    (description "Bit::Vector is an efficient C library which allows you to
handle bit vectors, sets (of integers), \"big integer arithmetic\" and boolean
matrices, all of arbitrary sizes.  The package also includes an
object-oriented Perl module for accessing the C library from Perl, and
optionally features overloaded operators for maximum ease of use.  The C
library can nevertheless be used stand-alone, without Perl.")
    (license (list (package-license perl) license:lgpl2.0+))))

(define-public perl-boolean
  (package
    (name "perl-boolean")
    (version "0.46")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/I/IN/INGY/"
                           "boolean-" version ".tar.gz"))
       (sha256
        (base32 "0shmiw8pmshnwj01cz8g94867hjf4vc1dkp61xlbz0rybh48ih4m"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/boolean")
    (synopsis "Boolean support for Perl")
    (description "This module provides basic Boolean support, by defining two
special objects: true and false.")
    (license (package-license perl))))

(define-public perl-business-isbn-data
  (package
    (name "perl-business-isbn-data")
    (version "20140910.003")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/B/BD/BDFOY/"
                           "Business-ISBN-Data-" version ".tar.gz"))
       (sha256
        (base32
         "1jc5jrjwkr6pqga7998zkgw0yrxgb5n1y7lzgddawxibkf608mn7"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Business-ISBN-Data")
    (synopsis "Data files for Business::ISBN")
    (description "This package provides a data pack for @code{Business::ISBN}.
These data are generated from the RangeMessage.xml file provided by the ISBN
Agency.")
    (license (package-license perl))))

(define-public perl-business-isbn
  (package
    (name "perl-business-isbn")
    (version "3.004")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/B/BD/BDFOY/"
                           "Business-ISBN-" version ".tar.gz"))
       (sha256
        (base32
         "07l3zfv8hagv37i3clvj5a1zc2jarr5phg80c93ks35zaz6llx9i"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-business-isbn-data perl-mojolicious))
    (home-page "https://metacpan.org/release/Business-ISBN")
    (synopsis "Work with International Standard Book Numbers")
    (description "This module provides tools to deal with International
Standard Book Numbers, including ISBN-10 and ISBN-13.")
    (license license:artistic2.0)))

(define-public perl-business-issn
  (package
    (name "perl-business-issn")
    (version "1.003")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/B/BD/BDFOY/"
                           "Business-ISSN-" version ".tar.gz"))
       (sha256
        (base32
         "1lcr9dabwqssjpff97ki6w8mjhvh8kfbj3csbyy28ylk35n4awhj"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Business-ISSN")
    (synopsis "Work with International Standard Serial Numbers")
    (description "This module provides tools to deal with International
Standard Serial Numbers.")
    (license (package-license perl))))

(define-public perl-business-ismn
  (package
    (name "perl-business-ismn")
    (version "1.201")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/B/BD/BDFOY/"
                           "Business-ISMN-" version ".tar.gz"))
       (sha256
        (base32 "1cpcfyaz1fl6fnm076jx2jsphw147wj6aszj2yzqrgsncjhk2cja"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-tie-cycle))
    (home-page "https://metacpan.org/release/Business-ISMN")
    (synopsis "Work with International Standard Music Numbers")
    (description "This module provides tools to deal with International
Standard Music Numbers.")
    (license (package-license perl))))

(define-public perl-cache-cache
  (package
    (name "perl-cache-cache")
    (version "1.08")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                                  "Cache-Cache-" version ".tar.gz"))
              (sha256
               (base32
                "1s6i670dc3yb6ngvdk48y6szdk5n1f4icdcjv2vi1l2xp9fzviyj"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-digest-sha1 perl-error perl-ipc-sharelite))
    (home-page "https://metacpan.org/release/Cache-Cache")
    (synopsis "Cache interface for Perl")
    (description "The Cache modules are designed to assist a developer in
persisting data for a specified period of time.  Often these modules are used
in web applications to store data locally to save repeated and redundant
expensive calls to remote machines or databases.  People have also been known
to use Cache::Cache for its straightforward interface in sharing data between
runs of an application or invocations of a CGI-style script or simply as an
easy to use abstraction of the file system or shared memory.")
    (license (package-license perl))))

(define-public perl-cache-fastmmap
  (package
    (name "perl-cache-fastmmap")
    (version "1.48")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RO/ROBM/"
                           "Cache-FastMmap-" version ".tar.gz"))
       (sha256
        (base32 "118y5lxwa092zrii7mcwnqypff7424w1dpgfkg8zlnz7h2mmnd9c"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Cache-FastMmap")
    (synopsis "Shared memory interprocess cache via mmap")
    (description "A shared memory cache through an mmap'ed file.  It's core is
written in C for performance.  It uses fcntl locking to ensure multiple
processes can safely access the cache at the same time.  It uses a basic LRU
algorithm to keep the most used entries in the cache.")
    (license (package-license perl))))

(define-public perl-capture-tiny
  (package
    (name "perl-capture-tiny")
    (version "0.48")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/D/DA/DAGOLDEN/Capture-Tiny-"
             version ".tar.gz"))
       (sha256
        (base32
         "069yrikrrb4vqzc3hrkkfj96apsh7q0hg8lhihq97lxshwz128vc"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Capture-Tiny")
    (synopsis "Capture STDOUT and STDERR from Perl, XS or external programs")
    (description
     "Capture::Tiny provides a simple, portable way to capture almost anything
sent to STDOUT or STDERR, regardless of whether it comes from Perl, from XS
code or from an external program.  Optionally, output can be teed so that it
is captured while being passed through to the original file handles.")
    (license license:asl2.0)))

(define-public perl-canary-stability
  (package
    (name "perl-canary-stability")
    (version "2013")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/ML/MLEHMANN/"
                                  "Canary-Stability-" version ".tar.gz"))
              (sha256
               (base32
                "1smnsx371x9zrqmylgq145991xh8561mraqfyrlbiz4mrxi1rjd5"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Canary-Stability")
    (synopsis "Check compatibility with the installed perl version")
    (description
     "This module is used by Schmorp's modules during configuration stage
to test the installed perl for compatibility with his modules.")
    (license (package-license perl))))

(define-public perl-carp
  (package
    (name "perl-carp")
    (version "1.50")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/X/XS/XSAWYERX/Carp-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1ngbpjyd9qi7n4h5r3q3qibd8by7rfiv7364jqlv4lbd3973n9zm"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Carp")
    (synopsis "Alternative warn and die for modules")
    (description "The @code{Carp} routines are useful in your own modules
because they act like @code{die()} or @code{warn()}, but with a message
which is more likely to be useful to a user of your module.  In the case
of @code{cluck}, @code{confess}, and @code{longmess} that context is a
summary of every call in the call-stack.  For a shorter message you can use
@code{carp} or @code{croak} which report the error as being from where your
module was called.  There is no guarantee that that is where the error was,
but it is a good educated guess.")
    (license (package-license perl))))

(define-public perl-carp-always
  (package
    (name "perl-carp-always")
    (version "0.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/F/FE/FERREIRA/Carp-Always-"
                           version ".tar.gz"))
       (sha256
        (base32 "1wb6b0qjga7kvn4p8df6k4g1pl2yzaqiln1713xidh3i454i3alq"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-base))
    (home-page "https://metacpan.org/release/Carp-Always")
    (synopsis "Warns and dies noisily with stack backtraces/")
    (description "This module is meant as a debugging aid.  It can be used to
make a script complain loudly with stack backtraces when @code{warn()}-ing or
@code{die()}ing.")
    (license (package-license perl))))

(define-public perl-carp-assert
  (package
    (name "perl-carp-assert")
    (version "0.21")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/N/NE/NEILB/"
                           "Carp-Assert-" version ".tar.gz"))
       (sha256
        (base32
         "0km5fc6r6whxh6h5yd7g1j0bi96sgk0gkda6cardicrw9qmqwkwj"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Carp-Assert")
    (synopsis "Executable comments for Perl")
    (description "Carp::Assert is intended for a purpose like the ANSI C
library assert.h.")
    (license (package-license perl))))

(define-public perl-carp-assert-more
  (package
    (name "perl-carp-assert-more")
    (version "1.26")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/P/PE/PETDANCE/"
                           "Carp-Assert-More-" version ".tar.gz"))
       (sha256
        (base32 "14x4m4dlj7pwq2r2fsmww3q3xb61cdgnrlmjh5mms3ikaln6rmmk"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-exception))
    (propagated-inputs
     (list perl-carp-assert))
    (home-page "https://metacpan.org/release/Carp-Assert-More")
    (synopsis "Convenience wrappers around Carp::Assert")
    (description "Carp::Assert::More is a set of handy assertion functions for
Perl.")
    (license license:artistic2.0)))

(define-public perl-carp-clan
  (package
    (name "perl-carp-clan")
    (version "6.08")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Carp-Clan-" version ".tar.gz"))
       (sha256
        (base32 "0237xx3rqa72sr4vdvws9r1m453h5f25bl85mdjmmk128kir4py7"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-exception))
    (home-page "https://metacpan.org/release/Carp-Clan")
    (synopsis "Report errors from a \"clan\" of modules")
    (description "This module allows errors from a clan (or family) of modules
to appear to originate from the caller of the clan.  This is necessary in
cases where the clan modules are not classes derived from each other, and thus
the Carp.pm module doesn't help.")
    (license (package-license perl))))

(define-public perl-cddb-get
  (package
    (name "perl-cddb-get")
    (version "2.28")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/F/FO/FONKIE/CDDB_get-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1jfrwvfasylcafbvb0jjm94ad4v6k99a7rf5i4qwzhg4m0gvmk5x"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/CDDB_get")
    (synopsis "Read the CDDB entry for an audio CD in your drive")
    (description "This module can retrieve information from the CDDB.")
    ;; Either GPLv2 or the "Artistic" license.
    (license (list license:gpl2 license:artistic2.0))))

(define-public circos
  (package
    (name "circos")
    (version "0.69-9")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://circos.ca/distribution/circos-" version ".tgz"))
              (sha256
               (base32 "1ll9yxbk0v64813np0qz6h8bc53qlnhg9y1053b57xgkxgmxgn1l"))
              (patches (list (search-patch "circos-remove-findbin.patch")))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; There are no tests.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (datapath (string-append out "/share/Circos"))
                    (error (string-append out "/share/Circos/error"))
                    (fonts (string-append out "/share/Circos/fonts"))
                    (data (string-append out "/share/Circos/data"))
                    (tiles (string-append out "/share/Circos/tiles"))
                    (etc (string-append out "/share/Circos/etc"))
                    (lib (string-append out "/lib/perl5/site_perl/"
                                        ,(package-version perl)))
                    (install-directory (lambda (source target)
                                         (mkdir-p target)
                                         (copy-recursively source target))))
               ;; Circos looks into a relative path for its configuration
               ;; files.  We need to provide an absolute path towards the
               ;; corresponding paths in the store.
               (substitute* '("bin/circos" "etc/colors_fonts_patterns.conf"
                              "etc/gddiag.conf" "etc/brewer.conf" "README")
                 (("<<include etc") (string-append "<<include " etc)))
               (substitute* '("etc/colors.conf" "etc/image.black.conf"
                              "etc/patterns.conf" "etc/image.conf")
                 (("<<include ") (string-append "<<include " etc "/")))
               (substitute* '("etc/fonts.conf" "fonts/README.fonts")
                 (("= fonts") (string-append "= " fonts)))
               (substitute* "etc/patterns.conf"
                 (("= tiles") (string-append "= " tiles)))
               (substitute* "lib/Circos/Error.pm"
                 (("error/configuration.missing.txt")
                  (string-append error "/configuration.missing.txt")))
               (substitute* "etc/housekeeping.conf"
                 (("# data_path = /home/martink/circos-tutorials ")
                  (string-append "data_path = " datapath)))
               (substitute* "lib/Circos/Configuration.pm"
                 (("my @possibilities = \\(")
                  (string-append "my @possibilities = ("
                                 "catfile( \"" datapath "\", $arg ), "
                                 "catfile( \"" etc "\", $arg ), "
                                 "catfile( \"" etc "/tracks\", $arg ), ")))
               (for-each install-directory
                         (list "error" "fonts" "data" "tiles" "etc" "lib")
                         (list error fonts data tiles etc lib))
               (install-file "bin/circos" bin)
               #t))))))
    (propagated-inputs
     (list perl
           perl-carp
           perl-clone
           perl-config-general
           perl-digest-md5
           perl-file-temp
           perl-font-ttf
           perl-gd
           perl-getopt-long
           perl-list-allutils
           perl-math-bezier
           perl-math-round
           perl-math-vecstat
           perl-memoize
           perl-number-format
           perl-params-validate
           perl-readonly
           perl-regexp-common
           perl-set-intspan
           perl-statistics-basic
           perl-svg
           perl-text-balanced
           perl-text-format
           perl-time-hires))
    (home-page "http://circos.ca/")
    (synopsis "Generation of circularly composited renditions")
    (description
     "Circos is a program for the generation of publication-quality, circularly
composited renditions of genomic data and related annotations.")
    (license license:gpl2+)))

(define-public perl-class-accessor
  (package
    (name "perl-class-accessor")
    (version "0.51")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/K/KA/KASEI/"
                           "Class-Accessor-" version ".tar.gz"))
       (sha256
        (base32
         "07215zzr4ydf49832vn54i3gf2q5b97lydkv8j56wb2svvjs64mz"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-sub-name))
    (home-page "https://metacpan.org/release/Class-Accessor")
    (synopsis "Automated accessor generation")
    (description "This module automagically generates accessors/mutators for
your class.")
    (license (package-license perl))))

(define-public perl-class-accessor-chained
  (package
    (name "perl-class-accessor-chained")
    (version "0.01")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RC/RCLAMP/"
                           "Class-Accessor-Chained-" version ".tar.gz"))
       (sha256
        (base32
         "1lilrjy1s0q5hyr0888kf0ifxjyl2iyk4vxil4jsv0sgh39lkgx5"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build))
    (propagated-inputs
     (list perl-class-accessor))
    (home-page "https://metacpan.org/release/Class-Accessor-Chained")
    (synopsis "Faster, but less expandable, chained accessors")
    (description "A chained accessor is one that always returns the object
when called with parameters (to set), and the value of the field when called
with no arguments.  This module subclasses Class::Accessor in order to provide
the same mk_accessors interface.")
    (license (package-license perl))))

(define-public perl-class-accessor-grouped
  (package
    (name "perl-class-accessor-grouped")
    (version "0.10014")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HA/HAARG/"
                           "Class-Accessor-Grouped-" version ".tar.gz"))
       (sha256
        (base32 "1fy48hx56n5kdn1gz66awg465qf34r0n5jam64x7zxh9zhzb1m9m"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-install perl-test-exception))
    (propagated-inputs
     (list perl-class-xsaccessor perl-module-runtime perl-sub-name))
    (home-page "https://metacpan.org/release/Class-Accessor-Grouped")
    (synopsis "Build groups of accessors")
    (description "This class lets you build groups of accessors that will call
different getters and setters.")
    (license (package-license perl))))

(define-public perl-class-c3
  (package
    (name "perl-class-c3")
    (version "0.35")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HA/HAARG/"
                           "Class-C3-" version ".tar.gz"))
       (sha256
        (base32 "0gp3czp6y0jxx4448kz37f7gdxq4vw514bvc0l98rk4glvqkq1c4"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-algorithm-c3))
    (home-page "https://metacpan.org/release//Class-C3")
    (synopsis "Pragma to use the C3 method resolution order algorithm")
    (description "This is pragma to change Perl 5's standard method resolution
order from depth-first left-to-right (a.k.a - pre-order) to the more
sophisticated C3 method resolution order.")
    (license (package-license perl))))

(define-public perl-class-c3-adopt-next
  (package
    (name "perl-class-c3-adopt-next")
    (version "0.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Class-C3-Adopt-NEXT-" version ".tar.gz"))
       (sha256
        (base32 "1xsbydmiskpa1qbmnf6n39cb83nlb432xgkad9kfhxnvm8jn4rw5"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build perl-module-build-tiny perl-test-exception))
    (propagated-inputs
     (list perl-list-moreutils perl-mro-compat))
    (home-page "https://metacpan.org/release/Class-C3-Adopt-NEXT")
    (synopsis "Drop-in replacement for NEXT")
    (description "This module is intended as a drop-in replacement for NEXT,
supporting the same interface, but using Class::C3 to do the hard work.")
    (license (package-license perl))))

(define-public perl-class-c3-componentised
  (package
    (name "perl-class-c3-componentised")
    (version "1.001002")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HA/HAARG/"
                           "Class-C3-Componentised-" version ".tar.gz"))
       (sha256
        (base32 "14wn1g45z3b5apqq7dcai5drk01hfyqydsd2m6hsxzhyvi3b2l9h"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-install perl-test-exception))
    (propagated-inputs
     (list perl-class-c3 perl-class-inspector perl-mro-compat))
    (home-page "https://metacpan.org/release/Class-C3-Componentised")
    (synopsis "Load mix-ins or components to your C3-based class")
    (description "This module will inject base classes to your module using
the Class::C3 method resolution order.")
    (license (package-license perl))))

(define-public perl-class-data-inheritable
  (package
    (name "perl-class-data-inheritable")
    (version "0.08")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/T/TM/TMTM/"
                           "Class-Data-Inheritable-" version ".tar.gz"))
       (sha256
        (base32
         "0jpi38wy5xh6p1mg2cbyjjw76vgbccqp46685r27w8hmxb7gwrwr"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Class-Data-Inheritable")
    (synopsis "Inheritable, overridable class data")
    (description "Class::Data::Inheritable is for creating accessor/mutators
to class data.  That is, if you want to store something about your class as a
whole (instead of about a single object).  This data is then inherited by your
subclasses and can be overridden.")
    (license (package-license perl))))

(define-public perl-class-date
  (package
    (name "perl-class-date")
    (version "1.1.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/Y/YA/YANICK/"
                           "Class-Date-" version ".tar.gz"))
       (sha256
        (base32 "1h7dfjxkpqbfymrf1bn7699i4fx6pbv5wvvi5zszfr8sqqkax1yf"))))
    (build-system perl-build-system)
    (arguments `(#:tests? #f))          ;timezone tests in chroot
    (home-page "https://metacpan.org/release/Class-Date")
    (synopsis "Class for easy date and time manipulation")
    (description "This module provides a general-purpose date and datetime
type for perl.")
    (license (package-license perl))))

(define-public perl-class-errorhandler
  (package
    (name "perl-class-errorhandler")
    (version "0.04")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/T/TO/TOKUHIROM/"
                                  "Class-ErrorHandler-" version ".tar.gz"))
              (sha256
               (base32
                "00j5f0z4riyq7i95jww291dpmbn0hmmvkcbrh7p0p8lpqz7jsb9l"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Class-ErrorHandler")
    (synopsis "Base class for error handling")
    (description
     "@code{Class::ErrorHandler} provides an error-handling mechanism that is generic
enough to be used as the base class for a variety of OO classes.  Subclasses inherit
its two error-handling methods, error and errstr, to communicate error messages back
to the calling program.")
    (license (package-license perl))))

(define-public perl-class-factory-util
  (package
    (name "perl-class-factory-util")
    (version "1.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/"
                           "Class-Factory-Util-" version ".tar.gz"))
       (sha256
        (base32
         "09ifd6v0c94vr20n9yr1dxgcp7hyscqq851szdip7y24bd26nlbc"))))
    (build-system perl-build-system)
    (native-inputs (list perl-module-build))
    (home-page "https://metacpan.org/release/Class-Factory-Util")
    (synopsis "Utility methods for factory classes")
    (description "This module exports methods useful for factory classes.")
    (license (package-license perl))))

(define-public perl-class-inspector
  (package
    (name "perl-class-inspector")
    (version "1.36")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/P/PL/PLICEASE/"
                           "Class-Inspector-" version ".tar.gz"))
       (sha256
        (base32
         "0kk900bp8iq7bw5jyllfb31gvf93mmp24n4x90j7qs3jlhimsafc"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Class-Inspector")
    (synopsis "Get information about a class and its structure")
    (description "Class::Inspector allows you to get information about a
loaded class.")
    (license (package-license perl))))

(define-public perl-class-load
  (package
    (name "perl-class-load")
    (version "0.25")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Class-Load-" version ".tar.gz"))
       (sha256
        (base32 "13sz4w8kwljhfcy7yjjgrgg5hv3wccr8n3iqarhyb5sjkdvzlj1a"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build-tiny perl-test-fatal perl-test-needs
           perl-test-without-module))
    (propagated-inputs
     (list perl-package-stash perl-data-optlist perl-namespace-clean
           perl-module-runtime perl-module-implementation))
    (home-page "https://metacpan.org/release/Class-Load")
    (synopsis "Working (require \"Class::Name\") and more")
    (description "\"require EXPR\" only accepts Class/Name.pm style module
names, not Class::Name.  For that, this module provides \"load_class
'Class::Name'\".")
    (license (package-license perl))))

(define-public perl-class-load-xs
  (package
    (name "perl-class-load-xs")
    (version "0.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Class-Load-XS-" version ".tar.gz"))
       (sha256
        (base32
         "1ldd4a306hjagm5v9j0gjg8y7km4v3q45bxxqmj2bzgb6vsjrhjv"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-fatal perl-test-needs perl-test-without-module))
    (inputs (list perl-class-load))
    (home-page "https://metacpan.org/release/Class-Load-XS")
    (synopsis "XS implementation of parts of Class::Load")
    (description "This module provides an XS implementation for portions of
Class::Load.")
    (license license:artistic2.0)))

(define-public perl-class-methodmaker
  (package
    (name "perl-class-methodmaker")
    (version "2.24")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SC/SCHWIGON/"
                           "class-methodmaker/Class-MethodMaker-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0a03i4k3a33qqwhykhz5k437ld5mag2vq52vvsy03gbynb65ivsy"))
       (patches (search-patches
                 "perl-class-methodmaker-reproducible.patch"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Class-MethodMaker")
    (synopsis "Create generic methods for OO Perl")
    (description "This module solves the problem of having to continually
write accessor methods for your objects that perform standard tasks.")
    (license (package-license perl))))

(define-public perl-class-method-modifiers
  (package
    (name "perl-class-method-modifiers")
    (version "2.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Class-Method-Modifiers-" version ".tar.gz"))
       (sha256
        (base32 "0qzx83mgd71hlc2m1kpw15dqsjzjq7b2cj3sdgg45a0q23vhfn5b"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-fatal perl-test-needs))
    (home-page "https://metacpan.org/release/Class-Method-Modifiers")
    (synopsis "Moose-like method modifiers")
    (description "Class::Method::Modifiers provides three modifiers:
@code{before}, @code{around}, and @code{after}.  @code{before} and @code{after}
are run just before and after the method they modify, but can not really affect
that original method.  @code{around} is run in place of the original method,
with a hook to easily call that original method.")
    (license (package-license perl))))

(define-public perl-class-mix
  (package
    (name "perl-class-mix")
    (version "0.006")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/Z/ZE/ZEFRAM/Class-Mix-"
            version ".tar.gz"))
      (sha256
       (base32
        "02vwzzqn1s24g525arbrjh9s9j0y1inp3wbr972gh51ri51zciw7"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build perl-test-pod perl-test-pod-coverage))
    (propagated-inputs
     (list perl-params-classify))
    (home-page "https://metacpan.org/release/Class-Mix")
    (synopsis "Dynamic class mixing")
    (description "The @code{mix_class} function provided by this
module dynamically generates anonymous classes with specified
inheritance.  This is useful where an incomplete class requires use of
a mixin in order to become instantiable.")
    (license license:perl-license)))

(define-public perl-class-singleton
  (package
    (name "perl-class-singleton")
    (version "1.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SH/SHAY/"
                           "Class-Singleton-" version ".tar.gz"))
       (sha256
        (base32
         "1942j9g0b4c88nvs3jghh3y31mlhbpwrx35xdcb2jaaiv7q17fi7"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Class-Singleton")
    (synopsis "Implementation of a singleton class for Perl")
    (description "This module implements a Singleton class from which other
classes can be derived.  By itself, the Class::Singleton module does very
little other than manage the instantiation of a single object.")
    (license (package-license perl))))

(define-public perl-class-tiny
  (package
    (name "perl-class-tiny")
    (version "1.008")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DA/DAGOLDEN/"
                           "Class-Tiny-" version ".tar.gz"))
       (sha256
        (base32
         "05anh4hn8va46xwbdx7rqxnhb8i1lingb614lywzr89gj5iql1gf"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Class-Tiny")
    (synopsis "Minimalist class construction")
    (description "This module offers a minimalist class construction kit.  It
uses no non-core modules for any recent Perl.")
    (license license:asl2.0)))

(define-public perl-class-unload
  (package
    (name "perl-class-unload")
    (version "0.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/I/IL/ILMARI/"
                           "Class-Unload-" version ".tar.gz"))
       (sha256
        (base32 "0pqa98z3ij6a3v9wkmvc8b410kv30y0xxqf0i6if3lp4lx3rgqjj"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-requires))
    (propagated-inputs
     (list perl-class-inspector))
    (home-page "https://metacpan.org/release/Class-Unload")
    (synopsis "Unload a class")
    (description "Class:Unload unloads a given class by clearing out its
symbol table and removing it from %INC.")
    (license (package-license perl))))

(define-public perl-class-xsaccessor
  (package
    (name "perl-class-xsaccessor")
    (version "1.19")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SM/SMUELLER/"
                           "Class-XSAccessor-" version ".tar.gz"))
       (sha256
        (base32
         "1wm6013il899jnm0vn50a7iv9v6r4nqywbqzj0csyf8jbwwnpicr"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Class-XSAccessor")
    (synopsis "Generate fast XS accessors without runtime compilation")
    (description "Class::XSAccessor implements fast read, write, and
read/write accessors in XS.  Additionally, it can provide predicates such as
\"has_foo()\" for testing whether the attribute \"foo\" is defined in the
object.  It only works with objects that are implemented as ordinary hashes.
Class::XSAccessor::Array implements the same interface for objects that use
arrays for their internal representation.")
    (license (package-license perl))))

(define-public perl-clone
  (package
    (name "perl-clone")
    (version "0.43")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/A/AT/ATOOMIC/"
                                  "Clone-" version ".tar.gz"))
              (sha256
               (base32
                "1npf5s4b90ds6lv8gn76b2w4bdh0z5ni5zk4skgc2db5d12560lr"))))
    (build-system perl-build-system)
    (synopsis "Recursively copy Perl datatypes")
    (description
     "This module provides a clone() method which makes recursive copies of
nested hash, array, scalar and reference types, including tied variables and
objects.")
    (home-page "https://metacpan.org/release/Clone")
    (license (package-license perl))))

(define-public perl-clone-choose
  (package
    (name "perl-clone-choose")
    (version "0.010")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HE/HERMES/"
                           "Clone-Choose-" version ".tar.gz"))
       (sha256
        (base32
         "0cin2bjn5z8xhm9v4j7pwlkx88jnvz8al0njdjwyvs6fb0glh8sn"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-clone perl-clone-pp perl-test-without-module))
    (propagated-inputs
     (list perl-module-runtime))
    (home-page "https://metacpan.org/release/Clone-Choose")
    (synopsis "Choose appropriate Perl @code{clone} utility")
    (description "This @code{Clone::Choose} module checks several different
modules which provide a @code{clone()} function and selects an appropriate
one.")
    (license license:perl-license)))

(define-public perl-clone-pp
  (package
    (name "perl-clone-pp")
    (version "1.08")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/N/NE/NEILB/Clone-PP-"
                           version ".tar.gz"))
       (sha256
        (base32 "0y7m25fksiavzg4xj4cm9zkz8rmnk4iqy7lm01m4nmyqlna3082p"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Clone-PP")
    (synopsis "Recursively copy Perl datatypes")
    (description "This module provides a general-purpose @code{clone} function
to make deep copies of Perl data structures.  It calls itself recursively to
copy nested hash, array, scalar and reference types, including tied variables
and objects.")
    (license (package-license perl))))

(define-public perl-common-sense
  (package
    (name "perl-common-sense")
    (version "3.75")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/ML/MLEHMANN/"
                           "common-sense-" version ".tar.gz"))
       (sha256
        (base32
         "0zhfp8f0czg69ycwn7r6ayg6idm5kyh2ai06g5s6s07kli61qsm8"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/common-sense")
    (synopsis "Sane defaults for Perl programs")
    (description "This module implements some sane defaults for Perl programs,
as defined by two typical specimens of Perl coders.")
    (license (package-license perl))))

(define-public perl-conf-libconfig
  (package
    (name "perl-conf-libconfig")
    (version "0.101")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/C/CN/CNANGEL/"
                           "Conf-Libconfig-" version ".tar.gz"))
       (sha256
        (base32 "11dd3kb0k45gqahnnwz50x3b4b25c5jgykkwgf74rcyr0dsy0n5a"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-extutils-pkgconfig perl-test-deep perl-test-exception
           perl-test-warn))
    (inputs
     (list libconfig))
    (home-page "https://metacpan.org/release/Conf-Libconfig")
    (synopsis "Perl extension for libconfig")
    (description
     "Conf::Libconfig is a Perl interface to the libconfig configuration file
library.  It support scalar, array, and hash data structures just like its C/C++
counterpart.  It reduces the effort required to implement a configuration file
parser in your Perl programme and allows sharing configuration files between
languages.")
    (license license:bsd-3)))

(define-public perl-config-grammar
  (package
    (name "perl-config-grammar")
    (version "1.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DS/DSCHWEI/"
                           "Config-Grammar-" version ".tar.gz"))
       (sha256
        (base32 "1qynf5bk6mnk90nggm3z8rdz2535kmqg46s0vj93pi68r6ia7cx8"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Config-Grammar")
    (synopsis "Grammar-based config parser")
    (description
     "Config::Grammar is a module to parse configuration files.  The
configuration may consist of multiple-level sections with assignments and
tabular data.")
    (license (package-license perl))))

(define-public perl-config-any
  (package
    (name "perl-config-any")
    (version "0.32")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HA/HAARG/"
                           "Config-Any-" version ".tar.gz"))
       (sha256
        (base32
         "0l31sg7dwh4dwwnql42hp7arkhcm15bhsgfg4i6xvbjzy9f2mnk8"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-module-pluggable))
    (home-page "https://metacpan.org/release/Config-Any")
    (synopsis "Load configuration from different file formats")
    (description "Config::Any provides a facility for Perl applications and
libraries to load configuration data from multiple different file formats.  It
supports XML, YAML, JSON, Apache-style configuration, and Perl code.")
    (license (package-license perl))))

(define-public perl-config-inifiles
  (package
    (name "perl-config-inifiles")
    (version "3.000002")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://cpan.metacpan.org/authors/id/S/SH/SHLOMIF/"
                           "Config-IniFiles-" version ".tar.gz"))
       (sha256
        (base32 "02dsz3inh5jwgaxmbcz8qxwgin8mkhm6vj9jyzfmm3dr5pnxcbnr"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-module-build perl-io-stringy))
    (home-page "https://metacpan.org/pod/Config::IniFiles")
    (synopsis "Package for configuration files outside your Perl script")
    (description "This package provides a way to have readable configuration
files outside your Perl script.  Configurations can be imported, sections
can be grouped, and settings can be accessed from a tied hash.")
    (license (package-license perl))))

(define-public perl-config-autoconf
  (package
    (name "perl-config-autoconf")
    (version "0.317")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RE/REHSACK/"
                           "Config-AutoConf-" version ".tar.gz"))
       (sha256
        (base32
         "1qcwib4yaml5z2283qy5khjcydyibklsnk8zrk9wzdzc5wnv5r01"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-capture-tiny))
    (home-page "https://metacpan.org/release/Config-AutoConf")
    (synopsis "Module to implement some AutoConf macros in Perl")
    (description "Config::AutoConf is intended to provide the same
opportunities to Perl developers as GNU Autoconf does for Shell developers.")
    (license (package-license perl))))

(define-public perl-config-general
  (package
    (name "perl-config-general")
    (version "2.63")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/T/TL/TLINDEN/"
                           "Config-General-" version ".tar.gz"))
       (sha256
        (base32 "1bbg3wp0xcpj04cmm86j1x0j5968jqi5s2c87qs7dgmap1vzk6qa"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Config-General")
    (synopsis "Generic Config Module")
    (description "This module opens a config file and parses its contents for
you.  The format of config files supported by Config::General is inspired by
the well known Apache config format and is 100% compatible with Apache
configs, but you can also just use simple name/value pairs in your config
files.  In addition to the capabilities of an Apache config file it supports
some enhancements such as here-documents, C-style comments, and multiline
options.")
    (license (package-license perl))))

(define-public perl-config-gitlike
  (package
    (name "perl-config-gitlike")
    (version "1.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/A/AL/ALEXMV/Config-GitLike-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0kp57na9mk6yni693h2fwap6l1ndbcj97l4860r9vkzx2jw0fjk7"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-exception))
    (propagated-inputs
     (list perl-moo perl-moox-types-mooselike))
    (home-page "https://metacpan.org/release/Config-GitLike")
    (synopsis "Parse Git style configuration files")
    (description
     "This module handles parsing, modifying and creating configuration files
of the style used by the Git version control system.")
    (license license:perl-license)))

(define-public perl-config-ini
  (package
    (name "perl-config-ini")
    (version "0.025")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/R/RJ/RJBS/Config-INI-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0clphq6a17chvb663fvjnxqvyvh26g03x0fl4bg9vy4ibdnzg2v2"))))
    (build-system perl-build-system)
    (inputs
     (list perl-mixin-linewise perl-perlio-utf8_strict perl-sub-exporter))
    (home-page "https://metacpan.org/release/Config-INI")
    (synopsis "Simple .ini-file format reader and writer")
    (description "@code{Config::INI} is a module that facilates the reading
and writing of @code{.ini}-style configuration files.")
    (license (package-license perl))))

(define-public perl-config-tiny
  (package
    (name "perl-config-tiny")
    (version "2.28")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/R/RS/RSAVAGE/Config-Tiny-"
                    version ".tgz"))
              (sha256
               (base32
                "000mw17nb7aj341s0afqimxd53w5y0c4yk61pihqzm191lx89pqj"))))
    (build-system perl-build-system)
    (native-inputs (list perl-test-pod))
    (home-page "https://metacpan.org/release/Config-Tiny")
    (synopsis "Read/Write .ini style files with as little code as possible")
    (description
     "@code{Config::Tiny} is a Perl class to read and write .ini
style configuration files with as little code as possible, reducing load time
and memory overhead.

This module is primarily for reading human written files, and anything we write
shouldn't need to have documentation/comments.  If you need something with more
power move up to @code{Config::Simple}, @code{Config::General} or one of the
many other @code{Config::*} modules.")
    (license license:perl-license)))

(define-public perl-const-fast
  (package
    (name "perl-const-fast")
    (version "0.014")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/L/LE/LEONT/"
             "Const-Fast-" version ".tar.gz"))
       (sha256
        (base32
         "1nwlldgrx86yn7y6a53cqgvzm2ircsvxg1addahlcy6510x9a1gq"))))
    (inputs
     (list perl-module-build-tiny perl-test-fatal))
    ;; Needed for tests.
    (native-inputs
     (list perl-sub-exporter-progressive))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Const-Fast")
    (synopsis "Facility for creating read-only scalars, arrays, and hashes")
    (description "This package provides procedures to create read-only
scalars, arrays, and hashes.")
    (license (package-license perl))))

(define-public perl-context-preserve
  (package
    (name "perl-context-preserve")
    (version "0.03")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Context-Preserve-" version ".tar.gz"))
       (sha256
        (base32
         "07zxgmb11bn4zj3w9g1zwbb9iv4jyk5q7hc0nv59knvv5i64m489"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-exception perl-test-simple))
    (home-page "https://metacpan.org/release/Context-Preserve")
    (synopsis "Preserve context during subroutine call")
    (description "This module runs code after a subroutine call, preserving
the context the subroutine would have seen if it were the last statement in
the caller.")
    (license (package-license perl))))

(define-public perl-convert-binhex
  (package
    (name "perl-convert-binhex")
    (version "1.125")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/S/ST/STEPHEN/Convert-BinHex-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "15v3489k179cx0fz3lix79ssjid0nhhpf6c33swpxga6pss92dai"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-file-slurp perl-test-most))
    (home-page
     "https://metacpan.org/release/Convert-BinHex")
    (synopsis "Extract data from Macintosh BinHex files")
    (description
     "BinHex is a format for transporting files safely through electronic
mail, as short-lined, 7-bit, semi-compressed data streams.  This module
provides a means of converting those data streams back into into binary
data.")
    (license license:perl-license)))

(define-public perl-cpan-changes
  (package
    (name "perl-cpan-changes")
    (version "0.400002")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/H/HA/HAARG/CPAN-Changes-"
             version ".tar.gz"))
       (sha256
        (base32
         "13dy78amkhwg278sv5im0ylyskhxpfivyl2aissqqih71nlxxvh1"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/CPAN-Changes")
    (synopsis "Read and write @file{Changes} files")
    (description
     "@code{CPAN::Changes} helps users programmatically read and write
@file{Changes} files that conform to a common specification.")
    (license license:perl-license)))

(define-public perl-cpan-distnameinfo
  (package
    (name "perl-cpan-distnameinfo")
    (version "0.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/G/GB/GBARR/CPAN-DistnameInfo-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0d94kx596w7k328cvq4y96z1gz12hdhn3z1mklkbrb7fyzlzn91g"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/CPAN-DistnameInfo")
    (synopsis "Extract the name and version from a distribution filename")
    (description
     "@code{CPAN::DistnameInfo} uses heuristics to extract the distribution
name and version from filenames.")
    (license license:perl-license)))

(define-public perl-cpan-meta-check
  (package
    (name "perl-cpan-meta-check")
    (version "0.014")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/L/LE/LEONT/"
                           "CPAN-Meta-Check-" version ".tar.gz"))
       (sha256
        (base32
         "07rmdbz1rbnb7w33vswn1wixlyh947sqr93xrvcph1hwzhmmg818"))))
    (build-system perl-build-system)
    (native-inputs (list perl-test-deep))
    (propagated-inputs (list perl-cpan-meta))
    (home-page "https://metacpan.org/release/CPAN-Meta-Check")
    (synopsis "Verify requirements in a CPAN::Meta object")
    (description "This module verifies if requirements described in a
CPAN::Meta object are present.")
    (license (package-license perl))))

(define-public perl-cpanel-json-xs
  (package
    (name "perl-cpanel-json-xs")
    (version "4.30")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RU/RURBAN/"
                           "Cpanel-JSON-XS-" version ".tar.gz"))
       (sha256
        (base32 "1d5xwk3j3pvc2s439vjrnhwcx44wkskda9mrwv3ix2c6pp7slpsn"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-common-sense))
    (home-page "https://metacpan.org/release/Cpanel-JSON-XS")
    (synopsis "JSON::XS for Cpanel")
    (description "This module converts Perl data structures to JSON and vice
versa.")
    (license (package-license perl))))

(define-public perl-critic
  (package
    (name "perl-critic")
    (version "1.140")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/P/PE/PETDANCE/Perl-Critic-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1nzxpn71mrpp85yxrxlraj52q2skvf9ja887ls11d57h6smg1vmz"))))
    (build-system perl-build-system)
    (native-inputs (list perl-module-build perl-test-deep))
    (propagated-inputs (list perltidy
                             perl-exception-class
                             perl-io-string
                             perl-ppi
                             perl-ppix-regexp
                             perl-b-keywords
                             perl-config-tiny
                             perl-padwalker
                             perl-test-memory-cycle
                             perl-file-which
                             perl-list-moreutils
                             perl-module-pluggable
                             perl-pod-parser
                             perl-pod-spell
                             perl-ppix-quotelike
                             perl-ppix-utilities
                             perl-readonly
                             perl-string-format
                             perl-task-weaken))
    (home-page "https://metacpan.org/release/Perl-Critic")
    (synopsis "Critique Perl source code for best-practices")
    (description
     "@code{perlcritic} is a Perl source code analyzer.  It is the
executable front-end to the @code{Perl::Critic} engine, which attempts to
identify awkward, hard to read, error-prone, or unconventional constructs in
your code.  Most of the rules are based on Damian Conway's book \"Perl Best
Practices\".  However, @code{perlcritic} is not limited to enforcing PBP, and it
will even support rules that contradict Conway.  All rules can easily be
configured or disabled to your liking.")
    (license license:perl-license)))

(define-public perl-crypt-cbc
  (package
    (name "perl-crypt-cbc")
    (version "2.33")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/L/LD/LDS/Crypt-CBC-"
            version ".tar.gz"))
      (sha256
       (base32
        "0ig698lmpjz7fslnznxm0609lvlnvf4f3s370082nzycnqhxww3a"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-crypt-rijndael))
    (home-page "https://metacpan.org/release/Crypt-CBC")
    (synopsis "Encrypt Data with Cipher Block Chaining Mode")
    (description "@code{Crypt::CBC} is a Perl-only implementation of
the cryptographic Cipher Block Chaining (CBC) mode.  In combination
with a block cipher such as @code{Crypt::Rijndael} you can encrypt and
decrypt messages of arbitrarily long length.  The encrypted messages
are compatible with the encryption format used by SSLeay.")
    (license license:perl-license)))

(define-public perl-crypt-des
  (package
    (name "perl-crypt-des")
    (version "2.07")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/D/DP/DPARIS/Crypt-DES-"
            version ".tar.gz"))
      (sha256
       (base32
        "1rypxlhpd1jc0c327aghgl9y6ls47drmpvn0a40b4k3vhfsypc9d"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-crypt-cbc))
    (home-page "https://metacpan.org/release/Crypt-DES")
    (synopsis "DES encryption module")
    (description "@code{Crypt::DES} is an XS-based implementation of
the DES cryptography algorithm.  The module implements the
@code{Crypt::CBC} interface which has blocksize, keysize, encrypt and
decrypt functions.")
    (license license:bsd-3)))

(define-public perl-crypt-eksblowfish
  (package
    (name "perl-crypt-eksblowfish")
    (version "0.009")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/Z/ZE/ZEFRAM/Crypt-Eksblowfish-"
            version ".tar.gz"))
      (sha256
       (base32
        "0k01aw3qb2s4m1w4dqsc9cycyry1zg3wabdym4vp4421b1ni5irw"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build perl-test-pod perl-test-pod-coverage))
    (propagated-inputs
     (list perl-class-mix))
    (home-page "https://metacpan.org/release/Crypt-Eksblowfish")
    (synopsis "The Eksblowfish block cipher")
    (description "Eksblowfish is a variant of the Blowfish cipher,
modified to make the key setup very expensive.  This doesn't make it
significantly cryptographically stronger but is intended to hinder
brute-force attacks.  Eksblowfish is a parameterised (family-keyed)
cipher.  It takes a cost parameter that controls how expensive the key
scheduling is.  It also takes a family key, known as the \"salt\".
Cost and salt parameters together define a cipher family.  Within each
family, the key determines the encryption function.  This distribution
also includes an implementation of @code{bcrypt}, the Unix crypt()
password hashing algorithm based on Eksblowfish.")
    (license license:perl-license)))

(define-public perl-crypt-mysql
  (package
    (name "perl-crypt-mysql")
    (version "0.04")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/I/IK/IKEBE/Crypt-MySQL-"
            version ".tar.gz"))
      (sha256
       (base32
        "1qyx6ha13r0rh80ldv5wy2bq2pa74igwh8817xlapsfgxymdzswk"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build perl-dbd-mysql))
    (propagated-inputs
     (list perl-digest-sha1))
    (home-page "https://metacpan.org/release/Crypt-MySQL")
    (synopsis "Emulate the MySQL PASSWORD() function")
    (description "@code{Crypt::MySQL} emulates the MySQL PASSWORD()
function.  The module does not depend on an interface to the MySQL
database server.  This enables the comparison of encrypted passwords
without the need for a real MySQL environment.")
    (license license:perl-license)))

(define-public perl-crypt-passwdmd5
  (package
    (name "perl-crypt-passwdmd5")
    (version "1.40")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/R/RS/RSAVAGE/Crypt-PasswdMD5-"
            version ".tgz"))
      (sha256
       (base32
        "0j0r74f18nk63phddzqbf7wqma2ci4p4bxvrwrxsy0aklbp6lzdp"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build))
    (home-page "https://metacpan.org/release/Crypt-PasswdMD5")
    (synopsis "Interoperable MD5-based crypt() functions")
    (description "@code{Crypt::PasswdMD5} provides various
crypt()-compatible interfaces to the MD5-based crypt() function found
in various *nixes.  It is based on the implementation found on FreeBSD
2.2.[56]-RELEASE.")
    (license license:perl-license)))

(define-public perl-crypt-randpasswd
  (package
    (name "perl-crypt-randpasswd")
    (version "0.06")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/N/NE/NEILB/"
                           "Crypt-RandPasswd-" version ".tar.gz"))
       (sha256
        (base32
         "0ca8544371wp4vvqsa19lnhl02hczpkbwkgsgm65ziwwim3r1gdi"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Crypt-RandPasswd")
    (synopsis "Random password generator")
    (description "Crypt::RandPasswd provides three functions that can be used
to generate random passwords, constructed from words, letters, or characters.
This code is a Perl implementation of the Automated Password Generator
standard, like the program described in \"A Random Word Generator For
Pronounceable Passwords\".  This code is a re-engineering of the program
contained in Appendix A of FIPS Publication 181, \"Standard for Automated
Password Generator\".")
    (license (package-license perl))))

(define-public perl-crypt-rijndael
  (package
    (name "perl-crypt-rijndael")
    (version "1.16")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/L/LE/LEONT/Crypt-Rijndael-"
            version ".tar.gz"))
      (sha256
       (base32 "0h2dr1bd15y0sipxsdh1k4hx5bccywn15haj0xpjmf0471g0hh35"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Crypt-Rijndael")
    (synopsis "Crypt::CBC compliant Rijndael encryption module")
    (description "This module implements the Rijndael cipher which has
been selected as the Advanced Encryption Standard.  The keysize for
Rijndael is 32 bytes.  The blocksize is 16 bytes (128 bits).  The
supported encryption modes are:

@itemize
@item @code{MODE_CBC}---Cipher Block Chaining
@item @code{MODE_CFB}---Cipher feedback
@item @code{MODE_CTR}---Counter mode
@item @code{MODE_ECB}---Electronic cookbook mode
@item @code{MODE_OFB}---Output feedback
@end itemize")
    (license license:gpl3)))

(define-public perl-crypt-rc4
  (package
    (name "perl-crypt-rc4")
    (version "2.02")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/S/SI/SIFUKURT/Crypt-RC4-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1sp099cws0q225h6j4y68hmfd1lnv5877gihjs40f8n2ddf45i2y"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release//Crypt-RC4")
    (synopsis "Perl implementation of the RC4 encryption algorithm")
    (description "A pure Perl implementation of the RC4 algorithm.")
    (license (package-license perl))))

(define-public perl-crypt-unixcrypt_xs
  (package
    (name "perl-crypt-unixcrypt_xs")
    (version "0.11")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/B/BO/BORISZ/Crypt-UnixCrypt_XS-"
            version ".tar.gz"))
      (sha256
       (base32
        "1ajg3x6kwxy4x9p3nw1j36qjxpjvdpi9wkca5gfd86y9q8939sv2"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Crypt-UnixCrypt_XS")
    (synopsis "XS interface for a portable traditional crypt function")
    (description "@code{Crypt::UnixCrypt_XS} implements the DES-based
Unix @code{crypt} function.  For those who need to construct
non-standard variants of @code{crypt}, the various building blocks
used in @code{crypt} are also supplied separately.")
    ;; Files in the 'fcrypt' directory are covered by a BSD licence.
    (license (list license:perl-license license:bsd-3))))

(define-public perl-cryptx
  (package
  (name "perl-cryptx")
  (version "0.078")
  (source
   (origin
     (method url-fetch)
     (uri (string-append "mirror://cpan/authors/id/M/MI/MIK/CryptX-"
                         version ".tar.gz"))
     (sha256
      (base32 "1gdw33k8h7izjfb4zy9j1qfq4ffbqzpvhcf9ncy79mgp8890n5lk"))))
  (build-system perl-build-system)
  (home-page "https://metacpan.org/release/CryptX")
  (synopsis "Self-contained cryptographic toolkit based on LibTomCrypt")
  (description
   "These self-contained Perl modules provide cryptography based on the
LibTomCrypt library.")
  (license license:perl-license)))

(define-public perl-cwd-guard
  (package
    (name "perl-cwd-guard")
    (version "0.05")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/K/KA/KAZEBURO/"
                                  "Cwd-Guard-" version ".tar.gz"))
              (sha256
               (base32
                "0xwf4rmii55k3lp19mpbh00mbgby7rxdk2lk84148bjhp6i7rz3s"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build perl-test-requires))
    (home-page "https://metacpan.org/release/Cwd-Guard")
    (synopsis "Temporarily change working directory")
    (description
     "@code{Cwd::Guard} changes the current directory using a limited scope.
It returns to the previous working directory when the object is destroyed.")
    (license (package-license perl))))

(define-public perl-czplib
  (package
    (name "perl-czplib")
    (version "1.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/czplib/czplib.v"
                           version ".tgz"))
       (sha256
        (base32
         "12kln8l5h406r1ss6zbazgcshmys9nvabkrhvk2zwrrgl1saq1kf"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove .git directory
           (delete-file-recursively ".git")
           #t))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace
          'install
          (lambda* (#:key outputs #:allow-other-keys)
            (copy-recursively "."
                              (string-append (assoc-ref outputs "out")
                                             "/lib/perl5/site_perl/"
                                             ,(package-version perl)))
            #t)))))
    (home-page "https://sourceforge.net/projects/czplib/")
    (synopsis "Library for genomic analysis")
    (description "Chaolin Zhang's Perl Library (czplib) contains assorted
functions and data structures for processing and analysing genomic and
bioinformatics data.")
    (license license:gpl3+)))

(define-public perl-data
  (package
    (name "perl-data")
    (version "0.002009")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MA/MATTP/"
                           "Data-Perl-" version ".tar.gz"))
       (sha256
        (base32
         "12vgqdjbfqf2qfg21x22wg88xnwxfbw2ki3qzcb3nb0chwjj4axn"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-deep perl-test-output perl-test-fatal))
    (inputs
     (list perl-class-method-modifiers perl-list-moreutils
           perl-module-runtime perl-role-tiny perl-strictures))
    (home-page "https://metacpan.org/release/Data-Perl")
    (synopsis "Base classes wrapping fundamental Perl data types")
    (description "Collection of classes that wrap fundamental data types that
exist in Perl.  These classes and methods as they exist today are an attempt
to mirror functionality provided by Moose's Native Traits.  One important
thing to note is all classes currently do no validation on constructor
input.")
    (license (package-license perl))))

(define-public perl-data-compare
  (package
    (name "perl-data-compare")
    (version "1.27")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DC/DCANTRELL/"
                           "Data-Compare-" version ".tar.gz"))
       (sha256
        (base32 "1gg8rqbv3x6a1lrpabv6vnlab53zxmpwz2ygad9fcx4gygqj12l1"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-clone perl-file-find-rule))
    (home-page "https://metacpan.org/release/Data-Compare")
    (synopsis "Compare Perl data structures")
    (description "This module compares arbitrary data structures to see if
they are copies of each other.")
    (license (package-license perl))))

(define-public perl-data-entropy
  (package
    (name "perl-data-entropy")
    (version "0.007")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/Z/ZE/ZEFRAM/Data-Entropy-"
            version ".tar.gz"))
      (sha256
       (base32
        "1r176jjzir2zg5kidx85f7vzi6jsw7ci9vd4kvbr9183lfhw8496"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build perl-test-pod perl-test-pod-coverage))
    (propagated-inputs
     (list perl-crypt-rijndael perl-data-float perl-http-lite
           perl-params-classify))
    (home-page "https://metacpan.org/release/Data-Entropy")
    (synopsis "Entropy (randomness) management")
    (description "@code{Data::Entropy} provides modules relating to
the generation and use of entropy.  The Data::Entropy::Source class
manages the entropy coming from a particular source.  This class acts
as a layer over a raw entropy source, which may be a normal I/O handle
or a special-purpose class.  The Data::Entropy::RawSource::* classes
provide fundamental sources of entropy.  The sources specially
supported are an OS-supplied entropy collector, downloads from servers
on the Internet, and cryptographic fake entropy.  The
Data::Entropy::Algorithms module contains a collection of fundamental
algorithms that use entropy.  There are random number generators and
functions to shuffle arrays.")
    (license license:perl-license)))

(define-public perl-data-integer
  (package
    (name "perl-data-integer")
    (version "0.006")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/Z/ZE/ZEFRAM/Data-Integer-"
            version ".tar.gz"))
      (sha256
       (base32
        "0m53zxhx9sn49yqh7azlpyy9m65g54v8cd2ha98y77337gg7xdv3"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build perl-test-pod perl-test-pod-coverage))
    (home-page "https://metacpan.org/release/Data-Integer")
    (synopsis "Details of the native integer data type")
    (description "This module is about the native integer numerical
data type.  A native integer is one of the types of datum that can
appear in the numeric part of a Perl scalar.  This module supplies
constants describing the native integer type.  Both signed and
unsigned representations are handled.")
    (license license:perl-license)))

(define-public perl-data-uniqid
  (package
    (name "perl-data-uniqid")
    (version "0.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MW/MWX/Data-Uniqid-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1jsc6acmv97pzsvx1fqywz4qvxxpp7kwmb78ygyqpsczkfj9p4dn"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Data-Uniqid")
    (synopsis "Perl extension for generating unique identifiers")
    (description "@code{Data::Uniqid} provides three simple routines for
generating unique ids.  These ids are coded with a Base62 system to make them
short and handy (e.g. to use it as part of a URL).")
    (license (package-license perl))))

(define-public perl-data-uuid
  (package
    (name "perl-data-uuid")
    (version "1.226")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/R/RJ/RJBS/"
                    "Data-UUID-" version ".tar.gz"))
              (sha256
               (base32
                "0lv4k4ibxwkw7zz9hw97s34za9nvjxb4kbmgmx5sj4fll3zmfg89"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Data-UUID")
    (synopsis "Universally Unique Identifiers generator")
    (description "@code{Data::UUID} provides a framework for generating
Universally Unique Identifiers (UUIDs), also known as Globally Unique
Identifiers (GUIDs).  A UUID is 128 bits long, and is guaranteed to be
different from all other UUIDs/GUIDs generated until 3400 CE.")
    (license (package-license perl))))

(define-public perl-data-dump
  (package
    (name "perl-data-dump")
    (version "1.23")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/G/GA/GAAS/"
                           "Data-Dump-" version ".tar.gz"))
       (sha256
        (base32
         "0r9ba52b7p8nnn6nw0ygm06lygi8g68piri78jmlqyrqy5gb0lxg"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Data-Dump")
    (synopsis "Pretty printing of data structures")
    (description "This module provide functions that takes a list of values as
their argument and produces a string as its result.  The string contains Perl
code that, when \"eval\"ed, produces a deep copy of the original arguments.")
    (license (package-license perl))))

(define-public perl-data-dumper
  (package
    (name "perl-data-dumper")
    (version "2.183")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/N/NW/NWCLARK/"
                           "Data-Dumper-" version ".tar.gz"))
       (sha256
        (base32
         "1lssmgag36w1lhrnli2gq3g55p0z3zx5x74dh4vipbkx1f4kc9z4"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Data-Dumper")
    (synopsis "Convert data structures to strings")
    (description "Given a list of scalars or reference variables,
@code{Data::Dumper} writes out their contents in Perl syntax.  The references
can also be objects.  The content of each variable is output in a single Perl
statement.  It handles self-referential structures correctly.")
    (license license:perl-license)))

(define-public perl-data-dumper-concise
  (package
    (name "perl-data-dumper-concise")
    (version "2.023")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Data-Dumper-Concise-" version ".tar.gz"))
       (sha256
        (base32
         "0lsqbl1mxhkj0qnjfa1jrvx8wwbyi81bgwfyj1si6cdg7h8jzhm6"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Data-Dumper-Concise")
    (synopsis "Concise data dumper")
    (description "Data::Dumper::Concise provides a dumper with Less
indentation and newlines plus sub deparsing.")
    (license (package-license perl))))

(define-public perl-data-float
  (package
    (name "perl-data-float")
    (version "0.013")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/Z/ZE/ZEFRAM/Data-Float-"
            version ".tar.gz"))
      (sha256
       (base32
        "12ji4yf3nc965rqqgfhr96w7irpm6n1g15nivfxvhc49hlym5cg2"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build perl-test-pod perl-test-pod-coverage))
    (home-page "https://metacpan.org/release/Data-Float")
    (synopsis "Details of the floating point data type")
    (description "@code{Data::Float} is about the native floating
point numerical data type.  A floating point number is one of the
types of datum that can appear in the numeric part of a Perl scalar.
This module supplies constants describing the native floating point
type, classification functions and functions to manipulate floating
point values at a low level.")
    (license license:perl-license)))

(define-public perl-data-optlist
  (package
    (name "perl-data-optlist")
    (version "0.112")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/R/RJ/RJBS/Data-OptList-"
             version ".tar.gz"))
       (sha256
        (base32
         "1arv203h6c4b3y5q49xzmn2cz21kn108kk3bwfd37mc8mv50rik2"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-sub-install perl-params-util))
    (home-page "https://metacpan.org/release/Data-OptList")
    (synopsis "Parse and validate simple name/value option pairs")
    (description
     "Data::OptList provides a simple syntax for name/value option pairs.")
    (license (package-license perl))))

(define-public perl-data-page
  (package
    (name "perl-data-page")
    (version "2.03")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Data-Page-" version ".tar.gz"))
       (sha256
        (base32 "12rxrr2b11qjk0c437cisw2kfqkafw1awcng09cv6yhzglb55yif"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build perl-test-exception))
    (propagated-inputs
     (list perl-class-accessor-chained))
    (home-page "https://metacpan.org/release/Data-Page")
    (synopsis "Help when paging through sets of results")
    (description "When searching through large amounts of data, it is often
the case that a result set is returned that is larger than we want to display
on one page.  This results in wanting to page through various pages of data.
The maths behind this is unfortunately fiddly, hence this module.")
    (license (package-license perl))))

(define-public perl-data-perl
  (package
    (name "perl-data-perl")
    (version "0.002009")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/M/MA/MATTP/Data-Perl-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "12vgqdjbfqf2qfg21x22wg88xnwxfbw2ki3qzcb3nb0chwjj4axn"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-deep perl-test-fatal perl-test-output))
    (inputs
     (list perl-class-method-modifiers perl-module-runtime perl-role-tiny
           perl-strictures))
    (propagated-inputs
     (list perl-list-moreutils))
    (home-page
     "https://metacpan.org/release/Data-Perl")
    (synopsis "Base classes wrapping fundamental Perl data types")
    (description
     "@code{Data::Perl} is a container class for the following classes:
@itemize
@item @code{Data::Perl::Collection::Hash}
@item @code{Data::Perl::Collection::Array}
@item @code{Data::Perl::String}
@item @code{Data::Perl::Number}
@item @code{Data::Perl::Counter}
@item @code{Data::Perl::Bool}
@item @code{Data::Perl::Code}
@end itemize")
    (license license:perl-license)))

(define-public perl-data-printer
  (package
    (name "perl-data-printer")
    (version "0.40")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/G/GA/GARU/Data-Printer-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0njjh8zp5afc4602jrnmg89icj7gfsil6i955ypcqxc2gl830sb0"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-clone-pp perl-file-homedir perl-package-stash
           perl-sort-naturally))
    (home-page "https://metacpan.org/release/Data-Printer")
    (synopsis "Colored pretty-print of Perl data structures and objects")
    (description "Display Perl variables and objects on screen, properly
formatted (to be inspected by a human).")
    (license (package-license perl))))

(define-public perl-data-record
  (package
    (name "perl-data-record")
    (version "0.02")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/O/OV/OVID/"
                           "Data-Record-" version ".tar.gz"))
       (sha256
        (base32
         "1gwyhjwg4lrnfsn8wb6r8msb4yh0y4wca4mz3z120xbnl9nycshx"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-exception perl-module-build))
    (propagated-inputs
     (list perl-sub-uplevel))
    (home-page "https://metacpan.org/release/Data-Record")
    (synopsis "Conditionally split data into records")
    (description "This Perl module allows you to split data into records by
not only specifying what you wish to split the data on, but also by specifying
an \"unless\" regular expression.  If the text in question matches the
\"unless\" regex, it will not be split there.  This allows us to do things
like split on newlines unless newlines are embedded in quotes.")
    (license (package-license perl))))

(define-public perl-data-section
  (package
    (name "perl-data-section")
    (version "0.200007")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/R/RJ/RJBS/Data-Section-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1pmlxca0a8sv2jjwvhwgqavq6iwys6kf457lby4anjp3f1dpx4yd"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-failwarnings))
    (propagated-inputs
     (list perl-mro-compat perl-sub-exporter))
    (home-page "https://metacpan.org/release/Data-Section")
    (synopsis "Read multiple hunks of data out of your DATA section")
    (description "This package provides a Perl library to read multiple hunks
of data out of your DATA section.")
    (license (package-license perl))))

(define-public perl-data-section-simple
  (package
    (name "perl-data-section-simple")
    (version "0.07")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MI/MIYAGAWA/"
                           "Data-Section-Simple-" version ".tar.gz"))
       (sha256
        (base32 "1jx9g5sxcw0i2zkm2z895k422i49kpx0idnnvvvs36lhvgzkac0b"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-requires))
    (home-page "https://metacpan.org/release/Data-Section-Simple")
    (synopsis "Read data from __DATA__")
    (description
     "Data::Section::Simple is a simple module to extract data from __DATA__
section of the file.")
    (license license:perl-license)))

(define-public perl-data-stag
  (package
    (name "perl-data-stag")
    (version "0.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/C/CM/CMUNGALL/"
                           "Data-Stag-" version ".tar.gz"))
       (sha256
        (base32
         "0ncf4l39ka23nb01jlm6rzxdb5pqbip01x0m38bnvf1gim825caa"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-io-string))
    (home-page "https://metacpan.org/release/Data-Stag")
    (synopsis "Structured tags datastructures")
    (description
     "This module is for manipulating data as hierarchical tag/value
pairs (Structured TAGs or Simple Tree AGgregates).  These datastructures can
be represented as nested arrays, which have the advantage of being native to
Perl.")
    (license (package-license perl))))

(define-public perl-data-stream-bulk
  (package
    (name "perl-data-stream-bulk")
    (version "0.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DO/DOY/"
                           "Data-Stream-Bulk-" version ".tar.gz"))
       (sha256
        (base32
         "05q9ygcv7r318j7daxz42rjr5b99j6whjmwjdih0axxrlqr89q06"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-requires))
    (propagated-inputs
     (list perl-moose perl-namespace-clean perl-path-class
           perl-sub-exporter))
    (home-page "https://metacpan.org/release/Data-Stream-Bulk")
    (synopsis "N at a time iteration API")
    (description "This module tries to find middle ground between one at a
time and all at once processing of data sets.  The purpose of this module is
to avoid the overhead of implementing an iterative api when this isn't
necessary, without breaking forward compatibility in case that becomes
necessary later on.")
    (license (package-license perl))))

(define-public perl-data-tumbler
  (package
    (name "perl-data-tumbler")
    (version "0.010")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RE/REHSACK/"
                           "Data-Tumbler-" version ".tar.gz"))
       (sha256
        (base32 "15pgvmf7mf9fxsg2l4l88xwvs41218d0bvawhlk15sx06qqp0kwb"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-most))
    (propagated-inputs
     (list perl-file-homedir))
    (home-page "https://metacpan.org/release/Data-Tumbler")
    (synopsis "Dynamic generation of nested combinations of variants")
    (description "Data::Tumbler - Dynamic generation of nested combinations of
variants.")
    (license (package-license perl))))

(define-public perl-data-visitor
  (package
    (name "perl-data-visitor")
    (version "0.30")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DO/DOY/"
                           "Data-Visitor-" version ".tar.gz"))
       (sha256
        (base32
         "0m7d1505af9z2hj5aw020grcmjjlvnkjpvjam457d7k5qfy4m8lf"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-requires))
    (propagated-inputs
     (list perl-class-load perl-moose perl-namespace-clean
           perl-task-weaken perl-tie-toobject))
    (home-page "https://metacpan.org/release/Data-Visitor")
    (synopsis "Visitor style traversal of Perl data structures")
    (description "This module is a simple visitor implementation for Perl
values.  It has a main dispatcher method, visit, which takes a single perl
value and then calls the methods appropriate for that value.  It can
recursively map (cloning as necessary) or just traverse most structures, with
support for per-object behavior, circular structures, visiting tied
structures, and all ref types (hashes, arrays, scalars, code, globs).")
    (license (package-license perl))))

(define-public perl-date-calc
  (package
    (name "perl-date-calc")
    (version "6.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/ST/STBEY/"
                           "Date-Calc-" version ".tar.gz"))
       (sha256
        (base32
         "1barz0jgdaan3jm7ciphs5n3ahwkl42imprs3y8c1dwpwyr3gqbw"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-bit-vector perl-carp-clan))
    (home-page "https://metacpan.org/release/Date-Calc")
    (synopsis "Gregorian calendar date calculations")
    (description "This package consists of a Perl module for date calculations
based on the Gregorian calendar, thereby complying with all relevant norms and
standards: ISO/R 2015-1971, DIN 1355 and, to some extent, ISO 8601 (where
applicable).")
    (license (package-license perl))))

(define-public perl-date-calc-xs
  (package
    (name "perl-date-calc-xs")
    (version "6.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/ST/STBEY/"
                           "Date-Calc-XS-" version ".tar.gz"))
       (sha256
        (base32
         "1cssi9rmd31cgaafgp4m70jqbm1mgh3aphxsxz1dwdz8h283n6jz"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-bit-vector perl-carp-clan perl-date-calc))
    (home-page "https://metacpan.org/release/Date-Calc-XS")
    (synopsis "XS wrapper for Date::Calc")
    (description "Date::Calc::XS is an XS wrapper and C library plug-in for
Date::Calc.")
    (license (list (package-license perl) license:lgpl2.0+))))

(define-public perl-date-manip
  (package
    (name "perl-date-manip")
    (version "6.85")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SB/SBECK/"
                           "Date-Manip-" version ".tar.gz"))
       (sha256
        (base32 "1p6clpx9r0kzpzr9d6gy4q6m0pw21lh7bnd9ir3qiidp8cwkjqhn"))))
    (build-system perl-build-system)
    (arguments
     ;; Tests would require tzdata for timezone information, but tzdata is in
     ;; (gnu packages base) which would create a circular dependency.  TODO:
     ;; Maybe put this package elsewhere so we can turn on tests.
     '(#:tests? #f))
    (home-page "https://metacpan.org/release/Date-Manip")
    (synopsis "Date manipulation routines")
    (description "Date::Manip is a series of modules for common date/time
operations, such as comparing two times, determining a date a given amount of
time from another, or parsing international times.")
    (license (package-license perl))))

(define-public perl-date-range
  (package
    (name "perl-date-range")
    (version "1.41")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/T/TM/TMTM/"
                           "Date-Range-" version ".tar.gz"))
       (sha256
        (base32 "1fa8v75pbplmkb3ff6k0hd1m80p9xgksf54xhw1ha70h5d4rg65z"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-date-simple))
    (home-page "https://metacpan.org/dist/Date-Range")
    (synopsis "Work with a range of dates")
    (description
     "@code{Date::Range} is a library to work with date ranges.  It can
be used to determine whether a given date is in a particular range, or what
the overlap between two ranges are.")
    (license license:gpl2+)))

(define-public perl-date-simple
  (package
    (name "perl-date-simple")
    (version "3.03")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/I/IZ/IZUT/"
                           "Date-Simple-" version ".tar.gz"))
       (sha256
        (base32
         "016x17r9wi6ffdc4idwirzd1sxqcb4lmq5fn2aiq25nf2iir5899"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Date-Simple")
    (synopsis "Simple date handling")
    (description "Dates are complex enough without times and timezones.  This
module may be used to create simple date objects.  It handles validation,
interval arithmetic, and day-of-week calculation.  It does not deal with
hours, minutes, seconds, and time zones.")
    ;; Can be used with either license.
    (license (list (package-license perl) license:gpl2+))))

(define-public perl-datetime
  (package
    (name "perl-datetime")
    (version "1.54")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/"
                           "DateTime-" version ".tar.gz"))
       (sha256
        (base32 "1rxjagwmkdlmksz1cbxwx2ad51pv5q7dri2djqkz44q7j1nxlbmi"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-cpan-meta-check perl-module-build perl-test-fatal
           perl-test-warnings))
    (propagated-inputs
     (list perl-datetime-locale perl-datetime-timezone perl-file-sharedir
           perl-params-validate perl-try-tiny))
    (home-page "https://metacpan.org/release/DateTime")
    (synopsis "Date and time object for Perl")
    (description "DateTime is a class for the representation of date/time
combinations.  It represents the Gregorian calendar, extended backwards in
time before its creation (in 1582).")
    (license license:artistic2.0)))

(define-public perl-datetime-calendar-julian
  (package
    (name "perl-datetime-calendar-julian")
    (version "0.102")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/W/WY/WYANT/"
                           "DateTime-Calendar-Julian-" version ".tar.gz"))
       (sha256
        (base32 "0j95dhma66spjyb04zi6rwy7l33hibnrx02mn0znd9m89aiq52s6"))))
    (build-system perl-build-system)
    ;; Only needed for tests
    (native-inputs
     (list perl-datetime))
    (home-page "https://metacpan.org/release/DateTime-Calendar-Julian")
    (synopsis "Dates in the Julian calendar")
    (description "This package is a companion module to @code{DateTime.pm}.
It implements the Julian calendar.  It supports everything that
@code{DateTime.pm} supports and more: about one day per century more, to be
precise.")
    (license (package-license perl))))

(define-public perl-datetime-set
  (package
    (name "perl-datetime-set")
    (version "0.3900")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/F/FG/FGLOCK/"
                           "DateTime-Set-" version ".tar.gz"))
       (sha256
        (base32
         "0ih9pi6myg5i26hjpmpzqn58s0yljl2qxdd6gzpy9zda4hwirx4l"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build))
    (propagated-inputs
     (list perl-datetime perl-params-validate perl-set-infinite))
    (home-page "https://metacpan.org/release/DateTime-Set")
    (synopsis "DateTime set objects")
    (description "The DateTime::Set module provides a date/time sets
implementation.  It allows, for example, the generation of groups of dates,
like \"every wednesday\", and then find all the dates matching that pattern,
within a time range.")
    (license (package-license perl))))

(define-public perl-datetime-event-ical
  (package
    (name "perl-datetime-event-ical")
    (version "0.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/F/FG/FGLOCK/"
                           "DateTime-Event-ICal-" version ".tar.gz"))
       (sha256
        (base32
         "1skmykxbrf98ldi72d5s1v6228gfdr5iy4y0gpl0xwswxy247njk"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-datetime perl-datetime-event-recurrence))
    (home-page "https://metacpan.org/release/DateTime-Event-ICal")
    (synopsis "DateTime rfc2445 recurrences")
    (description "This module provides convenience methods that let you easily
create DateTime::Set objects for RFC 2445 style recurrences.")
    (license (package-license perl))))

(define-public perl-datetime-event-recurrence
  (package
    (name "perl-datetime-event-recurrence")
    (version "0.19")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/F/FG/FGLOCK/"
                           "DateTime-Event-Recurrence-" version ".tar.gz"))
       (sha256
        (base32
         "19dms2vg9hvfx80p85m8gkn2ww0yxjrjn8qsr9k7f431lj4qfh7r"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-datetime perl-datetime-set))
    (home-page "https://metacpan.org/release/DateTime-Event-Recurrence")
    (synopsis "DateTime::Set extension for basic recurrences")
    (description "This module provides convenience methods that let you easily
create DateTime::Set objects for various recurrences, such as \"once a month\"
or \"every day\".  You can also create more complicated recurrences, such as
\"every Monday, Wednesday and Thursday at 10:00 AM and 2:00 PM\".")
    (license (package-license perl))))

(define-public perl-datetime-format-builder
  (package
    (name "perl-datetime-format-builder")
    (version "0.82")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/"
                           "DateTime-Format-Builder-" version ".tar.gz"))
       (sha256
        (base32
         "18qw5rn1qbji3iha8gmpgldbjv9gvn97j9d5cp57fb4r5frawgrq"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-class-factory-util perl-datetime
           perl-datetime-format-strptime perl-params-validate))
    (home-page "https://metacpan.org/release/DateTime-Format-Builder")
    (synopsis "Create DateTime parser classes and objects")
    (description "DateTime::Format::Builder creates DateTime parsers.  Many
string formats of dates and times are simple and just require a basic regular
expression to extract the relevant information.  Builder provides a simple way
to do this without writing reams of structural code.")
    (license license:artistic2.0)))

(define-public perl-datetime-format-flexible
  (package
    (name "perl-datetime-format-flexible")
    (version "0.32")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/T/TH/THINC/"
                           "DateTime-Format-Flexible-" version ".tar.gz"))
       (sha256
        (base32 "1vnq3a8bwhidcv3z9cvcmfiq2qa84hikr993ffr19fw7nbzbk9sh"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-exception perl-test-nowarnings perl-test-mocktime))
    (propagated-inputs
     (list perl-datetime perl-datetime-format-builder
           perl-datetime-timezone perl-list-moreutils perl-module-pluggable))
    (home-page "https://metacpan.org/release/DateTime-Format-Flexible")
    (synopsis "Parse date and time strings")
    (description "DateTime::Format::Flexible attempts to take any string you
give it and parse it into a DateTime object.")
    (license (package-license perl))))

(define-public perl-datetime-format-ical
  (package
    (name "perl-datetime-format-ical")
    (version "0.09")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/"
                           "DateTime-Format-ICal-" version ".tar.gz"))
       (sha256
        (base32
         "0cvwk7pigj7czsp81z35h7prxvylkrlk2l0kwvq0v72ykx9zc2cb"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build))
    (propagated-inputs
     (list perl-datetime perl-datetime-event-ical perl-datetime-set
           perl-datetime-timezone perl-params-validate))
    (home-page "https://metacpan.org/release/DateTime-Format-ICal")
    (synopsis "Parse and format iCal datetime and duration strings")
    (description "This module understands the ICal date/time and duration
formats, as defined in RFC 2445.  It can be used to parse these formats in
order to create the appropriate objects.")
    (license (package-license perl))))

(define-public perl-datetime-format-iso8601
  (package
    (name "perl-datetime-format-iso8601")
    (version "0.08")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/J/JH/JHOBLITT/DateTime-Format-ISO8601-"
            version ".tar.gz"))
      (sha256
       (base32
        "1syccqd5jlwms8v78ksnf68xijzl97jky5vbwhnyhxi5gvgfx8xk"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build))
    (propagated-inputs
     (list perl-datetime perl-datetime-format-builder perl-file-find-rule
           perl-test-distribution perl-test-pod))
    (home-page "https://metacpan.org/release/DateTime-Format-ISO8601")
    (synopsis "Parse ISO8601 date and time formats")
    (description "@code{DateTime::Format::ISO8601} is a DateTime
extension that parses almost all ISO8601 date and time formats.")
    (license license:perl-license)))

(define-public perl-datetime-format-natural
  (package
    (name "perl-datetime-format-natural")
    (version "1.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SC/SCHUBIGER/"
                           "DateTime-Format-Natural-" version ".tar.gz"))
       (sha256
        (base32 "0mqjsjyfymzp7lx7czx17bsdshzsh6l8r6hcadv81zvga326zprw"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build perl-module-util perl-test-mocktime))
    (propagated-inputs
     (list perl-boolean
           perl-clone
           perl-date-calc
           perl-date-calc-xs
           perl-datetime
           perl-datetime-timezone
           perl-list-moreutils
           perl-params-validate))
    (home-page "https://metacpan.org/release/DateTime-Format-Natural")
    (synopsis "Machine-readable date/time with natural parsing")
    (description "DateTime::Format::Natural takes a string with a human
readable date/time and creates a machine readable one by applying natural
parsing logic.")
    (license (package-license perl))))

(define-public perl-datetime-format-strptime
  (package
    (name "perl-datetime-format-strptime")
    (version "1.77")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/"
                           "DateTime-Format-Strptime-" version ".tar.gz"))
       (sha256
        (base32 "0jiy2yc9h9932ykb8x2l1j3ff8ms3p4426m947r5clygis1kr91g"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-datetime
           perl-datetime-locale
           perl-datetime-timezone
           perl-package-deprecationmanager
           perl-params-validate
           perl-sub-name
           perl-test-warnings))
    (home-page "https://metacpan.org/release/DateTime-Format-Strptime")
    (synopsis "Parse and format strp and strf time patterns")
    (description "This module implements most of `strptime(3)`, the POSIX
function that is the reverse of `strftime(3)`, for `DateTime`.  While
`strftime` takes a `DateTime` and a pattern and returns a string, `strptime`
takes a string and a pattern and returns the `DateTime` object associated.")
    (license license:artistic2.0)))

(define-public perl-datetime-locale
  (package
    (name "perl-datetime-locale")
    (version "1.23")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/"
                           "DateTime-Locale-" version ".tar.gz"))
       (sha256
        (base32
         "05f0jchminv5g2nrvsx5v1ihc5919fzzhh4f82dxi5ns8bkq2nis"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-file-sharedir
           perl-ipc-system-simple
           perl-test-file-sharedir-dist
           perl-test-warnings
           perl-test-requires
           perl-namespace-autoclean
           perl-file-sharedir-install
           perl-cpan-meta-check
           perl-module-build))
    (propagated-inputs
     (list perl-list-moreutils perl-params-validationcompiler))
    (home-page "https://metacpan.org/release/DateTime-Locale")
    (synopsis "Localization support for DateTime.pm")
    (description "The DateTime::Locale modules provide localization data for
the DateTime.pm class.")
    (license (package-license perl))))

(define-public perl-datetime-timezone
  (package
    (name "perl-datetime-timezone")
    (version "2.47")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/"
                           "DateTime-TimeZone-" version ".tar.gz"))
       (sha256
        (base32
         "1fgj3si94w87sy66p44mphsgj2cfrkqvdjn3bbz5bqmmvcw72qa1"))))
    (build-system perl-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-tzdata
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "lib/DateTime/TimeZone/Local/Unix.pm"
               (("our \\$ZoneinfoDir = '\\/usr\\/share\\/zoneinfo';")
                (string-append "our $ZoneinfoDir = '"
                               (assoc-ref inputs "tzdata") "/share/zoneinfo"
                               "';")))
             #t)))))
    (native-inputs
     (list perl-test-fatal perl-test-requires))
    (inputs
     (list tzdata))
    (propagated-inputs
     (list perl-class-singleton
           perl-list-allutils
           perl-module-runtime
           perl-namespace-autoclean
           perl-params-validationcompiler
           perl-try-tiny))
    (home-page "https://metacpan.org/release/DateTime-TimeZone")
    (synopsis "Time zone object for Perl")
    (description "This class is the base class for all time zone objects.  A
time zone is represented internally as a set of observances, each of which
describes the offset from GMT for a given time period.  Note that without the
DateTime module, this module does not do much.  It's primary interface is
through a DateTime object, and most users will not need to directly use
DateTime::TimeZone methods.")
    (license (package-license perl))))

(define-public perl-datetimex-easy
  (package
    (name "perl-datetimex-easy")
    (version "0.089")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RO/ROKR/"
                           "DateTimeX-Easy-" version ".tar.gz"))
       (sha256
        (base32
         "0ybs9175h4s39x8a23ap129cgqwmy6w7psa86194jq5cww1d5rhp"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-most))
    (propagated-inputs
     (list perl-datetime perl-datetime-format-flexible
           perl-datetime-format-ical perl-datetime-format-natural
           perl-timedate))
    (home-page "https://metacpan.org/release/DateTimeX-Easy")
    (synopsis "Parse date/time strings")
    (description "DateTimeX::Easy uses a variety of DateTime::Format packages
to create DateTime objects, with some custom tweaks to smooth out the rough
edges (mainly concerning timezone detection and selection).")
    (license (package-license perl))))

(define-public perl-datetime-format-mail
  (package
    (name "perl-datetime-format-mail")
    (version "0.403")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/B/BO/BOOK/"
                                  "DateTime-Format-Mail-" version ".tar.gz"))
              (sha256
               (base32
                "1c7wapbi9g9p2za52l3skhh31vg4da5kx2yfqzsqyf3p8iff7y4d"))))
    (build-system perl-build-system)
    (inputs
     (list perl-datetime perl-params-validate))
    (home-page "https://metacpan.org/release/DateTime-Format-Mail")
    (synopsis "Convert between DateTime and RFC2822/822 formats")
    (description "RFCs 2822 and 822 specify date formats to be used by email.
This module parses and emits such dates.")
    (license (package-license perl))))

(define-public perl-datetime-format-w3cdtf
  (package
    (name "perl-datetime-format-w3cdtf")
    (version "0.07")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/G/GW/GWILLIAMS/"
                                  "DateTime-Format-W3CDTF-" version ".tar.gz"))
              (sha256
               (base32
                "0s32lb1k80p3b3sb7w234zgxnrmadrwbcg41lhaal7dz3dk2p839"))))
    (build-system perl-build-system)
    (inputs
     (list perl-datetime))
    (native-inputs
     (list perl-test-pod perl-test-pod-coverage))
    (home-page "https://metacpan.org/release/DateTime-Format-W3CDTF")
    (synopsis "Parse and format W3CDTF datetime strings")
    (description
     "This module understands the W3CDTF date/time format, an ISO 8601 profile,
defined at https://www.w3.org/TR/NOTE-datetime.  This format is the native date
format of RSS 1.0.  It can be used to parse these formats in order to create
the appropriate objects.")
    (license (package-license perl))))

(define-public perl-devel-callchecker
  (package
    (name "perl-devel-callchecker")
    (version "0.008")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/Z/ZE/ZEFRAM/Devel-CallChecker-"
            version ".tar.gz"))
      (sha256
       (base32
        "1p0ij2k2i81zhl7064h9ghld1w5xy2zsbghkpdzm2hjryl5lwn2x"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build perl-test-pod perl-test-pod-coverage))
    (propagated-inputs
     (list perl-b-hooks-op-check perl-dynaloader-functions))
    (home-page "https://metacpan.org/release/Devel-CallChecker")
    (synopsis "Custom op checking attached to subroutines")
    (description "This module makes some new features of the Perl
5.14.0 C API available to XS modules running on older versions of
Perl.  The features are centred around the function
@code{cv_set_call_checker}, which allows XS code to attach a magical
annotation to a Perl subroutine, resulting in resolvable calls to that
subroutine being mutated at compile time by arbitrary C code.  This
module makes @code{cv_set_call_checker} and several supporting
functions available.")
    (license license:perl-license)))

(define-public perl-devel-caller
  (package
    (name "perl-devel-caller")
    (version "2.06")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RC/RCLAMP/"
                           "Devel-Caller-" version ".tar.gz"))
       (sha256
        (base32
         "1pxpimifzmnjnvf4icclx77myc15ahh0k56sj1djad1855mawwva"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-padwalker))
    (home-page "https://metacpan.org/release/Devel-Caller")
    (synopsis "Meatier version of caller")
    (description "Devel::Caller provides meatier version of caller.")
    (license (package-license perl))))

(define-public perl-devel-checkbin
  (package
    (name "perl-devel-checkbin")
    (version "0.04")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/T/TO/TOKUHIROM/"
                           "Devel-CheckBin-" version ".tar.gz"))
       (sha256
        (base32
         "1r735yzgvsxkj4m6ks34xva5m21cfzp9qiis2d4ivv99kjskszqm"))))
    (build-system perl-build-system)
    (native-inputs (list perl-module-build))
    (home-page "https://metacpan.org/release/Devel-CheckBin")
    (synopsis "Check that a command is available")
    (description "Devel::CheckBin is a perl module that checks whether a
particular command is available.")
    (license (package-license perl))))

(define-public perl-devel-checklib
  (package
    (name "perl-devel-checklib")
    (version "1.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MA/MATTN/Devel-CheckLib-"
             version ".tar.gz"))
       (sha256
        (base32 "15621qh5gaan1sgmk9y9svl70nm8viw17x5h1kf0zknkk8lmw77j"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-capture-tiny perl-mock-config))
    (home-page "https://metacpan.org/release/Devel-CheckLib")
    (synopsis "Check that a library is available")
    (description
     "@code{Devel::CheckLib} is a Perl module that checks whether a particular
C library and its headers are available.  You can also check for the presence of
particular functions in a library, or even that those functions return
particular results.")
    (license license:perl-license)))

(define-public perl-devel-checkcompiler
  (package
  (name "perl-devel-checkcompiler")
  (version "0.07")
  (source (origin
            (method url-fetch)
            (uri (string-append "mirror://cpan/authors/id/S/SY/SYOHEX/"
                                "Devel-CheckCompiler-" version ".tar.gz"))
            (sha256
             (base32
              "1db973a4dbyknjxq608hywil5ai6vplnayshqxrd7m5qnjbpd2vn"))))
  (build-system perl-build-system)
  (native-inputs
   (list perl-module-build-tiny))
  (home-page "https://metacpan.org/release/Devel-CheckCompiler")
  (synopsis "Check compiler availability")
  (description "@code{Devel::CheckCompiler} is a tiny module to check
whether a compiler is available.  It can test for a C99 compiler, or
you can tell it to compile a C source file with optional linker flags.")
  (license (package-license perl))))

(define-public perl-devel-cycle
  (package
    (name "perl-devel-cycle")
    (version "1.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/L/LD/LDS/Devel-Cycle-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1hhb77kz3dys8yaik452j22cm3510zald2mpvfyv5clqv326aczx"))))
    (build-system perl-build-system)
    (home-page
     "https://metacpan.org/release/Devel-Cycle")
    (synopsis "Find memory cycles in objects")
    (description
     "@code{Devel::Cycle} This is a tool for finding circular references in
objects and other types of references.  Because of Perl's reference-count
based memory management, circular references will cause memory leaks.")
    (license license:perl-license)))

(define-public perl-devel-globaldestruction
  (package
    (name "perl-devel-globaldestruction")
    (version "0.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/H/HA/HAARG/"
                           "Devel-GlobalDestruction-" version ".tar.gz"))
       (sha256
        (base32
         "1aslj6myylsvzr0vpqry1cmmvzbmpbdcl4v9zrl18ccik7rabf1l"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-sub-exporter-progressive))
    (home-page "https://metacpan.org/release/Devel-GlobalDestruction")
    (synopsis "Provides equivalent of ${^GLOBAL_PHASE} eq 'DESTRUCT' for older perls")
    (description "Devel::GlobalDestruction provides a function returning the
equivalent of \"$@{^GLOBAL_PHASE@} eq 'DESTRUCT'\" for older perls.")
    (license (package-license perl))))

(define-public perl-devel-hide
  (package
    (name "perl-devel-hide")
    (version "0.0010")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/F/FE/FERREIRA/Devel-Hide-"
                           version ".tar.gz"))
       (sha256
        (base32 "10jyv9nmv513hs75rls5yx2xn82513xnnhjir3dxiwgb1ykfyvvm"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-test-pod perl-test-pod-coverage))
    (home-page "https://metacpan.org/release/Devel-Hide")
    (synopsis "Forces the unavailability of specified Perl modules (for testing)")
    (description "Given a list of Perl modules/filenames, this module makes
@code{require} and @code{use} statements fail (no matter whether the specified
files/modules are installed or not).")
    (license (package-license perl))))

(define-public perl-devel-leak
  (package
    (name "perl-devel-leak")
    (version "0.03")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/N/NI/NI-S/"
                           "Devel-Leak-" version ".tar.gz"))
       (sha256
        (base32
         "0lkj2xwc3lhxv7scl43r8kfmls4am0b98sqf5vmf7d72257w6hkg"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Devel-Leak")
    (synopsis "Utility for looking for perl objects that are not reclaimed")
    (description
     "This module provides a basic way to discover if a piece of perl code is
allocating perl data and not releasing them again.")
    (license license:perl-license)))

(define-public perl-devel-lexalias
  (package
    (name "perl-devel-lexalias")
    (version "0.05")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RC/RCLAMP/"
                           "Devel-LexAlias-" version ".tar.gz"))
       (sha256
        (base32
         "0wpfpjqlrncslnmxa37494sfdy0901510kj2ds2k6q167vadj2jy"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-devel-caller))
    (home-page "https://metacpan.org/release/Devel-LexAlias")
    (synopsis "Alias lexical variables")
    (description "Devel::LexAlias provides the ability to alias a lexical
variable in a subroutines scope to one of your choosing.")
    (license (package-license perl))))

(define-public perl-devel-overloadinfo
  (package
    (name "perl-devel-overloadinfo")
    (version "0.005")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/I/IL/ILMARI/"
                           "Devel-OverloadInfo-" version ".tar.gz"))
       (sha256
        (base32
         "1rx6g8pyhi7lx6z130b7vlf8syzrq92w9ky8mpw4d6bwlkzy5zcb"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-fatal))
    (propagated-inputs
     (list perl-package-stash perl-sub-identify perl-mro-compat))
    (home-page "https://metacpan.org/release/Devel-OverloadInfo")
    (synopsis "Introspect overloaded operators")
    (description "Devel::OverloadInfo returns information about overloaded
operators for a given class (or object), including where in the inheritance
hierarchy the overloads are declared and where the code implementing it is.")
    (license (package-license perl))))

(define-public perl-devel-partialdump
  (package
    (name "perl-devel-partialdump")
    (version "0.18")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Devel-PartialDump-" version ".tar.gz"))
       (sha256
        (base32
         "0i1khiyi4h4h8vfwn7xip5c53z2hb2rk6407f3csvrdsiibvy53q"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build-tiny perl-test-warn perl-test-simple))
    (propagated-inputs
     (list perl-class-tiny perl-sub-exporter perl-namespace-clean))
    (home-page "https://metacpan.org/release/Devel-PartialDump")
    (synopsis "Partial dumping of data structures")
    (description "This module is a data dumper optimized for logging of
arbitrary parameters.")
    (license (package-license perl))))

(define-public perl-devel-stacktrace
  (package
    (name "perl-devel-stacktrace")
    (version "2.04")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/"
                           "Devel-StackTrace-" version ".tar.gz"))
       (sha256
        (base32 "0mb8bngjq7s3kbh95h3ig4p3jfb156c4r0d53z344gbxaknh6g6d"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Devel-StackTrace")
    (synopsis "Object representing a stack trace")
    (description "The Devel::StackTrace module contains two classes,
Devel::StackTrace and Devel::StackTrace::Frame.  These objects encapsulate the
information that can be retrieved via Perl's caller() function, as well as
providing a simple interface to this data.")
    (license license:artistic2.0)))

(define-public perl-devel-stacktrace-ashtml
  (package
    (name "perl-devel-stacktrace-ashtml")
    (version "0.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MI/MIYAGAWA/"
                           "Devel-StackTrace-AsHTML-" version ".tar.gz"))
       (sha256
        (base32
         "0iri5nb2lb76qv5l9z0vjpfrq5j2fyclkd64kh020bvy37idp0v2"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-devel-stacktrace))
    (home-page "https://metacpan.org/release/Devel-StackTrace-AsHTML")
    (synopsis "Displays stack trace in HTML")
    (description "Devel::StackTrace::AsHTML adds as_html method to
Devel::StackTrace which displays the stack trace in beautiful HTML, with code
snippet context and function parameters.  If you call it on an instance of
Devel::StackTrace::WithLexicals, you even get to see the lexical variables of
each stack frame.")
    (license (package-license perl))))

(define-public perl-devel-symdump
  (package
    (name "perl-devel-symdump")
    (version "2.18")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AN/ANDK/"
                           "Devel-Symdump-" version ".tar.gz"))
       (sha256
        (base32
         "1h3n0w23camhj20a97nw7v40rqa7xcxx8vkn2qjjlngm0yhq2vw2"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Devel-Symdump")
    (synopsis "Dump symbol names or the symbol table")
    (description "Devel::Symdump provides access to the perl symbol table.")
    (license (package-license perl))))

(define-public perl-digest-crc
  (package
    (name "perl-digest-crc")
    (version "0.23")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/O/OL/OLIMAUL/Digest-CRC-"
            version ".tar.gz"))
      (sha256
       (base32 "1n64qnjxhw1jjikxgfa1x5a4f7qi298839r3xhzvmj5736754j51"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Digest-CRC")
    (synopsis "Generic CRC functions")
    (description "The @code{Digest::CRC} module calculates CRC sums of
all sorts.  It contains wrapper functions with the correct parameters
for CRC-CCITT, CRC-16 and CRC-32.")
    (license license:public-domain)))

(define-public perl-digest-hmac
  (package
    (name "perl-digest-hmac")
    (version "1.04")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AR/ARODLAND/"
                           "Digest-HMAC-" version ".tar.gz"))
       (sha256
        (base32 "1m4fn0w3hb4vn7k5kja508a5hjmcrm28zhdpjkbl8p17m9b83g6n"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Digest-HMAC")
    (synopsis "Keyed-Hashing for Message Authentication")
    (description "The Digest::HMAC module follows the common Digest::
interface for the RFC 2104 HMAC mechanism.")
    (license (package-license perl))))

(define-public perl-digest-md4
  (package
    (name "perl-digest-md4")
    (version "1.9")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/M/MI/MIKEM/DigestMD4/Digest-MD4-"
            version ".tar.gz"))
      (sha256
       (base32
        "19ma1hmvgiznq95ngzvm6v4dfxc9zmi69k8iyfcg6w14lfxi0lb6"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Digest-MD4")
    (synopsis "Interface to the MD4 Algorithm")
    (description "The @code{Digest::MD4} module allows you to use the
RSA Data Security Inc.@: MD4 Message Digest algorithm from within Perl
programs.  The algorithm takes as input a message of arbitrary length
and produces as output a 128-bit \"fingerprint\" or \"message digest\"
of the input.  MD4 is described in RFC 1320.")
    (license license:perl-license)))

(define-public perl-digest-md5
  (package
    (name "perl-digest-md5")
    (version "2.58")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/T/TO/TODDR/Digest-MD5-"
                           version ".tar.gz"))
       (sha256
        (base32 "057psy6k7im0pr3344ny6k5rsnbqj8aizkmwgw53kbbngabh20kx"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Digest-MD5")
    (synopsis "Perl interface to the MD-5 algorithm")
    (description
     "The @code{Digest::MD5} module allows you to use the MD5 Message Digest
algorithm from within Perl programs.  The algorithm takes as
input a message of arbitrary length and produces as output a
128-bit \"fingerprint\" or \"message digest\" of the input.")
    (license (package-license perl))))

(define-public perl-digest-sha
  (package
    (name "perl-digest-sha")
    (version "6.02")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://cpan/authors/id/M/MS/MSHELOR/Digest-SHA-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "01lv0dc3mgnl3ap8npdnqiwmdqz2yc5bziss648c5jgalfzacric"))))
    (build-system perl-build-system)
    (home-page
      "https://metacpan.org/release/Digest-SHA")
    (synopsis
      "Perl extension for SHA-1/224/256/384/512")
    (description
     "The @code{Digest::SHA} Perl module implements the hash functions
of the SHA family. It also provides the @code{shasum} binary.")
    (license (package-license perl))))

(define-public perl-digest-sha1
  (package
    (name "perl-digest-sha1")
    (version "2.13")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/G/GA/GAAS/"
                                  "Digest-SHA1-" version ".tar.gz"))
              (sha256
               (base32
                "1k23p5pjk42vvzg8xcn4iwdii47i0qm4awdzgbmz08bl331dmhb8"))))
    (build-system perl-build-system)
    (synopsis "Perl implementation of the SHA-1 message digest algorithm")
    (description
     "This package provides @code{Digest::SHA1}, an implementation of the NIST
SHA-1 message digest algorithm for use by Perl programs.")
    (home-page "https://metacpan.org/release/Digest-SHA1")
    (license (package-license perl))))

(define-public perl-dist-checkconflicts
  (package
    (name "perl-dist-checkconflicts")
    (version "0.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/D/DO/DOY/"
                                  "Dist-CheckConflicts-" version ".tar.gz"))
              (sha256
               (base32
                "1i7dr9jpdiy2nijl2p4q5zg2q2s9ckbj2hs4kmnnckf9hsb4p17a"))))
    (build-system perl-build-system)
    (native-inputs (list perl-test-fatal))
    (propagated-inputs
     (list perl-module-runtime))
    (home-page "https://metacpan.org/release/Dist-CheckConflicts")
    (synopsis "Declare version conflicts for your dist")
    (description "This module allows you to specify conflicting versions of
modules separately and deal with them after the module is done installing.")
    (license (package-license perl))))

(define-public perl-dynaloader-functions
  (package
    (name "perl-dynaloader-functions")
    (version "0.003")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://cpan/authors/id/Z/ZE/ZEFRAM/DynaLoader-Functions-"
            version ".tar.gz"))
      (sha256
       (base32
        "10x13q920j9kid7vmbj6fiaz153042dy4mwdmpzrdrxw2ir39ciy"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build perl-test-pod perl-test-pod-coverage))
    (home-page "https://metacpan.org/release/DynaLoader-Functions")
    (synopsis "Deconstructed dynamic C library loading")
    (description "This module provides a function-based interface to
dynamic loading as used by Perl.  Some details of dynamic loading are
very platform-dependent, so correct use of these functions requires
the programmer to be mindfulof the space of platform variations.")
    (license license:perl-license)))

(define-public perl-encode
  (package
    (name "perl-encode")
    (version "3.19")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DA/DANKOGAI/"
                           "Encode-" version ".tar.gz"))
       (sha256
        (base32 "1x9f0naqskv9v7dif480vrzfmn8zhvq9g0w3r164v7pnxr4ghqwi"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/dist/Encode")
    (synopsis "Character encodings in Perl")
    (description "Encode module provides the interface between Perl strings and
the rest of the system.  Perl strings are sequences of characters.")
    (license (package-license perl))))

(define-public perl-encode-detect
  (package
    (name "perl-encode-detect")
    (version "1.01")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/J/JG/JGMYERS/"
                           "Encode-Detect-" version ".tar.gz"))
       (sha256
        (base32
         "1wdv9ffgs4xyfh5dnh09dqkmmlbf5m1hxgdgb3qy6v6vlwx8jkc3"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build))
    (home-page "https://metacpan.org/release/Encode-Detect")
    (synopsis "Detect the encoding of data")
    (description "This package provides a class @code{Encode::Detect} to detect
the encoding of data.")
    (license license:mpl1.1)))

(define-public perl-encode-eucjpascii
  (package
    (name "perl-encode-eucjpascii")
    (version "0.03")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/N/NE/NEZUMI/"
                           "Encode-EUCJPASCII-" version ".tar.gz"))
       (sha256
        (base32
         "0qg8kmi7r9jcf8326b4fyq5sdpqyim2a11h7j77q577xam6x767r"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Encode-EUCJPASCII")
    (synopsis "ASCII mapping for eucJP encoding")
    (description "This package provides an ASCII mapping for the eucJP
encoding.")
    (license (package-license perl))))

(define-public perl-encode-jis2k
  (package
    (name "perl-encode-jis2k")
    (version "0.03")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DA/DANKOGAI/"
                           "Encode-JIS2K-" version ".tar.gz"))
       (sha256
        (base32
         "1k1mdj4rd9m1z4h7qd2dl92ky0r1rk7mmagwsvdb9pirvdr4vj0y"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Encode-JIS2K")
    (synopsis "JIS X 0212 (aka JIS 2000) encodings")
    (description "This package provides encodings for JIS X 0212, which is
also known as JIS 2000.")
    (license (package-license perl))))

(define-public perl-encode-hanextra
  (package
    (name "perl-encode-hanextra")
    (version "0.23")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AU/AUDREYT/"
                           "Encode-HanExtra-" version ".tar.gz"))
       (sha256
        (base32
         "0fj4vd8iva2i0j6s2fyhwgr9afrvhr6gjlzi7805h257mmnb1m0z"))))
    (build-system perl-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-env
           (lambda _ (setenv "PERL_USE_UNSAFE_INC" "1") #t)))))
    (home-page "https://metacpan.org/release/Encode-HanExtra")
    (synopsis "Additional Chinese encodings")
    (description "This Perl module provides Chinese encodings that are not
part of Perl by default, including \"BIG5-1984\", \"BIG5-2003\", \"BIG5PLUS\",
\"BIG5EXT\", \"CCCII\", \"EUC-TW\", \"CNS11643-*\", \"GB18030\", and
\"UNISYS\".")
    (license license:expat)))

(define-public perl-env-path
  (package
    (name "perl-env-path")
    (version "0.19")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/D/DS/DSB/Env-Path-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1qhmj15a66h90pjl2dgnxsb9jj3b1r5mpvnr87cafcl8g69z0jr4"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Env-Path")
    (synopsis "Advanced operations on path variables")
    (description "@code{Env::Path} presents an object-oriented interface to
path variables, defined as that subclass of environment variables which name
an ordered list of file system elements separated by a platform-standard
separator.")
    (license (package-license perl))))

(define-public perl-error
  (package
    (name "perl-error")
    (version "0.17028")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/S/SH/SHLOMIF/"
                                  "Error-" version ".tar.gz"))
              (sha256
               (base32
                "0q796nwwiarfc6pga97380c9z8xva5545632001qj75kb1g5rn1s"))))
    (build-system perl-build-system)
    (native-inputs (list perl-module-build))
    (home-page "https://metacpan.org/release/Error")
    (synopsis "OO-ish Error/Exception handling for Perl")
    (description "The Error package provides two interfaces.  Firstly Error
provides a procedural interface to exception handling.  Secondly Error is a
base class for errors/exceptions that can either be thrown, for subsequent
catch, or can simply be recorded.")
    (license (package-license perl))))

(define-public perl-eval-closure
  (package
    (name "perl-eval-closure")
    (version "0.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DO/DOY/"
                           "Eval-Closure-" version ".tar.gz"))
       (sha256
        (base32
         "1bcc47r6zm3hfr6ccsrs72kgwxm3wkk07mgnpsaxi67cypr482ga"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-fatal perl-test-requires))
    (propagated-inputs
     (list perl-devel-lexalias))
    (home-page "https://metacpan.org/release/Eval-Closure")
    (synopsis "Safely and cleanly create closures via string eval")
    (description "String eval is often used for dynamic code generation.  For
instance, Moose uses it heavily, to generate inlined versions of accessors and
constructors, which speeds code up at runtime by a significant amount.  String
eval is not without its issues however - it's difficult to control the scope
it's used in (which determines which variables are in scope inside the eval),
and it's easy to miss compilation errors, since eval catches them and sticks
them in $@@ instead.  This module attempts to solve these problems.  It
provides an eval_closure function, which evals a string in a clean
environment, other than a fixed list of specified variables.  Compilation
errors are rethrown automatically.")
    (license (package-license perl))))

(define-public perl-eval-withlexicals
  (package
    (name "perl-eval-withlexicals")
    (version "1.003006")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/H/HA/HAARG/Eval-WithLexicals-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0x09mq0q745cxkw3xgr0h7dil7p1pdq3l5299kj3mk2ijkk2gwb6"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'install 'wrap-tinyrepl
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out")))
                        (wrap-program (string-append out "/bin/tinyrepl")
                          `("PERL5LIB" ":" prefix
                            (,(getenv "PERL5LIB")
                             ,(string-append out "/lib/perl5/site_perl"))))
                        #t))))))
    (propagated-inputs
     (list perl-moo perl-strictures))
    (home-page "https://metacpan.org/release/Eval-WithLexicals")
    (synopsis "Lexical scope evaluation library for Perl")
    (description "The Eval::WithLexicals Perl library provides support for
lexical scope evaluation.  This package also includes the @command{tinyrepl}
command, which can be used as a minimal Perl read-eval-print loop (REPL).")
    (license (package-license perl))))

(define-public perl-exception-class
  (package
    (name "perl-exception-class")
    (version "1.44")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DR/DROLSKY/"
                           "Exception-Class-" version ".tar.gz"))
       (sha256
        (base32
         "03gf4cdgrjnljgrlxkvbh2cahsyzn0zsh2zcli7b1lrqn7wgpwrk"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-devel-stacktrace perl-class-data-inheritable))
    (home-page "https://metacpan.org/release/Exception-Class")
    (synopsis "Allows you to declare real exception classes in Perl")
    (description "Exception::Class allows you to declare exception hierarchies
in your modules in a \"Java-esque\" manner.")
    (license (package-license perl))))

(define-public perl-exporter
  (package
    (name "perl-exporter")
    (version "5.74")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/T/TO/TODDR/"
                           "Exporter-" version ".tar.gz"))
       (sha256
        (base32 "1f25k5iaygiizlrkbbl6wxd647pwfmynykxalq6r9bbkysg8inza"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-pod))
    (propagated-inputs
     (list perl-carp))
    (home-page "https://metacpan.org/dist/Exporter")
    (synopsis "Default import method for modules")
    (description "Exporter implements an import method which allows a module to
export functions and variables to its users' namespaces.  Many modules use
Exporter rather than implementing their own import method because Exporter
provides a highly flexible interface, with an implementation optimised for the
common case.")
    (license (package-license perl))))

(define-public perl-exporter-lite
  (package
    (name "perl-exporter-lite")
    (version "0.08")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/N/NE/NEILB/"
                                  "Exporter-Lite-" version ".tar.gz"))
              (sha256
               (base32
                "1hns15imih8z2h6zv3m1wwmv9fiysacsb52y94v6zf2cmw4kjny0"))))
    (build-system perl-build-system)
    (synopsis "Lightweight exporting of functions and variables")
    (description
     "Exporter::Lite is an alternative to Exporter, intended to provide a
lightweight subset of the most commonly-used functionality.  It supports
import(), @@EXPORT and @@EXPORT_OK and not a whole lot else.")
    (home-page "https://metacpan.org/release/Exporter-Lite")
    (license (package-license perl))))

(define-public perl-exporter-tiny
  (package
    (name "perl-exporter-tiny")
    (version "1.002001")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/T/TO/TOBYINK/"
                           "Exporter-Tiny-" version ".tar.gz"))
       (sha256
        (base32 "13f4sd9n9iyi15r5rbjbmawajxlgfdvvyrvwlyg0yjyf09636b58"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Exporter-Tiny")
    (synopsis "Exporter with the features of Sub::Exporter but only core dependencies")
    (description "Exporter::Tiny supports many of Sub::Exporter's
external-facing features including renaming imported functions with the `-as`,
`-prefix` and `-suffix` options; explicit destinations with the `into` option;
and alternative installers with the `installler` option.  But it's written in
only about 40% as many lines of code and with zero non-core dependencies.")
    (license (package-license perl))))

(define-public perl-extutils-manifest
  (package
    (name "perl-extutils-manifest")
    (version "1.73")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://cpan.metacpan.org/authors/id/E/ET/ETHER/"
                    "ExtUtils-Manifest-" version ".tar.gz"))
              (sha256
               (base32
                "1y5siyw9sbxq6kdmsjfsx0mrbqb6xr8kmniwli7xc6hbmhyhcp6w"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/ExtUtils-Manifest")
    (synopsis "Utilities to write and check a MANIFEST file")
    (description "This package contains functions to manipulate a MANIFEST
file.  The package exports no functions by default.  The following are exported
on request: mkmanifest, manifind, manicheck, filecheck, fullcheck, skipcheck,
maniread, maniskip, manicopy, maniadd.")
    (license (package-license perl))))

(define-public perl-extutils-installpaths
  (package
    (name "perl-extutils-installpaths")
    (version "0.012")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/L/LE/LEONT/"
                           "ExtUtils-InstallPaths-" version ".tar.gz"))
       (sha256
        (base32
         "1v9lshfhm9ck4p0v77arj5f7haj1mmkqal62lgzzvcds6wq5www4"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-extutils-config))
    (home-page "https://metacpan.org/release/ExtUtils-InstallPaths")
    (synopsis "Build.PL install path logic made easy")
    (description "This module tries to make install path resolution as easy as
possible.")
    (license (package-license perl))))

(define-public perl-extutils-config
  (package
    (name "perl-extutils-config")
    (version "0.008")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/L/LE/LEONT/"
                           "ExtUtils-Config-" version ".tar.gz"))
       (sha256
        (base32
         "130s5zk4krrymbynqxx62g13jynnb7xi7vdpg65cw3b56kv08ldf"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/ExtUtils-Config")
    (synopsis "Wrapper for perl's configuration")
    (description "ExtUtils::Config is an abstraction around the %Config hash.
By itself it is not a particularly interesting module by any measure