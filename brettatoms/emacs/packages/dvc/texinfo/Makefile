

srcdir = ../../dvc-dev/texinfo
top_srcdir = ../../dvc-dev
PACKAGE_VERSION = 0

##############################################################################
# location of required programms
RM   = @RM@
MKDIR_P = /bin/mkdir -p
INSTALL = /usr/bin/install -c
INSTALL_DATA = ${INSTALL} -m 644
MAKEINFO = makeinfo
TEXI2DVI = texi2dvi

# Other settings
datarootdir = ${prefix}/share
prefix = /usr/local
info_dir = ${datarootdir}/info

##############################################################################
all: info dvc.dvi  dvc.html dvc.pdf

dvi: dvc.dvi

pdf: dvc.pdf

html: dvc.html

Makefile: $(srcdir)/Makefile.in ../config.status
	cd ..; ./config.status

ii = install-info

install: uninstall info
	$(MKDIR_P) -m 0755 $(info_dir)
	@for i in dvc.info* ; do		\
	  echo Installing $$i ;			\
          $(INSTALL_DATA) $$i $(info_dir) ;	\
	done
	@if ($(ii) --version &&					\
	     $(ii) --version 2>&1 | sed 1q | grep -i -v debian)	\
	   >/dev/null 2>&1 ; then				\
	  $(ii) --info-dir="$(info_dir)" "$(info_dir)/dvc.info"	\
	    || : ; else : ;					\
	fi

uninstall:
	rm -f $(info_dir)/dvc.info*

info: dvc.info

alldeps = $(srcdir)/dvc.texinfo dvc-version.texinfo

dvc.info: $(alldeps)
	$(MAKEINFO) $(srcdir)/dvc.texinfo

dvc.html: $(alldeps)
	$(MAKEINFO) --html --no-split $(srcdir)/dvc.texinfo

dvc.dvi: $(alldeps)
	$(TEXI2DVI) -o $@ $(srcdir)/dvc.texinfo

dvc.pdf: $(alldeps)
	$(TEXI2DVI) -o $@ -p $(srcdir)/dvc.texinfo

clean:
	rm -f *.aux *.cp *.cps *.dvi *.pdf *.fn *.fns *.ky *.log *.pg \
	  *.toc *.tp *.vr *.vrs *.html *.info

distclean: clean
	rm -f Makefile

maintainer-clean:
	rm -f dvc-version.texinfo

.PHONY: pdf dvi html info clean distclean install-pkg uninstall-pkg all

dvc-version.texinfo: $(top_srcdir)/configure
	@echo Creating $@
	@( echo @set VERSION $(PACKAGE_VERSION) ; \
	   date '+@set UPDATED %F' -r $< ) > $@

.PHONY: all dvi pdf html info \
	install uninstall \
	clean distclean maintainer-clean
