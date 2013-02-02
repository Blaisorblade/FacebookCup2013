ROUND=qualification
#ROUND=round1
BASESRC=src/main/scala
UTILS=$(BASESRC)/util
SRCPATH=$(BASESRC)/$(ROUND)

COMMON_SRCS=$(wildcard $(UTILS)/*.scala) build.sbt project/build.properties

%.tar.gz: $(SRCPATH)/%.scala $(COMMON_SRCS)
	tar czf $@ $^

lsprobs:
	@cd $(SRCPATH); ls *.scala | sed -e 's/\.scala$$/.tar.gz/'
