OUTDIR = ../ebin
SOURCES = $(wildcard *.erl)
BEAMS = $(addprefix $(OUTDIR)/,$(SOURCES:.erl=.beam))
EFLAGS += -I . +warn_missing_spec

all: 	$(BEAMS)

$(OUTDIR)/%.beam: %.erl
	erlc -W $(EFLAGS) -o $(OUTDIR) $<

clean:
	-rm -f $(BEAMS) $(ASN_OUT)
