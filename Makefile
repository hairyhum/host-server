YAWS_DIR=/usr/lib/yaws

YAWS_EBIN=$(YAWS_DIR)/ebin
ETEST_EBIN=$(CURDIR)/ebin $(CURDIR)/libs/epgsql/ebin $(CURDIR)/libs/jiffy/ebin
DOCROOT=${CURDIR}/www
REBAR=./rebar

ERL_OPTS=+K true
ERL_RUN=-eval 'ok = application:start(todo).'

all: rebar clean compile test

rebar:
	wget http://cloud.github.com/downloads/basho/rebar/rebar && chmod u+x rebar

clean:
	${REBAR} clean
	rm -f *.access *.auth *.log *.dump

compile:
	${REBAR} compile
	sed -i 's,SEE_MAKEFILE,${DOCROOT},' ebin/todo.app

test: eunit
eunit:
	${REBAR} eunit

run:
	erl ${ERL_OPTS} -pa ${YAWS_DIR} ${YAWS_EBIN} ${ETEST_EBIN} ${ERL_RUN}

drun:
	erl ${ERL_OPTS} -detached -pa ${YAWS_DIR} ${YAWS_EBIN} ${ETEST_EBIN} ${ERL_RUN}

archive:
	git archive --format tar --prefix todo/ HEAD | gzip > todo.tar.gz

