CXXOPT1=-ggdb
CXXOPT2=-pg -fprofile-arcs -ftest-coverage
# CXXOPT3=-O4
CXXOPT=$(CXXOPT1) $(CXXOPT2) $(CXXOPT3)

elymas: $(shell ls *.c++ | sed -e 's/c++/o/g')
	g++-4.5 $(CXXOPT) -o $@ $^

%.o: %.c++ *.h
	g++-4.5 $(CXXOPT) -std=c++0x -c -o $@ $<

clean:
	rm -fv *.o *.gcda *.gcno
	rm -fv elymas
