#ref = "http://urin.github.io/posts/2013/simple-makefile-for-clang/"

TARGET = mips.out
COMPILER = clang++
CFLAGS = -g -MMD -MP -Wall -Wextra -Winit-self -Wno-unused-parameter -Wfloat-equal -std=c++14 -O3 #-fopenmp
LDFLAGS = #-fopenmp
LIBS =
INCLUDE =
SOURCES = $(wildcard *.cpp)
OBJECTS = $(SOURCES:.cpp=.o)
DEPENDS = $(OBJECTS:.o=.d)


$(TARGET): $(OBJECTS) $(LIBS)
	$(COMPILER) -o $@ $^ $(LDFLAGS)

%.o: %.cpp
	$(COMPILER) $(CFLAGS) $(INCLUDE) -o $@ -c $<

.PHONY: all
all: clean $(TARGET)

.PHONY: clean
clean:
	rm -f $(OBJECTS) $(DEPENDS) $(TARGET)

-include $(DEPENDS)
