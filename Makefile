CXX = g++
CXXFLAGS = -std=c++20 -Wall -pedantic -g \
           -Wno-unused-parameter -Wno-unused-variable

all: main.exe

main.exe: main.cpp
	$(CXX) $(CXXFLAGS) main.cpp -o main.exe

clean:
	rm -rf main.exe main.exe.dSYM
