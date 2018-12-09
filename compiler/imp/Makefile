CXX = g++
CXXFLAGS = -Wall -Wextra -std=c++17 -g
TARGET = imp

OBJDIR = obj

XXDFILES = prologue.asm epilogue.asm
XXDCPP = $(patsubst %,%.cpp,$(XXDFILES))

.PHONY: all clean

.SUFFIXES:

all: $(TARGET)

clean:
	@echo "Cleaning"
	@rm -f $(TARGET)
	@rm -rf $(OBJDIR)


$(OBJDIR)/%.o: %.cpp $(wildcard *.h)
	@mkdir -p $(OBJDIR)
	@echo "CXX $<"
	@$(CXX) $(CXXFLAGS) -c -o $@ $<

$(TARGET): $(patsubst %.cpp,$(OBJDIR)/%.o,$(wildcard *.cpp) $(XXDCPP))
	@echo "LD -o $@"
	@$(CXX) -o $@ $^

$(XXDCPP): %:
	@echo "XXD $<"
	@xxd -i $< >$@

prologue.asm.cpp: prologue.asm
epilogue.asm.cpp: epilogue.asm
