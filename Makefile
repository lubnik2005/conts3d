# Makefile for compiling and linking the conts3d program

# Compiler settings
FC = gfortran
CC = cc
FFLAGS = -Wall -Wextra -g -std=legacy
# Uncomment line below for compilation on macos
# LFLAGS = -L../ygl -lYgl -L/opt/homebrew/lib -lGL -lGLU -lX11 -lXext -L/opt/X11/lib -lGLU -lgfortran -lquadmath -lm
# Uncomment line below for compilation on linux
LFLAGS = -L../ygl -lYgl -L/usr/lib64/ -lGL -lGLU -lX11 -lXext -lgfortran -lquadmath -lm

# Target executable name
TARGET = conts3d

# Source files and object files
SRCS = $(wildcard *.f)
OBJS = $(SRCS:.f=.o)

# Default target
all: $(TARGET)

# Rule to link the program
$(TARGET): $(OBJS)
	$(FC) -o $@ $^ $(LFLAGS)

# Rule to compile Fortran source files
%.o: %.f
	$(FC) $(FFLAGS) -c -o $@ $<

# Clean up build files
clean:
	rm -f $(OBJS) $(TARGET)

# Phony targets
.PHONY: all clean
