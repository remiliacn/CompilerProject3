cmake_minimum_required(VERSION 3.4)
project(Project3)

set(CMAKE_CXX_STANDARD 11)

add_executable(Project3 compiler.h compiler.cc demo.cc inputbuf.cc inputbuf.h lexer.h lexer.cc)