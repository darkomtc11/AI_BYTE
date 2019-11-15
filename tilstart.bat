@echo off
set arg1=%1
set arg2=%2
shift
shift

python tilisp.py %arg1% %arg2%
