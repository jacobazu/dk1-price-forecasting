@echo off
rem chronos-2 then tabpfn, both continue where they stopped; the log is in run_log.txt. the pc shuts down after,
rem with two minutes to cancel: shutdown /a
cd /d "%~dp0"
.venv\Scripts\python foundation.py chronos2 ..\data\processed\bakeoff\daily.csv ..\data\processed\bakeoff\out >> run_log.txt 2>&1
.venv\Scripts\python foundation.py tabpfn ..\data\processed\bakeoff\daily.csv ..\data\processed\bakeoff\out >> run_log.txt 2>&1
shutdown /s /t 120
