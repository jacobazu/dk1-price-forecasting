@echo off
rem chronos-2, then chronos-2 on prices alone, then tabpfn; each continues where it stopped. log in run_log.txt.
rem the pc shuts down after, with two minutes to cancel: shutdown /a
cd /d "%~dp0"
.venv\Scripts\python foundation.py chronos2 ..\data\processed\bakeoff\daily.csv ..\data\processed\bakeoff\out >> run_log.txt 2>&1
.venv\Scripts\python foundation.py chronos2_plain ..\data\processed\bakeoff\daily.csv ..\data\processed\bakeoff\out >> run_log.txt 2>&1
.venv\Scripts\python foundation.py tabpfn ..\data\processed\bakeoff\daily.csv ..\data\processed\bakeoff\out >> run_log.txt 2>&1
shutdown /s /t 120
