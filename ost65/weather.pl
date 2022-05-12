#!/usr/bin/perl
# Try to get weather for POINT82.
   # E X A M P L E   D A T A
   #       |TEMP.|HUMID|DEW PT.| BAROM | WIND_DIR | WIND SPEED  | PARTICLE |  RAIN
   # TIME  | avg | avg |  avg  | PRESS | avg stdev| avg max min |  COUNT   |  Y/N
   #-[GMT]-|-[F]-|-[%]-|--[F]--|[In.Hg]|---[Az]---|----[MPH]----|--[ppcf]--|[on/off]
   # 22:50 |  76 |  19 |  30.4 | 23.46 | 222 ~ 15 |  16  25   8 |     2747 |   N

   # should parse 76 19 23.46 

   $weather = `/usr/bin/curl -s -m 3 http://weather.as.utexas.edu/latest_5min.dat`;
   if ($weather =~ m/\s+\d\d:\d\d\s+\|\s+(-?\d+\d+)\s+\|\s+>?(\d\d)\s+\|.*\|\s+(\d\d\.\d+)/) {
      if (open(TEMP,">/var/point82/weather.temp")) { 
         print TEMP "$3 $1 $2\n"; # Pressure, Temperature, Relative Humidity
         close(TEMP);
         rename '/var/point82/weather.temp',  '/var/point82/weather.dat';
      } else {
         unlink '/var/point82/weather.dat';
      }
   } else {
      unlink '/var/point82/weather.dat';
   }

