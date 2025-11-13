# salvelinr 

*Note! This is a package written by a biologist (i.e., not a programmer) for herself and collaborators. For other applications, use at your own risk!*

This package is a collection of accumulated functions I've written and reused over the past few years of working with positional telemetry data, particularly on charr (hence the name) in small boreal lakes. I am actively making them better and more transferrable, so I anticipate that there will be problems to solve with the functions as they are. I'm working on packaging them up so they can easily be used by other lab members. Below, I have given an overview of a sample workflow and flagged functions that I would *not* currently recommend using - but which may be ready soon! salvelinr depends on the tidyverse system of pacakges.

## Example workflow
### Step 1: 
You acquire a folder of detection .csv files. If they are from discrete receivers, this may be a very large folder with many different files that all need to be treated the same way. Depending on the scope of your study, positional (e.g., VPS) data may be similar, but they might also just need one simple line of code to read in: `read.csv("VPS Results 20251015.csv")`. Let's proceed assuming you have a large folder of files that need the same processing pipeline. The following functions could be useful:

- `readDetectsFolder` takes the path to your folder of detection files and reads them all in, storing them all together in one long data frame.
- `keepFish` will filter for the IDs that you want to keep. Typically, this will be IDs that you know are in the system - especially in a small contained lake where you know all the other researchers, and likely will not encounter tagged fish you haven't heard of.
- `deduplicate` removes detections that are within the minimum interval identified in the tag specs. This is likely to **substantially** cut down on the number of detections in your file, so I recommend doing this as soon as possible so that the rest of the code is faster and doesn't bring all the extra detections along. This function defaults to doing this like a "hammer" - which is to say by remvoing anything within the min interval of another detection - but if you have the computing power, specifying the type as "spear" will make it more precise and return more valid detections.

### Step 2:
It's very likely that these detections are in UTC time, but to make them meaningful - and interoperable with variables like sunrise and sunset - you'll need to convert them into a local timezone, and maybe even assign seasons and periods of the day (early night, late night, etc.) to each value. These functions are related to time:

- `timezone` very simply converts timestamps from UTC to local time. Provide the difference between the timezone you'd like and UTC, paying attention to +/- values. This will return columns with the suffix _LT, which stands for local time, not lake trout.
- `season` requires a vector of the first days of seasons. **This function assumes a four-season paradigm.** If `type == "ice"` it will assign based on dates, and if `type == "month"` it will assign based on months. The hemisphere argument defaults to "north," but specifying `hemisphere = "south"` will flip the seasonal order (and change the label "fall" to "autumn").
- `sundial` essentially runs `suncalc::getSunlightTimes`, but with the added twist that it will then add a column to indicate whether each instance is during the day, early night, or late night according to Cruz-Font et al. 2019.

### Step 3: 
You may have temperature data to work with as well, which may need to be interpolated. You will need to load "zoo" and "reshape2" to use the functions below.

- `interpolateDepths` should be used first, if both are to be used. It will linearly interpolate down each day's profile to fill in missing values at the given interval. I use this to get temps for every 0.1 m from meter-wise depth profiles. If you write `sinkends == TRUE` it will always give you a value for the very top and bottom of the lake, even if there are no data to support it.
- `interpolateDates` can be used next. It does the same thing, but along the date axis - linearly interpolates to fill in the values. 
