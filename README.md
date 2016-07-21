# A Tool For Working With MassMine Data

This repo is not yet fit for public consumption. In time, this will grow into a tool for working with data collected using [MassMine](http://www.massmine.org). 

## Features (coming soon)

Creating a one-size-fits-all analysis tool for social media and web data research is a tall, and likely impossible endeavor. However, some tasks are common to many goals, and this tool will attempt to accomplish at least the following:

* Data cleaning. For example, it will remove bad data (e.g., deleted tweets returned from Twitter), fix character encoding issues, etc.
* Data anonymization, to the extent possible. For example, across a given data set, @usernames can be consistently replaced with @1234 codes to minimize privacy risks while still providing a means for studying the interactions between networks of friends/users.
* Topic analyses. For example, quick and easy #hashtag identification, displayed as a sorted table of frequencies, co-occurrences, etc.
* Similar analyses for @user-mentions. Who's talking to who? Which users are the most productive/influential, etc.
* Time series analyses. How quickly were tweets occurring across time? Do changes in tweet patterns correspond to other identifiable data patterns? Etc.
* Tweet "hydration". Make your Twitter data set share-able by reducing tweets to coded representations permitted by Twitter.
* Filtering/removal of unnecessary data fields in MassMine JSON. MassMine returns a lot of information. You probably don't need it all. Reduce your data's file size by filtering out the chaff from the wheat. 
* Visual, web-browser interface for running MassMine and working with the resulting data.
* And more!

## Caching by default

MassMine provides data in line-oriented JSON format. `mmtool` will work with arbitrarily large JSON files by employing line-by-line based algorithms (as opposed to reading an entire data set into RAM). To avoid parsing data files more than necessary, and for additional speed up, `mmtool` will utilize a cache for complex and slow-to-calculate algorithms. This cache will be used to not only avoid calculating the same expensive task more than once, but will also be shared across analyses and tasks to ensure that each sub-routine calculates the minimum necessary to build upon what came before. For very large data sets, waiting is a given. But you shouldn't need to wait twice!

## Screenshots
mmtool is not yet visually polished. However, the software is currently working, and the following screenshots give a sneak peek:

mmtool provides time series visualization
![Time Series](https://github.com/n3mo/mmtool/raw/master/img/time-series.png)

mmtool also provides a JSON data viewer to allow the user to visually inspect their "raw" data
![Data Viewer](https://github.com/n3mo/mmtool/raw/master/img/data-viewer.png)

Analysis routines are individually threaded, and status information and results are available asynchronously at all times:
![Analysis Results](https://github.com/n3mo/mmtool/raw/master/img/results.png)

# Contributors

- [Aaron Beveridge](https://github.com/aabeveridge/)

# License

Copyright (C) 2016 Nicholas M. Van Horn

Author: Nicholas M. Van Horn <nvanhorn@capital.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
