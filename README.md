# gps-bank-data-analysis

Automatically align GPS and bank data

This software was originally programmed for the course "Web data management"
of the Parisian Master of Research in Computer Science.

## How to build the software
To compile it, you will need the GHC compiler and cabal-install.

    # Download the sources
    wget https://github.com/Zimmi48/gps-bank-data-analysis/archive/master.zip
    unzip master.zip
    cd gps-bank-data-analysis
    cabal install

## How to use the software

The first time you run the software, it will prompt you for
a Google Places API key. To create one, go to the
[Google APIs Console](https://code.google.com/apis/console/?noredirect).

To know all the options, do `gps-bank-data-analysis --help`.

### Input files

The software takes as input a position data file (in KML or Google's JSON format).
One way to get one would be going to
[Google Takeout](https://www.google.com/settings/takeout)
(if you are tracked by Google).

If you have the choice between the two formats, we recommend Google's JSON format
because it contains more information than the KML format.

The second input must be a bank data file in OFX format. I found
[this blog post](http://thefinancebuff.com/replacing-microsoft-money-part-5-ofx-scripts.html)
explaining a way of downloading such a file.
You will notice however that the script they link to only fetches
the latest 31 days of transaction data.
In order to obtain a longer transaction history, edit the script,
locate and modify the following line:

    dtstart = time.strftime("%Y%m%d",time.localtime(time.time()-31*86400))

## What does the software do?
If you are able to get these two kinds of file, the program will align the
information contained in them with the help of Google Places.
Thus, for some of the transactions, it will show you the precise place
(name and address of establishment)
and the exact time when the transaction happened.

If there is an ambiguity on some of the data, it may show you several
possibilities for the same transaction (in particular, different times
of the day when it might have occured).

## License

This software is a free software available under the terms of the MPL 2.0
(Mozilla Public License 2.0).
The text of the license is provided in the [LICENSE](LICENSE) file.

If you don't know what the MPL 2.0 is, you could have a look at the
[official FAQ](https://www.mozilla.org/MPL/2.0/FAQ.html).

Do not hesitate to submit patches (pull requests) for any improvement
you may have done!
