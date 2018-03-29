# OLTTDataScrape
Script to collect OLTT data for upload to REDCap

## GitHub repository
The repository for this script can be found at [https://github.com/MichiganADC/OLTTDataScrape](https://github.com/MichiganADC/OLTTDataScrape).

## Running the script in Mac OS X or Linux

1. If necessary, copy the `OLTTDataScrape.R` file to the directory that contains the **OLTT Data** directory.

2. Make sure `OLTTDataScrape.R` is executable by running this command:

    ```chmod 744 OLTTDataScrape.R```

2. Open a terminal.

3. Navigate to the directory containing the `OLTTDataScrape.R` file.

    a. In Mac OS X, you would currently type this command:  ```cd /Volumes/Neuro_Shared/ClinicalCore/OLTT/```

4. Run the `OLTTDataScrape.R` script.

    a. In Mac OS X or Linux, type this command: ```./OLTTDataScrape.R```

Depending on how many OLTT data files there are to scrape, the script may take a few seconds to execute.

Once the script is finished running, there should be a file called `oltt_scraped_data.csv`. This `csv` file can be directly uploaded to the **UM MAP NACC 3.0** project in REDCap.

## Running the script in Windows
