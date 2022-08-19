import cdsapi
import requests
from datetime import datetime
import os
import shutil
from tenacity import retry, stop_after_attempt, wait_random_exponential

#-------------------------------------------------------------------------------
# Define Function for Retrieving Daily ERA5* via CDS API
# Modified by Jessica from script posted by Kevin Marsh
# Last edited September 2021
# 
# Original script:
# https://confluence.ecmwf.int/pages/viewpage.action?pageId=228867588
# 
# Requires:
# 1) The CDS API to be installed and working on your system
# 2) You have agreed to the ERA5 Licence (via the CDS web page)
# 3) Selection of required variable, daily statistic, etc
# 
# Output:
# 1) Separate netCDF file for chosen daily statistic/variable for each month
#-------------------------------------------------------------------------------
@retry(stop = stop_after_attempt(10), wait = wait_random_exponential(multiplier = 1, min = 5, max = 60))
def retrieve_era5_daily(
    dataset, 
    variable, 
    statistic, 
    year, 
    month, 
    product_type = 'reanalysis', 
    time_zone = 'UTC+00:00', 
    frequency = '1-hourly', 
    grid = None, 
    area = {'lat': [-90, 90], 'lon': [-180, 180]}, 
    file_name = None, 
    folder = None, 
    timeout = 300):

    # Launch CDS API
    c = cdsapi.Client(timeout = timeout)

    # Check data set
    assert dataset in [
        'reanalysis-era5-single-levels', 
        'reanalysis-era5-pressure-levels', 
        'reanalysis-era5-land'
    ]

    # Check product type
    if dataset in ['reanalysis-era5-single-levels', 'reanalysis-era5-pressure-levels']:
        assert product_type in [
            'reanalysis', 
            'ensemble_members', 
            'ensemble_mean'
        ]
    elif dataset == 'reanalysis-era5-land':
        assert product_type == 'reanalysis'

    # Check variable
    # For valid keywords, see Table 2 of:
    # https://datastore.copernicus-climate.eu/documents/app-c3s-daily-era5-statistics/C3S_Application-Documentation_ERA5-daily-statistics-v2.pdf

    # Check statistic
    assert statistic in [
        'daily_mean', 
        'daily_minimum',
        'daily_maximum',
        'daily_mid_range'
    ]

    # Check year
    if dataset in ['reanalysis-era5-single-levels', 'reanalysis-era5-pressure-levels']:
        earliest = 1950
    elif dataset == 'reanalysis-era5-land':
        earliest = 1981
    today = datetime.today()
    latest = today.year
    assert earliest < year < latest

    # Check month
    if year == latest:
        assert month < today.month

    # Convert year and month to string
    year = str(year)
    month = '0' + str(month) if month < 10 else str(month)

    # Check time zone
    assert time_zone in [
        'UTC-12:00', 'UTC-11:00', 'UTC-10:00', 
        'UTC-09:00', 'UTC-08:00', 'UTC-07:00', 
        'UTC-06:00', 'UTC-05:00', 'UTC-04:00', 
        'UTC-03:00', 'UTC-02:00', 'UTC-01:00', 
        'UTC+00:00', 'UTC+01:00', 'UTC+02:00', 
        'UTC+03:00', 'UTC+04:00', 'UTC+05:00', 
        'UTC+06:00', 'UTC+07:00', 'UTC+08:00', 
        'UTC+09:00', 'UTC+10:00', 'UTC+11:00'
        'UTC+12:00', 'UTC+13:00', 'UTC+14:00'
    ]

    # Check frequency
    assert frequency in ['1-hourly', '3-hourly', '6-hourly']

    # Check grid resolution
    if dataset in ['reanalysis-era5-single-levels', 'reanalysis-era5-pressure-levels']:
        if not grid:
            grid = '0.25/0.25'
        assert grid in [
            '0.25/0.25', '0.5/0.5', '1.0/1.0', 
            '1.5/1.5', '2.0/2.0', '2.5/2.5', '3.0/3.0'
        ]
    elif dataset == 'reanalysis-era5-land':
        if not grid:
            grid = '0.1/0.1'
        assert grid in [
            '0.1/0.1', '0.25/0.25', '0.5/0.5', '1.0/1.0', 
            '1.5/1.5', '2.0/2.0', '2.5/2.5', '3.0/3.0'
        ]

    # Check area:
    if area:
        lat_min = area['lat'][0] >= -90
        lat_max = area['lat'][1] <= 90
        lon_min = area['lon'][0] >= -180
        lon_max = area['lon'][1] <= 180
        assert lat_min & lat_max & lon_min & lon_max

    # Request data
    result = c.service(
        'tool.toolbox.orchestrator.workflow',
        params = {
            'realm': 'user-apps',
            'project': 'app-c3s-daily-era5-statistics',
            'version': 'master',
            'kwargs': {
                'dataset': dataset,
                'product_type': product_type,
                'variable': variable,
                'statistic': statistic,
                'year': year,
                'month': month,
                'time_zone': time_zone,
                'frequency': frequency,
                'grid': grid,
                'area': area
            },
            'workflow_name': 'application'
        }
    )

    # Set name of output file
    file_name = file_name if file_name else dataset + '_' + variable + '_' + statistic + '_' + year + '_' + month + '.nc'

    # Write data to output file
    location = result[0]['location']
    res = requests.get(location, stream = True)
    print('Writing data to ' + file_name)
    with open(file_name, 'wb') as fh:
        for r in res.iter_content(chunk_size = 1024):
            fh.write(r)
    fh.close()

    # Move output file to output folder if specified
    if folder:
        cwd_file = os.path.join(os.getcwd(), file_name)
        out_file = os.path.join(os.path.abspath(os.path.expanduser(os.path.expandvars(folder))), file_name)
        shutil.move(cwd_file, out_file)
