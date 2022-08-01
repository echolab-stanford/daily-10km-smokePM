import cdsapi
from datetime import datetime
import os
from tenacity import retry, stop_after_attempt, wait_random_exponential

# @retry(stop = stop_after_attempt(10), wait = wait_random_exponential(multiplier = 1, min = 5, max = 60))
def retrieve_era5_hourly(
    dataset,
    variable,
    years,
    months,
    days,
    hours,
    product_type = 'reanalysis',
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

    # Convert years, months, days, and hours to string
    years = [int(year) for year in years] if type(years) == list else [int(years)]
    years = [str(year) for year in years]
    months = [int(month) for month in months] if type(months) == list else [int(months)]
    months = ['0' + str(month) if month < 10 else str(month) for month in months]
    days = [int(day) for day in days] if type(days) == list else [int(days)]
    days = ['0' + str(day) if day < 10 else str(day) for day in days]
    hours = [int(hour) for hour in hours] if type(hours) == list else [int(hours)]
    hours = ['0' + str(hour) if hour < 10 else str(hour) for hour in hours]
    hours = [hour + ':00' for hour in hours]

    # Check area:
    lat_min = area['lat'][0]
    lat_max = area['lat'][1]
    lon_min = area['lon'][0]
    lon_max = area['lon'][1]
    assert (lat_min  >= -90) & (lat_max <= 90) & (lon_min >= -180) & (lon_max <= 180)
    area = [lat_max, lon_min, lat_min, lon_max]

    # Set name of output file
    file_years = min(years) + '-' + max(years) if len(years) > 1 else years[0]
    file_months = min(months) + '-' + max(months) if len(months) > 1 else months[0]
    file_days = min(days) + '-' + max(days) if len(days) > 1 else days[0]
    file_hours = min(hours)[0:2] + '-' + max(hours)[0:2] if len(hours) > 1 else hours[0][0:2]

    file_name = file_name if file_name else dataset + '_' + variable + '_' \
    + file_years + '_' + file_months + '_' + file_days + '_' + file_hours + '.nc'

    file_name = os.path.join(os.path.abspath(os.path.expanduser(os.path.expandvars(folder))), file_name)

    # Request data
    c.retrieve(
        dataset,
        {
            'product_type': product_type,
            'format': 'netcdf',
            'variable': variable,
            'year': years,
            'month': months,
            'day': days,
            'time': hours,
            'area': area,
        },
        file_name)
