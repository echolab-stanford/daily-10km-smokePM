####################################################################
# Download ERA5 Wind (u and v components) over Continental USA
# Modified by Jessica from script written by Jeff
# Last edited August 2021
# 
# Comment out variable and year as desired. Run this script in your 
# Copernicus Climate Data Store Toolbox. Takes ~1 hour. When done 
# running, click the link to download.
####################################################################
import cdstoolbox as ct

@ct.application(title='Download data: wind_u 2016-2020')
@ct.output.download()
def download_application():
    data = ct.catalogue.retrieve(
        'reanalysis-era5-land',
        {
            'variable': [
                '10m_u_component_of_wind'
                #'10m_v_component_of_wind'
            ],
            'year': [
                #'2006', '2007', '2008','2009', '2010'
                #'2011','2012','2013','2014','2015'
                '2016','2017','2018','2019','2020'
            ],
            'month': [
                '01', '02', '03',
                '04', '05', '06',
                '07', '08', '09',
                '10', '11', '12',
            ],
            'day': [
                '01', '02', '03',
                '04', '05', '06',
                '07', '08', '09',
                '10', '11', '12',
                '13', '14', '15',
                '16', '17', '18',
                '19', '20', '21',
                '22', '23', '24',
                '25', '26', '27',
                '28', '29', '30',
                '31',
            ],
            'time': [
                '00:00', #'01:00', '02:00',
                '03:00', #'04:00', '05:00',
                '06:00', #'07:00', '08:00',
                '09:00', #'10:00', '11:00',
                '12:00', #'13:00', '14:00',
                '15:00', #'16:00', '17:00',
                '18:00', #'19:00', '20:00',
                '21:00', #'22:00', '23:00',
            ],
            'area': [
                49.92, -125.5, 24.43,-66.62,
            ],
        }
    )
    
    data_daily = ct.climate.daily_mean(data)

    return data_daily