data_obs_optics.mat

Contain all observed data for optics analysis : Ice camp 2015, Ice camp 2016 , Amundsen 2016 , SUBICE 2014 , Optimism 2016-2017-2018, Saroma 2019

- albedo   : albedo calculated from NEMO parameterization using measured hi,hs,c and temp. 
- c        : nebulosity. It is set to 0 for clear sky , 1 to overcast sky. 
- chla     : Integrated ice Chl-a (mg/m2). 
- doy      : day of year for each station
- f_mp     : melt pond fraction. It is set to 0 before melt ponding onset ; 0.05 at melt 
             ponding onset and increases linearly for each station until 0.25.
- Fsw0     : Incoming solar irradiance (W/m2)
- h_i      : Ice thickness (m)
- h_s      : Snow thickness (m)
- lat      : latitude  (rad)
- lon      : longitude (rad)
- snow_dis : this value is set to 1 for drift ice where snow distribution should not be constant and 
	     it is set to 0 for land fast ice. 
T_obs      : observed transmittance (%)
temp       : Air temperature (°c)
year       : year of each station (juste in case)

