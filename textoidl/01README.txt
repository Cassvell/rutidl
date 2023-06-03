              Mexican Magnetic Index K (K_mex)
  <<processed_data DIR>>
                                             
  The processed_data directory contains the processed geomagnetic field data.
  There are three types of files:
        GMS_YYYYMMDD.data.final[/early]
        GMS_YYYYMMDD.differences.final[/early]
        GMS_YYYYMMDD.dst.final[/early]
  with GMS the geomagnetic station code, YYYYMMDD the date. The first type of 
  files contain the original geomagnetic data without the significant cyclical 
  effects. The second type contains the larger differences in spams of 3 hours
  for all the geomagnetic components and the associated uncertainness. The third
  type of file has the hourly averaged variations of H component of geomangetic
  field. Finally, the extension early[final] appoints that the analysis is done
  with data between 28 days before[and 28 days after] the given date.
  
  The data files are ordered inside subdirectories, whose names are the
  corresponding GMS name, where the data come from.

  Kmex is a product by the Mexican Space Weather Service (SCiESMEX) at the 
  National Space Weather Laboratory (LANCE) in collaboration with the Geomagnetic
  Service at Geophysics Institute-UNAM.
                   
  Data gaps were filled with dummy numbers for the missing hours or entire
  days to make all files of equal length.  The character '9' is used to
  fill all fields for missing data according to their format, e.g.
  ' 9999.9' for a field with the FORTRAN format F7.1. Note that format F7.1
  below really means (1X,F6.1),etc.

--------------------------------------------------------------------------------
 
     
                    GMS_YYYYMMDD.data.* FORMAT DESCRIPTION     
                                                   
         
WORD  FORMAT  Fill Value         MEANING                  UNITS/COMMENTS
                               
 1      I4                Row number
 2      I2                Hour                              0, 1,...,23
 3      I2                Minute                            0,1,...,59
 4      I2                Data used for the analysis
 5      I2                Read data for the analysis
 6     F7.2  9999.00      Declination angle (eastward)      arc minutes
 7     F9.2  999999.00    Horizontal component (H)          nT
 8     F9.2  999999.00    Vertical component (Z downward+)  nT
 9     F9.2  999999.00    Magntitude                        nT
10     F9.2  999999.00    North component (N=HxZ)           nT
11     F7.2  9999.00      Median value of D                 arc minutes
12     F9.2  999999.00    Median value of H                 nT
13     F9.2  999999.00    Median value of Z                 nT
14     F9.2  999999.00    Median value of Magntitude        nT
15     F9.2  999999.00    Median value of N                 nT
16     F7.2  9999.00      Standard deviation of D           arc minutes
17     F9.2  999999.00    Standard deviation of H           nT
18     F9.2  999999.00    Standard deviation of Z           nT
19     F9.2  999999.00    Standard deviation of Magntitude  nT
20     F9.2  999999.00    Standard deviation of N           nT
21     F7.2  9999.00      D - median                        arc minutes
22     F9.2  999999.00    H - median                        nT
23     F9.2  999999.00    Z - median                        nT
24     F9.2  999999.00    Magntitude - median               nT
25     F9.2  999999.00    N - median                        nT
                 


The data may be read with the format statement
(I4,X,I2,X,I2,X,I2,X,I2,2X,F7,4(X,F9),2X,F7,4(X,F9),2X,F7,4(X,F9),2X,F7,4(X,F9))

                                C O M M E N T S 

   (*)   Median and standard deviation are calculated with previous data for the
         case of "early" files and the "final" files are calculated with
         previous and subsequent data.

--------------------------------------------------------------------------------
 
     
                GMS_YYYYMMDD.differences.* FORMAT DESCRIPTION     
                                                   
         
WORD  FORMAT  Fill Value         MEANING                  UNITS/COMMENTS
                               
 1      I2                Hour                              0, 3, 6,...,21
 2     F7.2  9999.00      Max value of D                    arc minutes
 3     F7.2  9999.00      Min value of D                    arc minutes
 4     F7.2  9999.00      Standard deviation of D           arc minutes
 5     F7.2  9999.00      |Max[D]-Min[D]|                   arc minutes
 6     F9.2  999999.00    Max value of H                    nT
 7     F9.2  999999.00    Min value of H                    nT
 8     F9.2  999999.00    Standard deviation of H           nT
 9     F9.2  999999.00    |Max[H]-Min[H]|                   nT
10     F9.2  999999.00    Max value of Z                    nT
11     F9.2  999999.00    Min value of Z                    nT
12     F9.2  999999.00    Standard deviation of Z           nT
13     F9.2  999999.00    |Max[Z]-Min[Z]|                   nT
14     F9.2  999999.00    Max value of Magnitude            nT
15     F9.2  999999.00    Min value of Magnitude            nT
16     F9.2  999999.00    Standard deviation of Magnitude   nT
17     F9.2  999999.00    |Max[M]-Min[M]|                   nT
18     F9.2  999999.00    Max value of N                    nT
19     F9.2  999999.00    Min value of N                    nT
20     F9.2  999999.00    Standard deviation of N           nT
21     F9.2  999999.00    |Max[N]-Min[N]|                   nT
                 


The data may be read with the format statement
(I2,2X,4(X,F7),4(X, F9),4(X, F9),4(X, F9),4(X, F9))

                                C O M M E N T S 

   (*)   Median and standard deviation are calculated with previous data for the
         case of "early" files and the "final" files are calculated with
         previous and subsequent data.
         
  (**)   The standard deviation values are 3-hourly averages calculated with
         values from "data" files.

--------------------------------------------------------------------------------
 
     
                GMS_YYYYMMDD.dst.* FORMAT DESCRIPTION     
                                                   
         
WORD  FORMAT  Fill Value         MEANING                  UNITS/COMMENTS
                               
 1      I2                Hour                              01, 02, 03,...,24
 2     F9.2  999999.00    Standard deviation of Delta H     nT
 3     F9.2  999999.00    Hourly mean of Delta H            nT
                 


The data may be read with the format statement
(I2,2(X, F9))

                                C O M M E N T S 

   (*)   Mean and standard deviation are calculated with previous data for the
         case of "early" files and the "final" files are calculated with
         previous and subsequent data.
         
  (**)   The mean and standard deviation values are 1-hourly averages calculated
         with values from "data" files.

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
 REMARKS:
 
 Kmex is a prodcut by the Mexican Space Weather Service (SCiESMEX) at the 
 National Space Weather Laboratory (LANCE) in colaboration with the Geomagnetic
 Service at Geophysic Institute-UNAM.

 Geomagnetic data is provided by the Geomagnetic Service of the Geophysics
 Insitute, National University of Mexico (UNAM). The Geomagnetic Service is
 opperated by Dr. Esteban Hernández, Dr. Gerardo Cifuentes and Dr. Ana Caccavari.
--------------------------------------------------------------------------------

 ACKNOWLEDGMENT:

 Use of these data in publications should be accompanied at minimum by 
 acknowledgements to the Mexican Space Weather Service and the Mexican 
 Geomagnetic Service, as well.
             
--------------------------------------------------------------------------------
 K_mex CONTACT:
 Dr. P. Corona-Romero  E-mail: pcorona@geofisica.unam.mx
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
