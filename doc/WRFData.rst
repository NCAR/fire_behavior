.. _WRF_data:

================================
Running real cases with WRF data
================================

The schematic figure illustrates the CFBM operating in offline mode. To run CFBM in the offline mode, users need to provide WRF atmospheric data in the ```wrf.nc``` format and static inputs as ```geo_em.d01.nc```.

Users can obtain the model from the github repository:

```git clone https://github.com/NCAR/fire_behavior.git```

To compile the code on derecho, run:

```./compile.sh --env-auto```

If the compilation is successful, the model can be run using ```fire_behavior.exe``` located in the ```build``` directory.

An example is provided in ```test/test7/``` directory with the ```namelist.fire```.

.. !image:: https://github.com/NCAR/fire_behavior/blob/develop/doc/CFBM-offline.jpeg
.. !  :width: 400
.. !  :alt: CFBM-WRF data schematic
.. !  :align: center

