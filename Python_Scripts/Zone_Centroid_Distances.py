# Calculates the distances from the centroid of each DHS region to the closest
# WWF or CI conservation zone.

import arcpy
from arcpy import env

env.workspace = "M:\Data\Global\DHS\Statcompiler_Processing\Shapefiles"

RECALCULATE_CENTROIDS = False

DHS_Regions = "DHS_Regions.shp"
DHS_Regions_Centroids = "DHS_Regions_Centroids.shp"
if arcpy.Exists(DHS_Regions_Centroids) and not RECALCULATE_CENTROIDS:
    print "Centroids file already exists - will not be recreated"
else:
    print "Calculate centroids %s"%DHS_Regions
    if arcpy.Exists(DHS_Regions_Centroids):
        print "\tDelete %s"%DHS_Regions_Centroids
        arcpy.Delete_management(DHS_Regions_Centroids)
    print "\tNow calculating centroids %s"%DHS_Regions
    arcpy.FeatureToPoint_management(DHS_Regions, DHS_Regions_Centroids, "INSIDE")

near_features_list = ["WWF_Priority_Places.shp", "CI_Hotspots_Areas.shp", "WDPA_INTpol2010.shp", "WDPA_NATpol2010.shp"]
near_fields_list = [["WWFNrName", "WWFNrkm"], ["CINrName", "CINrkm"], \
        ["WDPAIName","WDPAIkm"], ["WDPANName","WDPANkm"]]
for near_features, near_fields in zip(near_features_list, near_fields_list):
    print "Processing %s"%near_features
    print "\tDrop old near fields"
    dropFields = ["NEAR_FID", "NEAR_DIST"]
    arcpy.DeleteField_management(DHS_Regions_Centroids, dropFields)
    # After running the near script, the distance to the nearest zone is stored
    # in the "NEAR_DIST" and the FID of the near feature is stored in
    # "NEAR_FID". Need to rename these fields to CI or WWF as appropriate, and
    # also join the CI or WWF feature class to convert the NEAR_FID into
    # CI_Near_Name (or WWF) with the name of the nearest zone.
    print "\tRun near script"
    arcpy.Near_analysis(DHS_Regions_Centroids, near_features)

    arcpy.DeleteField_management(DHS_Regions_Centroids, near_fields)
    arcpy.DeleteField_management(DHS_Regions, near_fields)

    name_field, dist_field = near_fields
    print "\tAdd new fields"
    arcpy.AddField_management(DHS_Regions, name_field, "TEXT",
        "", "", "100")
    arcpy.AddField_management(DHS_Regions, dist_field, "DOUBLE")

    print "\tAdd joins"
    # First join the name field to centroids data
    arcpy.JoinField_management(DHS_Regions_Centroids, "NEAR_FID", near_features,
            "FID", "NAME")
    #  Now join the name field and dist field from centroids data to the main 
    #  (polygon) dataset.
    arcpy.JoinField_management(DHS_Regions, "FID", DHS_Regions_Centroids,
            "FID", ["NAME", "NEAR_DIST"])
    print "\tCalculate fields"
    arcpy.CalculateField_management(DHS_Regions, name_field,
            "[NAME]")
    arcpy.CalculateField_management(DHS_Regions, dist_field,
            "[NEAR_DIST]/1000") # /1000 to convert from meters to km
    print "\tRemove joins"
    # Now remove the "NAME" field that was joined to the centroids file, and 
    # the NAME and NEAR_DIST fields joined to the polygon file, and the 
    # unneeded near fields, to prepare for the next run through the loop.
    dropFields = ["NAME", "NEAR_DIST"]
    arcpy.DeleteField_management(DHS_Regions, dropFields)
    dropFields = ["NAME", "NEAR_FID", "NEAR_DIST"]
    arcpy.DeleteField_management(DHS_Regions_Centroids, dropFields)

    print "\n"
