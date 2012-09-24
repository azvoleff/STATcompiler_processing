# Intersects the DHS region data with the CI and WWF conservation zones.

import arcpy
from arcpy import env

env.workspace = "M:\Data\Global\DHS\Statcompiler_Processing\Shapefiles"

in_features_list = ["WWF_Priority_Places.shp", "CI_Hotspots_Areas.shp", 
                    "WDPA_INTpol2010.shp", "WDPA_NATpol2010.shp"]
intersect_out_list = ["DHS_Regions_WWF_Intersect_Temp.shp", \
        "DHS_Regions_CI_Intersect_Temp.shp", \
        "DHS_Regions_WDPA_INT_Intersect_Temp.shp", \
        "DHS_Regions_WDPA_NAT_Intersect_Temp.shp"]
dissolved_out_list = ["DHS_Regions_WWF_Intersect.shp", \
        "DHS_Regions_CI_Intersect.shp", \
        "DHS_Regions_WDPA_INT_Intersect.shp", \
        "DHS_Regions_WDPA_NAT_Intersect.shp"]

fields_list = [["WWFInNm", "WWFInkm"], \
        ["CIInNm", "CIInkm"], \
        ["WDPAIInNm", "WDPAIInkm"], \
        ["WDPANInNm", "WDPANInkm"]]

for in_features, intersect_out, dissolved_out, fields in zip(in_features_list,
        intersect_out_list, dissolved_out_list, fields_list):
    print "Intersect %s"%in_features
    intersect_in_features = ["DHS_Regions.shp", in_features]
    if arcpy.Exists(intersect_out):
        print "\tDelete %s"%intersect_out
        arcpy.Delete_management(intersect_out)
    print "\tNow intersecting %s"%intersect_in_features
    arcpy.Intersect_analysis(intersect_in_features, intersect_out)

    print "Dissolve %s"%intersect_out
    if arcpy.Exists(dissolved_out):
        print "\tDelete %s"%dissolved_out
        arcpy.Delete_management(dissolved_out)
    print "\tNow dissolving %s"%intersect_out
    # The NAME field is the name of the conservation zone
    arcpy.Dissolve_management(intersect_out, dissolved_out,
            ["FID_DHS_Re", "NAME"], "", "MULTI_PART")
    if arcpy.Exists(intersect_out):
        # Now delete the unneeded intersect_out intermediary file
        print "\tDelete %s"%intersect_out
        arcpy.Delete_management(intersect_out)
    arcpy.AddField_management(dissolved_out, fields[0], "TEXT",
        "", "", "100")

    # Add InName field
    arcpy.CalculateField_management(dissolved_out, fields[0],
            "[NAME]")

    # Add Inkm field with area in square kilometers
    arcpy.AddField_management(dissolved_out, fields[1], "DOUBLE")
    expression = "float(!shape.area@squarekilometers!)"
    arcpy.CalculateField_management(dissolved_out, fields[1], expression, "PYTHON")
