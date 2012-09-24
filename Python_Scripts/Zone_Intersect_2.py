# Calculates the area of each DHS region that intersects with the CI and WWF
# conservation zones, in kilometers, and as a percentage of total region area.

import arcpy
from arcpy import env

env.workspace = "M:\Data\Global\DHS\Statcompiler_Processing\Shapefiles"

DHS_Regions = "DHS_Regions.shp"

dissolved_out_list = ["DHS_Regions_WWF_Intersect.shp", \
        "DHS_Regions_CI_Intersect.shp", \
        "DHS_Regions_WDPA_INT_Intersect.shp", \
        "DHS_Regions_WDPA_NAT_Intersect.shp"]

area_fields_list = [["WWFInNm", "WWFInkm", "WWFInPct"], \
        ["CIInNm", "CIInkm", "CIInPct"], \
        ["WDPAIInNm", "WDPAIInkm", "WDPAIInPct"], \
        ["WDPANInNm", "WDPANInkm", "WDPANInPct"]]
shape_area_fieldname = "Areakm"
shape_perimeter_fieldname = "Perimkm"

for dissolved_out, area_fields in zip(dissolved_out_list, area_fields_list):
    print "Processing %s"%dissolved_out
    arcpy.DeleteField_management(DHS_Regions, area_fields)
    arcpy.JoinField_management(DHS_Regions, "FID", dissolved_out,
            "FID_DHS_Re", area_fields[0:2])

    print "\tCalculating percent of area within conservation zone..."
    # First add a shape area field
    arcpy.DeleteField_management(DHS_Regions, shape_area_fieldname)
    arcpy.AddField_management(DHS_Regions, shape_area_fieldname, "DOUBLE")
    expression = "float(!shape.area@squarekilometers!)"
    arcpy.CalculateField_management(DHS_Regions, shape_area_fieldname, expression, "PYTHON")

    # Calculate the perimeter at this point (though it is not needed for this 
    # code, it needs to be in the final shapefile)
    arcpy.DeleteField_management(DHS_Regions, shape_perimeter_fieldname)
    arcpy.AddField_management(DHS_Regions, shape_perimeter_fieldname, "DOUBLE")
    expression = "float(!shape.length@kilometers!)"
    arcpy.CalculateField_management(DHS_Regions, shape_perimeter_fieldname, expression, "PYTHON")

    arcpy.AddField_management(DHS_Regions, area_fields[2], "DOUBLE")
    expression = "([" + area_fields[1] + "]/[" + shape_area_fieldname + "])*100"
    arcpy.CalculateField_management(DHS_Regions, area_fields[2], expression)
