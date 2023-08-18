import arcpy
def ScriptTool(param0, param1):
    # Script execution code goes here
    arcpy.management.JoinField(param0, "GlobalID", param1, "GlobalID",["tref","tprem_id"])
    arcpy.management.CalculateField(param0, "ref", "!tref! if !tref! else !ref!", "PYTHON3")
    arcpy.management.CalculateField(param0, "prem_id", "!tprem_id! if !tprem_id! else !prem_id!", "PYTHON3")
    arcpy.management.DeleteField(param0, ["tref", "tprem_id"])
    return
# This is used to execute code if the file was run but not imported
if __name__ == '__main__':
    # Tool parameter accessed with GetParameter or GetParameterAsText
    param0 = arcpy.GetParameterAsText(0)
    param1 = arcpy.GetParameterAsText(1)
    
    ScriptTool(param0, param1)
    
    # Update derived parameter values using arcpy.SetParameter() or arcpy.SetParameterAsText()
