import arcpy
from subprocess import run

def script_tool(param0, param1, param2, param3, param4, param5, param6):
    if param0:
        param0=arcpy.Describe(param0).catalogPath
    else:
        param0=''

    param1=arcpy.Describe(param1).catalogPath
    param2=arcpy.Describe(param2).catalogPath
    param3=arcpy.Describe(param3).catalogPath
    param4=arcpy.Describe(param4).catalogPath
    param5=arcpy.Describe(param5).catalogPath
#    param6=arcpy.Describe(param6).catalogPath


    result=run(["Rscript", "--vanilla", "drops_1.R", param0, param1, param2, param3, param4, param5, param6], universal_newlines=True,capture_output=True)
#    result=run(["Rscript", "--vanilla", "test.R"])

    with  open("log_drops.txt", "w") as f:
        print(result.stdout, file=f)
        print(result.stderr, file=f)
    return

if __name__ == "__main__":
    param0=arcpy.GetParameterAsText(0)
    param1=arcpy.GetParameterAsText(1)
    param2=arcpy.GetParameterAsText(2)
    param3=arcpy.GetParameterAsText(3)
    param4=arcpy.GetParameterAsText(4)
    param5=arcpy.GetParameterAsText(5)
    param6=arcpy.GetParameterAsText(6)
    script_tool(param0, param1, param2, param3, param4, param5, param6)
