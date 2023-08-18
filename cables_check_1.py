import arcpy
from subprocess import run

def script_tool(param0, param1, param2, param3):
    param0=arcpy.Describe(param0).catalogPath
    param1=arcpy.Describe(param1).catalogPath
    param2=arcpy.Describe(param2).catalogPath
    param3=arcpy.Describe(param3).catalogPath


    result=run(["Rscript", "--vanilla", "cables_check_1.R", param0, param1, param2, param3], universal_newlines=True,capture_output=True)
#    result=run(["Rscript", "--vanilla", "test.R"])

    with  open("log_cables.txt", "w") as f:
        print(result.stdout, file=f)
        print(result.stderr, file=f)
    return

if __name__ == "__main__":
    param0=arcpy.GetParameterAsText(0)
    param1=arcpy.GetParameterAsText(1)
    param2=arcpy.GetParameterAsText(2)
    param3=arcpy.GetParameterAsText(3)
    script_tool(param0, param1, param2, param3)
