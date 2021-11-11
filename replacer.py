import re
import shutil
with open("flattener.bc.js", "r+") as f:
    data = f.read()
    data = re.sub(r"runtime\.ml_z_mul_overflows\(_gS_,_gR_\)", "false", data)
    data = re.sub(r'\(function\((.*?)\){"use strict";var', r'(function(\1){"use strict";\1.XMLHttpRequest=require("xmlhttprequest").XMLHttpRequest;\1.ml_z_mul_overflows=function(){return false;};var', data)
    data = re.sub(r'if\(typeof g\.XMLHttpRequest !== "undefined"\)', 'g.XMLHttpRequest = require("xmlhttprequest").XMLHttpRequest;\n      if(typeof g.XMLHttpRequest !== "undefined")', data)
    f.seek(0)
    f.write("#!/usr/bin/env node\n")
    f.write(data)
    f.truncate()

shutil.copy("flattener.bc.js", "jooskos_flattener.js")